{-# LANGUAGE TemplateHaskell, FlexibleContexts, TupleSections, StandaloneDeriving, DeriveDataTypeable #-}

-- | Generating synthesis constraints from specifications, qualifiers, and program templates
module Synquid.Explorer where

import Synquid.Logic
import Synquid.Type
import Synquid.Program
import Synquid.Error
import Synquid.SolverMonad
import Synquid.TypeConstraintSolver hiding (freshId, freshVar)
import qualified Synquid.TypeConstraintSolver as TCSolver (freshId, freshVar)
import Synquid.Util
import Synquid.Pretty
import Synquid.Tokens
import Synquid.Succinct
import Synquid.Graph hiding (instantiate)
import Database.GraphWeightsProvider
import Database.Util
import Synquid.GraphConstraintSolver
import qualified PetriNet.PNSolver as PNSolver

import Data.Maybe
import Data.List
import Data.Foldable
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Char
import qualified Data.Foldable as Foldable
import qualified Data.PQueue.Prio.Max as PQ
import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.PQueue.Prio.Min as MinPQ
import Data.PQueue.Prio.Min (MinPQueue)
-- import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Heap (MinHeap)
import Data.Data (Data)
import qualified Data.Heap as Heap
-- import Control.Monad.List
import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative hiding (empty)
import Control.Lens hiding (index, indices)
import Debug.Trace
import Z3.Monad (evalZ3WithEnv, stdOpts, opt, (+?))
import qualified Z3.Monad as Z3
import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Aeson as Aeson

{- Interface -}

-- | Choices for the type of terminating fixpoint operator
data FixpointStrategy =
    DisableFixpoint   -- ^ Do not use fixpoint
  | FirstArgument     -- ^ Fixpoint decreases the first well-founded argument
  | AllArguments      -- ^ Fixpoint decreases the lexicographical tuple of all well-founded argument in declaration order
  | Nonterminating    -- ^ Fixpoint without termination check

-- | Choices for the order of e-term enumeration
data PickSymbolStrategy = PickDepthFirst | PickInterleave

-- | Choices for the type of path search
data PathStrategy =
    DisablePath
  | MaxSAT -- ^ Use SMT solver to find a path
  | Dijkstra -- ^ Use dijkstra algorithm
  | BiDijkstra -- ^ Use bidirectional dijkstra algorithm
  | PetriNet -- ^ Use PetriNet and SyPet
  deriving (Eq, Show, Data)

-- | Parameters of program exploration
data ExplorerParams = ExplorerParams {
  _eGuessDepth :: Int,                    -- ^ Maximum depth of application trees
  _scrutineeDepth :: Int,                 -- ^ Maximum depth of application trees inside match scrutinees
  _matchDepth :: Int,                     -- ^ Maximum nesting level of matches
  _auxDepth :: Int,                       -- ^ Maximum nesting level of auxiliary functions (lambdas used as arguments)
  _fixStrategy :: FixpointStrategy,       -- ^ How to generate terminating fixpoints
  _polyRecursion :: Bool,                 -- ^ Enable polymorphic recursion?
  _predPolyRecursion :: Bool,             -- ^ Enable recursion polymorphic in abstract predicates?
  _abduceScrutinees :: Bool,              -- ^ Should we match eagerly on all unfolded variables?
  _unfoldLocals :: Bool,                  -- ^ Unfold binders introduced by matching (to use them in match abduction)?
  _partialSolution :: Bool,               -- ^ Should implementations that only cover part of the input space be accepted?
  _incrementalChecking :: Bool,           -- ^ Solve subtyping constraints during the bottom-up phase
  _consistencyChecking :: Bool,           -- ^ Check consistency of function's type with the goal before exploring arguments?
  _splitMeasures :: Bool,                 -- ^ Split subtyping constraints between datatypes into constraints over each measure
  _context :: RProgram -> RProgram,       -- ^ Context in which subterm is currently being generated (used only for logging and symmetry reduction)
  _useMemoization :: Bool,                -- ^ Should enumerated terms be memoized?
  _symmetryReduction :: Bool,             -- ^ Should partial applications be memoized to check for redundancy?
  _sourcePos :: SourcePos,                -- ^ Source position of the current goal
  _explorerLogLevel :: Int,               -- ^ How verbose logging is
  _useSuccinct :: Bool,
  _buildGraph :: Bool,
  _solutionCnt :: Int,
  _pathSearch :: PathStrategy
} 

makeLenses ''ExplorerParams

type Requirements = Map Id [RType]

data ProgramRank = ProgramRank {
  holes :: Int,
  weights :: Double
} deriving(Ord, Eq, Show)

data ProgramItem = ProgramItem {
  iProgram :: SProgram,
  iExpoState :: ExplorerState,
  iConstraints :: [Constraint],
  iNode :: SuccinctType
} deriving(Ord, Eq)

type ProgramQueue = MaxPQueue ProgramRank ProgramItem

-- | State of program exploration
data ExplorerState = ExplorerState {
  _typingState :: TypingState,                     -- ^ Type-checking state
  _auxGoals :: [Goal],                             -- ^ Subterms to be synthesized independently
  _solvedAuxGoals :: Map Id RProgram,              -- ^ Synthesized auxiliary goals, to be inserted into the main program
  _lambdaLets :: Map Id (Environment, UProgram),   -- ^ Local bindings to be checked upon use (in type checking mode)
  _requiredTypes :: Requirements,                  -- ^ All types that a variable is required to comply to (in repair mode)
  _symbolUseCount :: Map Id Int,                   -- ^ Number of times each symbol has been used in the program so far
  -- temporary storage of the queue state
  _termQueueState :: ProgramQueue                  -- ^ Candidate term queue, only used when we use succinct type graph for generateE
} deriving (Eq, Ord)

makeLenses ''ExplorerState

-- | Key in the memoization store
data MemoKey = MemoKey {
  keyTypeArity :: Int,
  keyLastShape :: SType,
  keyState :: ExplorerState,
  keyDepth :: Int
} deriving (Eq, Ord)
instance Pretty MemoKey where
  -- pretty (MemoKey arity t d st) = pretty env <+> text "|-" <+> hsep (replicate arity (text "? ->")) <+> pretty t <+> text "AT" <+> pretty d
  pretty (MemoKey arity t st d) = hsep (replicate arity (text "? ->")) <+> pretty t <+> text "AT" <+> pretty d <+> parens (pretty (st ^. typingState . candidates))

-- | Memoization store
type Memo = Map MemoKey [(RProgram, ExplorerState)]

data PartialKey = PartialKey {
    pKeyContext :: RProgram
} deriving (Eq, Ord)

type PartialMemo = Map PartialKey (Map RProgram (Int, Environment))
-- | Persistent state accross explorations
data PersistentState = PersistentState {
  _termMemo :: Memo,
  _partialFailures :: PartialMemo,
  _typeErrors :: [ErrorMessage]
}

makeLenses ''PersistentState

-- | Computations that explore program space, parametrized by the the horn solver @s@
type Explorer s = StateT ExplorerState (
                    ReaderT (ExplorerParams, TypingParams, Reconstructor s) (
                    LogicT (StateT PersistentState s)))
             
-- | This type encapsulates the 'reconstructTopLevel' function of the type checker,
-- which the explorer calls for auxiliary goals             
data Reconstructor s = Reconstructor (Goal -> Explorer s RProgram) (Environment -> RType -> UProgram -> Explorer s RProgram)

-- | empty typing state, just for initialization
emptyTypingState = TypingState {
  _typingConstraints = [],
  _typeAssignment = Map.empty,
  _predAssignment = Map.empty,
  _qualifierMap = Map.empty,
  _candidates = [],
  _initEnv = emptyEnv,
  _idCount = Map.empty,
  _isFinal = False,
  _simpleConstraints = [],
  _hornClauses = [],
  _consistencyChecks = [],
  _errorContext = (noPos, empty)
}

-- | 'runExplorer' @eParams tParams initTS go@ : execute exploration @go@ with explorer parameters @eParams@, typing parameters @tParams@ in typing state @initTS@
runExplorer :: (MonadHorn s, MonadIO s) => ExplorerParams -> TypingParams -> Reconstructor s -> TypingState -> Explorer s a -> s (Either ErrorMessage [a])
runExplorer eParams tParams topLevel initTS go = do
  (ress, (PersistentState _ _ errs)) <- runStateT (observeManyT (eParams ^. solutionCnt) $ runReaderT (evalStateT go initExplorerState) (eParams, tParams, topLevel)) (PersistentState Map.empty Map.empty [])
  -- (ress, (PersistentState _ _ errs)) <- runStateT (observeManyT 1 $ runReaderT (evalStateT go initExplorerState) (eParams, tParams, topLevel)) (PersistentState Map.empty Map.empty [])
  case ress of
    [] -> return $ Left $ head errs
    res:_ -> return $ Right ress
  where
    initExplorerState = ExplorerState initTS [] Map.empty Map.empty Map.empty Map.empty PQ.empty

-- | 'generateI' @env t@ : explore all terms that have refined type @t@ in environment @env@
-- (top-down phase of bidirectional typechecking)
generateI :: (MonadHorn s, MonadIO s) => Environment -> RType -> Bool -> Explorer s RProgram
generateI env t@(FunctionT x tArg tRes) isElseBranch = do
  let ctx = \p -> Program (PFun x p) t
  useSucc <- asks . view $ _1 . buildGraph
  env' <- if useSucc then addSuccinctEdge x (Monotype tArg) env else return env
  pBody <- inContext ctx $ generateI (unfoldAllVariables $ addVariable x tArg $ addArgument x tArg $ env') tRes False
  return $ ctx pBody
generateI env t@(ScalarT _ _) isElseBranch = do -- splitGoal env t
  -- useSuccinct <- asks . view $ _1 . useSuccinct
  -- let env' = if useSuccinct 
  --             then let goalTy = outOfSuccinctAll $ lastSuccinctType (HashMap.lookupDefault SuccinctAny "__goal__" (env ^. succinctSymbols))
  --                      starters = Set.toList $ Set.filter (\typ -> isSuccinctInhabited typ || isSuccinctFunction typ || typ == (SuccinctScalar BoolT) || hasSuccinctAny typ) (allSuccinctNodes env)
  --                      reachableSet = getReachableNodes env starters
  --                      graphEnv = env { _succinctGraph = pruneGraphByReachability (env ^. succinctGraph) reachableSet }
  --                      subgraphNodes = if goalTy == SuccinctAny then allSuccinctNodes graphEnv else reachableGraphFromNode graphEnv goalTy
  --                      in graphEnv { _graphFromGoal = pruneGraphByReachability (graphEnv ^. succinctGraph) subgraphNodes }
  --             else env
  let env' = env
  pathEnabled <- asks . view $ _1 . pathSearch
  case pathEnabled of
    Dijkstra    -> splitGoal env' t
    BiDijkstra  -> splitGoal env' t
      -- liftIO $ writeFile "test.log" $ showGraphViz True env
    MaxSAT      -> do
      start <- liftIO $ getCurrentTime
      getKSolution env'
      end <- liftIO $ getCurrentTime
      error $ show $ diffUTCTime end start
      -- splitGoal env t
    PetriNet    -> do
      m <- evalStateT (PNSolver.instantiate (PNSolver.initSigs env)) (PNSolver.InstantiateState Map.empty)
      liftIO $ writeFile ("data/base.db") $ LB8.unpack $ Aeson.encode $ map (uncurry PNSolver.encodeFunction) $ Map.toList m
      let args = map (show . PNSolver.abstract . shape . toMonotype) $ Map.elems (env' ^. arguments)
      liftIO $ PNSolver.findPath args (show $ PNSolver.abstract $ shape t)
      generateMaybeMatchIf env' t isElseBranch
    DisablePath -> do
      maEnabled <- asks . view $ _1 . abduceScrutinees -- Is match abduction enabled?
      d <- asks . view $ _1 . matchDepth
      maPossible <- runInSolver $ hasPotentialScrutinees env' -- Are there any potential scrutinees in scope?
      if maEnabled && d > 0 && maPossible then generateMaybeMatchIf env' t isElseBranch else generateMaybeIf env' t isElseBranch           

-- | Generate a possibly conditional term type @t@, depending on whether a condition is abduced
generateMaybeIf :: (MonadHorn s, MonadIO s) => Environment -> RType -> Bool -> Explorer s RProgram
generateMaybeIf env t isElseBranch = -- (generateThen >>= (uncurry3 $ generateElse env t)) `mplus` generateMatch env t
  ifte generateThen
    (uncurry3 $ generateElse env t True)
    (generateMatch env t)
  where
    -- | Guess an E-term and abduce a condition for it
    generateThen = do
      cUnknown <- Unknown Map.empty <$> freshId "C"
      addConstraint $ WellFormedCond env cUnknown
      pThen <- cut $ generateE (addAssumption cUnknown env) t True isElseBranch False -- Do not backtrack: if we managed to find a solution for a nonempty subset of inputs, we go with it      
      cond <- conjunction <$> currentValuation cUnknown
      return (cond, unknownName cUnknown, pThen)

-- | Proceed after solution @pThen@ has been found under assumption @cond@
generateElse env t notMatched cond condUnknown pThen = if cond == ftrue
  then return pThen -- @pThen@ is valid under no assumptions: return it
  else do -- @pThen@ is valid under a nontrivial assumption, proceed to look for the solution for the rest of the inputs
    pCond <- inContext (\p -> Program (PIf p uHole uHole) t) $ generateCondition env cond
    
    cUnknown <- Unknown Map.empty <$> freshId "C"
    runInSolver $ addFixedUnknown (unknownName cUnknown) (Set.singleton $ fnot cond) -- Create a fixed-valuation unknown to assume @!cond@
    pElse <- optionalInPartial t $ inContext (\p -> Program (PIf pCond pThen p) t) $ generateI (addAssumption cUnknown env) t notMatched
    ifM (tryEliminateBranching pElse (runInSolver $ setUnknownRecheck (unknownName cUnknown) Set.empty (Set.singleton condUnknown)))
      (return pElse)
      (return $ Program (PIf pCond pThen pElse) t)

tryEliminateBranching branch recheck = 
  if isHole branch
      then return False
      -- else (recheck >>= (const (return True))) `mplus` (return False)
      else ifte -- If synthesis of the branch succeeded, try to remove the branching construct
            recheck -- Re-check Horn constraints after retracting the branch guard
            (const $ return True) -- constraints still hold: @branch@ is a valid solution overall
            (return False) -- constraints don't hold: the guard is essential
            
generateCondition env fml = do
  conjuncts <- mapM genConjunct allConjuncts
  return $ fmap (flip addRefinement $ valBool |=| fml) (foldl1 conjoin conjuncts)
  where
    allConjuncts = Set.toList $ conjunctsOf fml
    genConjunct c = if isExecutable c
                              then return $ fmlToProgram c
                              else cut (generateE env (ScalarT BoolT $ valBool |=| c) False False False)
    andSymb = Program (PSymbol $ binOpTokens Map.! And) (toMonotype $ binOpType And)
    conjoin p1 p2 = Program (PApp (Program (PApp andSymb p1) boolAll) p2) boolAll
                
-- | If partial solutions are accepted, try @gen@, and if it fails, just leave a hole of type @t@; otherwise @gen@
optionalInPartial :: (MonadHorn s, MonadIO s) => RType -> Explorer s RProgram -> Explorer s RProgram
optionalInPartial t gen = ifM (asks . view $ _1 . partialSolution) (ifte gen return (return $ Program PHole t)) gen

-- | Generate a match term of type @t@
generateMatch env t = do
  d <- asks . view $ _1 . matchDepth
  if d == 0
    then mzero
    else do
      (Program p tScr) <- local (over _1 (\params -> set eGuessDepth (view scrutineeDepth params) params))
                      $ inContext (\p -> Program (PMatch p []) t)
                      $ generateE env anyDatatype False False True-- Generate a scrutinee of an arbitrary type
      let (env', tScr') = embedContext env tScr
      let pScrutinee = Program p tScr'

      case tScr of
        (ScalarT (DatatypeT scrDT _ _) _) -> do -- Type of the scrutinee is a datatype
          let ctors = ((env ^. datatypes) Map.! scrDT) ^. constructors

          let scrutineeSymbols = symbolList pScrutinee
          let isGoodScrutinee = not (null ctors) &&                                               -- Datatype is not abstract
                                (not $ pScrutinee `elem` (env ^. usedScrutinees)) &&              -- Hasn't been scrutinized yet
                                (not $ head scrutineeSymbols `elem` ctors) &&                     -- Is not a value
                                (any (not . flip Set.member (env ^. constants)) scrutineeSymbols) -- Has variables (not just constants)
          guard isGoodScrutinee

          (env'', x) <- toVar (addScrutinee pScrutinee env') pScrutinee
          (pCase, cond, condUnknown) <- cut $ generateFirstCase env'' x pScrutinee t (head ctors)                  -- First case generated separately in an attempt to abduce a condition for the whole match
          pCases <- map fst <$> mapM (cut . generateCase (addAssumption cond env'') x pScrutinee t) (tail ctors)  -- Generate a case for each of the remaining constructors under the assumption
          let pThen = Program (PMatch pScrutinee (pCase : pCases)) t
          generateElse env t False cond condUnknown pThen                                                               -- Generate the else branch

        _ -> mzero -- Type of the scrutinee is not a datatype: it cannot be used in a match

generateFirstCase env scrVar pScrutinee t consName = do
  case Map.lookup consName (allSymbols env) of
    Nothing -> error $ show $ text "Datatype constructor" <+> text consName <+> text "not found in the environment" <+> pretty env
    Just consSch -> do
      consT <- instantiate env consSch True []
      runInSolver $ matchConsType (lastType consT) (typeOf pScrutinee)
      consT' <- runInSolver $ currentAssignment consT
      binders <- replicateM (arity consT') (freshVar env "x")
      (syms, ass) <- caseSymbols env scrVar binders consT'
      let env' = foldr (uncurry addVariable) (addAssumption ass env) syms
      useSucc <- asks . view $ _1 . buildGraph
      let scrutineeSyms = symbolsOf pScrutinee
      caseEnv <- if useSucc then foldM (\e (name, ty) -> addSuccinctEdge name (Monotype ty) e) env' syms else return env'

      -- ifte (do -- Try to find a vacuousness condition:
      --         deadUnknown <- Unknown Map.empty <$> freshId "C"
      --         addConstraint $ WellFormedCond env deadUnknown
      --         err <- inContext (\p -> Program (PMatch pScrutinee [Case consName binders p]) t) $ generateError (addAssumption deadUnknown caseEnv)
      --         deadValuation <- conjunction <$> currentValuation deadUnknown
      --         ifte (generateError (addAssumption deadValuation env)) (const mzero) (return ()) -- The error must be possible only in this case
      --         return (err, deadValuation, unknownName deadUnknown)) 
      --       (\(err, deadCond, deadUnknown) -> return $ (Case consName binders err, deadCond, deadUnknown))
      --       (do
      pCaseExpr <- local (over (_1 . matchDepth) (-1 +)) 
                    $ inContext (\p -> Program (PMatch pScrutinee [Case consName binders p]) t)
                    $ generateI caseEnv t False
      return $ (Case consName binders pCaseExpr, ftrue, dontCare)
              -- )

-- | Generate the @consName@ case of a match term with scrutinee variable @scrName@ and scrutinee type @scrType@
generateCase :: (MonadHorn s, MonadIO s) => Environment -> Formula -> RProgram -> RType -> Id -> Explorer s (Case RType, Explorer s ())
generateCase env scrVar pScrutinee t consName = do
  case Map.lookup consName (allSymbols env) of
    Nothing -> error $ show $ text "Datatype constructor" <+> text consName <+> text "not found in the environment" <+> pretty env
    Just consSch -> do
      consT <- instantiate env consSch True []
      runInSolver $ matchConsType (lastType consT) (typeOf pScrutinee)
      consT' <- runInSolver $ currentAssignment consT
      binders <- replicateM (arity consT') (freshVar env "x")
      (syms, ass) <- caseSymbols env scrVar binders consT'      
      unfoldSyms <- asks . view $ _1 . unfoldLocals
      
      cUnknown <- Unknown Map.empty <$> freshId "M"
      runInSolver $ addFixedUnknown (unknownName cUnknown) (Set.singleton ass) -- Create a fixed-valuation unknown to assume @ass@      
      
      let env' = (if unfoldSyms then unfoldAllVariables else id) $ foldr (uncurry addVariable) (addAssumption cUnknown env) syms
      useSucc <- asks . view $ _1 . buildGraph
      let scrutineeSyms = symbolsOf pScrutinee
      caseEnv <- if useSucc then foldM (\e (name, ty) -> addSuccinctEdge name (Monotype ty) e) env' syms else return env'
      pCaseExpr <- optionalInPartial t $ local (over (_1 . matchDepth) (-1 +))
                                       $ inContext (\p -> Program (PMatch pScrutinee [Case consName binders p]) t)
                                       $ generateI caseEnv t False
                                       -- $ generateError caseEnv `mplus` generateI caseEnv t False
            
      let recheck = if disjoint (symbolsOf pCaseExpr) (Set.fromList binders)
                      then runInSolver $ setUnknownRecheck (unknownName cUnknown) Set.empty Set.empty -- ToDo: provide duals here
                      else mzero
                                       
      return (Case consName binders pCaseExpr, recheck)

-- | 'caseSymbols' @scrutinee binders consT@: a pair that contains (1) a list of bindings of @binders@ to argument types of @consT@
-- and (2) a formula that is the return type of @consT@ applied to @scrutinee@
caseSymbols env x [] (ScalarT _ fml) = let subst = substitute (Map.singleton valueVarName x) in
  return ([], subst fml)
caseSymbols env x (name : names) (FunctionT y tArg tRes) = do
  (syms, ass) <- caseSymbols env x names (renameVar (isBound env) y name tArg tRes)
  return ((name, tArg) : syms, ass)  

-- | Generate a possibly conditional possibly match term, depending on which conditions are abduced
generateMaybeMatchIf :: (MonadHorn s, MonadIO s) => Environment -> RType -> Bool -> Explorer s RProgram
generateMaybeMatchIf env t isElseBranch = (generateOneBranch >>= generateOtherBranches) `mplus` (generateMatch env t) -- might need to backtrack a successful match due to match depth limitation
  where
    -- | Guess an E-term and abduce a condition and a match-condition for it
    generateOneBranch = do
      matchUnknown <- Unknown Map.empty <$> freshId "M"
      addConstraint $ WellFormedMatchCond env matchUnknown
      condUnknown <- Unknown Map.empty <$> freshId "C"
      addConstraint $ WellFormedCond env condUnknown

      cut $ do
        p0 <- generateEOrError (addAssumption matchUnknown . addAssumption condUnknown $ env) t
        matchValuation <- Set.toList <$> currentValuation matchUnknown
        let matchVars = Set.toList $ Set.unions (map varsOf matchValuation)
        condValuation <- currentValuation condUnknown
        let badError = isError p0 && length matchVars /= 1 -- null matchValuation && (not $ Set.null condValuation) -- Have we abduced a nontrivial vacuousness condition that is not a match branch?
        writeLog 3 $ text "Match valuation" <+> pretty matchValuation <+> if badError then text ": discarding error" else empty
        guard $ not badError -- Such vacuousness conditions are not productive (do not add to the environment assumptions and can be discovered over and over infinitely)        
        let matchConds = map (conjunction . Set.fromList . (\var -> filter (Set.member var . varsOf) matchValuation)) matchVars -- group by vars
        d <- asks . view $ _1 . matchDepth -- Backtrack if too many matches, maybe we can find a solution with fewer
        guard $ length matchConds <= d
        return (matchConds, conjunction condValuation, unknownName condUnknown, p0)
        
    -- generateEOrError env typ = generateError env `mplus` generateE env typ True isElseBranch False
    generateEOrError env typ = generateE env typ True isElseBranch False

    -- | Proceed after solution @p0@ has been found under assumption @cond@ and match-assumption @matchCond@
    generateOtherBranches (matchConds, cond, condUnknown, p0) = do
      pThen <- cut $ generateMatchesFor (addAssumption cond env) matchConds p0 t
      generateElse env t False cond condUnknown pThen

    generateMatchesFor env [] pBaseCase t = return pBaseCase
    generateMatchesFor env (matchCond : rest) pBaseCase t = do
      let (Binary Eq matchVar@(Var _ x) (Cons _ c _)) = matchCond
      scrT@(ScalarT (DatatypeT scrDT _ _) _) <- runInSolver $ currentAssignment (toMonotype $ symbolsOfArity 0 env Map.! x)
      let pScrutinee = Program (PSymbol x) scrT
      let ctors = ((env ^. datatypes) Map.! scrDT) ^. constructors
      let env' = addScrutinee pScrutinee env
      pBaseCase' <- cut $ inContext (\p -> Program (PMatch pScrutinee [Case c [] p]) t) $
                            generateMatchesFor (addAssumption matchCond env') rest pBaseCase t
                            
      let genOtherCases previousCases ctors = 
            case ctors of
              [] -> return $ Program (PMatch pScrutinee previousCases) t
              (ctor:rest) -> do
                (c, recheck) <- cut $ generateCase env' matchVar pScrutinee t ctor
                ifM (tryEliminateBranching (expr c) recheck)
                  (return $ expr c)
                  (genOtherCases (previousCases ++ [c]) rest)
         
      genOtherCases [Case c [] pBaseCase] (delete c ctors)

overDepthProgram d = overDepthProgramHelper (d+1) (Program PHole (SuccinctAny, AnyT))
  where
    overDepthProgramHelper d p@(Program prog (sty, rty)) = 
      if d == 0 
        then p 
        else overDepthProgramHelper (d-1) (Program (PApp (Program PHole (SuccinctAny,AnyT)) p) (SuccinctAny, AnyT))

keepIdCount old new = new {
  _typingState = (new ^. typingState) { _idCount = Map.unionWith max ((old ^. typingState) ^. idCount) ((new ^. typingState) ^. idCount) },
  _symbolUseCount = Map.unionWith max (old ^. symbolUseCount) (new ^. symbolUseCount)
}

walkThrough :: (MonadHorn s, MonadIO s) => Environment -> ProgramQueue -> Explorer s (Maybe (SProgram, ExplorerState), ProgramQueue)
walkThrough env pq =
  if PQ.size pq == 0
    then return (Nothing, PQ.empty)
    else do 
      let (score, ProgramItem p pes constraints node) = PQ.findMax pq
      writeLog 2 $ text "Score for" <+> pretty (toRProgram p) <+> text "is" <+> text (show score)
      es <- get
      put $ keepIdCount es pes
      let pq' = PQ.deleteMax pq
      writeLog 2 $ text "Current queue size" <+> text (show $ PQ.size pq')
      ctx <- asks . view $ _1 . context
      if not (hasHole p)
        then do
          writeLog 2 $ text "Checking" <+> pretty (toRProgram p) <+> text "in" $+$ pretty (ctx (untyped PHole))
          ifte (runInSolver solveTypeConstraints) 
              (\() -> do es' <- get; return (Just (p, es') , pq'))
              (do es' <- get; put $ keepIdCount es' es; walkThrough env pq')
        else do
          
          -- checking the partial program before filling holes
          writeLog 2 $ text "Checking" <+> pretty (toRProgram p) <+> text "in" $+$ pretty (ctx (untyped PHole))
          -- check the last filled parameter fits the hole
          ifte (runInSolver solveTypeConstraints) 
              (\() -> do
                case constraints of
                  c:cs -> do
                    mapM_ addConstraint constraints
                    ifte (runInSolver solveTypeConstraints)
                      (\()-> fillAndEnqueue p pq')
                      (do currSt <- get; put $ keepIdCount currSt es; walkThrough env pq')
                  [] -> fillAndEnqueue p pq')
              -- (typingState .= ts >> walkThrough env pq')
              (do currSt <- get; put $ keepIdCount currSt es; walkThrough env pq')
  where
    typeOfFirstHole (Program p (sty,rty,typ)) = case p of
      PHole -> do
        tass <- use (typingState . typeAssignment)
        let rty' = typeSubstitute tass rty
        let styp = toSuccinctType (rty')
        let subst = Set.foldr (\t acc -> Map.insert t SuccinctAny acc) Map.empty (extractSuccinctTyVars styp `Set.difference` Set.fromList (env ^. boundTypeVars))
        let succinctTy = outOfSuccinctAll $ succinctTypeSubstitute subst styp
        return (succinctTy, rty', typ)
      PApp fun arg -> if hasHole fun then typeOfFirstHole fun else typeOfFirstHole arg
      _ -> error "we are not handling none-application now"

    fillAndEnqueue p pq' = do
      d <- asks . view $ _1 . eGuessDepth
      es' <- get
      writeLog 2 $ text "Filling holes in" <+> pretty (toRProgram p)
      holeTy <- typeOfFirstHole p
      candidates <- uncurry3 (termWithType env) holeTy
      currSt <- get
      put $ keepIdCount currSt es'
      filteredCands <- mapM (\(prog, progES) -> do
        currSt <- get
        put $ keepIdCount currSt progES
        (p', constraints') <- fillFirstHole env p prog
        fes <- get
        put (keepIdCount fes es')
        if depth p' <= d 
          then return (Just $ ProgramItem p' fes constraints' SuccinctAny)
          else return Nothing
        ) candidates --if hasHole p then PQ.insertBehind  prog accQ else PQ.insertBehind 1 prog accQ
      newPQ <- foldM (\accQ prog@(ProgramItem p _ _ _) -> do
        score <- lift . lift . lift . liftIO $ termScore env p
        return $ PQ.insertBehind score prog accQ) pq' $ map fromJust $ filter isJust filteredCands
      walkThrough env newPQ

    holesOf (Program p (_, typ, _)) = case p of
      PApp fun arg -> holesOf fun ++ holesOf arg
      PHole -> [typ]
      _ -> []
    hasRoomForParams p = length (holesOf p >.> (Map.elems $ Map.filterWithKey (\k v -> Set.notMember k (symbolsOf p)) (env ^. arguments))) == 0

termWithType :: (MonadHorn s, MonadIO s) => Environment -> SuccinctType -> RType -> RType -> Explorer s [(SProgram, ExplorerState)]
termWithType env sty rty typ = do
  if isFunctionType rty
    then do -- Higher-order argument: its value is not required for the function type, return a placeholder and enqueue an auxiliary goal
      d <- asks . view $ _1 . auxDepth 
      if d <= 0
        then do
          writeLog 2 (text "Cannot synthesize higher-order argument: no auxiliary functions allowed")
          return []
        else do
          arg <- enqueueGoal env rty (untyped PHole) (d - 1)
          es <- get
          return [(toSProgram env arg, es)]
    else do
      writeLog 2 $ text "Looking for rtype" <+> pretty rty
      let styp = outOfSuccinctAll $ toSuccinctType rty
      writeLog 2 $ text "Looking for succinct type" <+> pretty styp
      let ids = Set.toList $ Set.unions $ HashMap.elems $ findDstNodesInGraph env sty
      -- writeLog 2 $ text "found ids" <+> pretty (map getEdgeId ids)
      useCounts <- use symbolUseCount
      let sortedIds = if isSuccinctFunction sty
                      then sortBy (mappedCompare (\(SuccinctEdge x _ _) -> (Set.member x (env ^. constants), (Map.findWithDefault 0 x useCounts)))) ids
                      else sortBy (mappedCompare (\(SuccinctEdge x _ _) -> (not $ Set.member x (env ^. constants), (Map.findWithDefault 0 x useCounts)))) ids    
      -- writeLog 2 $ text "found ids" <+> pretty (map getEdgeId sortedIds)
      es <- get
      mapM (\edge -> do
        let id = edge ^. symbolId
        case lookupSymbol id (-1) env of
          Nothing -> error ("symbol " ++ id ++ "not in the scope")
          Just sch -> do
            let pc = edge ^. params
            t <- symbolType env id sch -- instantiate the type with fresh names
            case Map.lookup id (env ^. shapeConstraints) of
              Nothing -> return ()
              Just sc -> addConstraint $ Subtype env (refineBot env $ shape t) (refineTop env sc) False ""
            symbolUseCount %= Map.insertWith (+) id 1
            if pc == 0
              then do
                -- writeLog 2 $ text "Trying" <+> text id
                let succinctTy = outOfSuccinctAll (toSuccinctType (t))
                let p = Program (PSymbol id) (succinctTy, t, typ)
                addConstraint $ Subtype env t rty False "" -- Add subtyping check, unless it's a function type and incremental checking is diasbled
                when (arity rty > 0) (addConstraint $ Subtype env t rty True "") -- Add consistency constraint for function types
                es' <- get
                put $ keepIdCount es' es
                return (p, es')
              else do -- it means it is a compound node here
                d' <- asks . view $ _1 . eGuessDepth
                tFun <- buildFunctionType pc rty
                let succinctTy = outOfSuccinctAll (toSuccinctType (t))
                -- p <- generateSketch env succinctTy
                let p = Program (PSymbol id) (succinctTy, t, tFun)
                -- writeLog 2 $ text "Trying" <+> text id
                addConstraint $ Subtype env t tFun False "" -- Add subtyping check, unless it's a function type and incremental checking is diasbled
                when (arity tFun > 0) (addConstraint $ Subtype env t tFun True "") -- Add consistency constraint for function types
                let p' = buildApp pc (Program (PSymbol id) (succinctTy,t, tFun))
                es' <- get
                put $ keepIdCount es' es
                return (p', es')
        ) $ filter (\(SuccinctEdge id _ _) -> id /= "__goal__" && id /= "" && not ("||" `isInfixOf` id)) sortedIds
  where
    buildApp 0 p = p
    buildApp paramCnt p@(Program _ (styp,rtyp,typ)) = case styp of
      SuccinctFunction _ argSet retTy -> let
        FunctionT x tArg tRet = rtyp
        FunctionT x' tArg' tRet' = typ
        arg = outOfSuccinctAll $ toSuccinctType (tArg)
        args = if paramCnt > Set.size argSet || paramCnt == 1 then Set.delete arg argSet else argSet
        in buildApp (paramCnt - 1) (Program (PApp p (Program PHole (arg, tArg, tArg'))) ((if paramCnt == 1 then retTy else SuccinctFunction (paramCnt-1) args retTy), tRet, tRet'))
      _ -> p -- buildApp args (Program (PApp p (Program PHole arg)) (styp, rtyp))

    buildFunctionType 0 typ = return typ
    buildFunctionType paramCnt typ = do
      x <- freshId "X"
      buildFunctionType (paramCnt - 1) (FunctionT x AnyT typ)

fillFirstHole :: (MonadHorn s, MonadIO s) => Environment -> SProgram -> SProgram -> Explorer s (SProgram, [Constraint])
fillFirstHole env (Program p (sty, rty, typ)) subprogram = case p of
  PHole -> return (subprogram, [])
  PApp fun arg -> if hasHole fun
    then do 
      (fun', c) <- fillFirstHole env fun subprogram
      let (_, tFun@(FunctionT x tArg tRet), cFun@(FunctionT cx cArg cRet)) = typeOf fun'
      let (argSty, _, _) = typeOf arg
      let arg' = Program (content arg) (argSty, tArg, cArg)
      let tRet' = appType env (toRProgram arg') x tRet
      -- add partial program type constraints
      let p' = Program (PApp fun' arg') (sty, tRet', cRet)
      if (hasHole fun && not (hasHole fun')) 
        then do
          let subConstraint = (Subtype env tFun cFun False ""):c -- add subtyping constraint
          let conConstraint = (Subtype env tFun cFun True ""):subConstraint -- add consistency constraint
          return (p', conConstraint)
        else return (p', c)
    else do
      (arg', c) <- fillFirstHole env arg subprogram
      let (_, FunctionT x tArg tRet, FunctionT cx cArg cRet) = typeOf fun
      let tRet' = appType env (toRProgram arg') x tRet
      -- add arguments type constraints
      when (hasHole arg && not (hasHole arg') && not (isFunctionType tArg) && depth arg' /= 0) (addConstraint $ Subtype env (typeOf (toRProgram arg')) tArg False "")
      let p' = Program (PApp fun arg') (sty, tRet', cRet)
      return (p', c)
  _ -> error "unsupported program type"

toRProgram :: SProgram -> RProgram
toRProgram (Program p (_, rty, _)) = case p of
  PApp fun arg -> Program (PApp (toRProgram fun) (toRProgram arg)) rty
  PSymbol id -> Program (PSymbol id) rty
  PHole -> Program PHole rty

toSProgram :: Environment -> RProgram -> SProgram
toSProgram env (Program p typ) = case p of
  PApp fun arg -> Program (PApp (toSProgram env fun) (toSProgram env arg)) (outOfSuccinctAll (toSuccinctType (typ)),typ, typ)
  PSymbol id -> Program (PSymbol id) (outOfSuccinctAll (toSuccinctType (typ)), typ, typ)
  PHole -> Program PHole (outOfSuccinctAll (toSuccinctType (typ)), typ, typ)

checkArguments :: (MonadHorn s, MonadIO s) => Environment -> RProgram -> Explorer s RProgram
checkArguments env (Program p typ) = case p of
  PSymbol id -> case lookupSymbol id (arity typ) env of
    Nothing -> error ("symbol" ++ id ++ "not in the scope")
    Just sch -> do
      t <- symbolType env id sch -- instantiate the type
      return (Program (PSymbol id) t)
  PApp fun arg -> do
    fun' <- checkArguments env fun
    arg' <- checkArguments env arg
    let FunctionT x tArg tRet = typeOf fun'
    let tRet' = appType env arg' x tRet
    addConstraint $ Subtype env (typeOf arg') tArg False ""
    return $ Program (PApp fun' arg') tRet'

initProgramQueue :: (MonadHorn s, MonadIO s) => Environment -> RType -> Explorer s ProgramQueue
initProgramQueue env typ = do
  tass <- use (typingState . typeAssignment)
  let typ' = typeSubstitute tass typ
  writeLog 2 $ text "Looking for type" <+> pretty typ'
  let styp = toSuccinctType (typ')
  let subst = Set.foldr (\t acc -> Map.insert t SuccinctAny acc) Map.empty (extractSuccinctTyVars styp `Set.difference` Set.fromList (env ^. boundTypeVars))
  let succinctTy = outOfSuccinctAll $ succinctTypeSubstitute subst styp
  let p = Program PHole (succinctTy, typ, AnyT)
  -- ts <- use typingState
  es <- get
  score <- lift . lift . lift . liftIO $ termScore env p
  let pq = PQ.singleton score $ ProgramItem p es [] SuccinctAny
  return pq

getKSolution :: (MonadHorn s, MonadIO s) => Environment -> Explorer s ()
getKSolution env = do
  
  let params = Map.keys (env ^. arguments)
  z3Env <- liftIO $ Z3.newEnv Nothing stdOpts
  let edgeType = BoolVar
  (edgeConsts, nodeConsts) <- liftIO $ evalZ3WithEnv (addGraphConstraints simplifiedGraph goalTy params edgeType) z3Env
  -- liftIO $ evalZ3WithEnv (getPathSolution simplifiedGraph edgeConsts nodeConsts edgeType) z3Env
  cnt <- asks . view $ _1 . solutionCnt
  -- return ()
  getKSolution' z3Env edgeConsts nodeConsts edgeType cnt
  where
    simplifiedGraph = env ^. graphFromGoal
    -- simplifiedGraph = graphWithin goalTy 4 $ HashMap.map (HashMap.map (Set.filter ((>=) 8.920956316770301 . getEdgeWeight))) (env ^. graphFromGoal)
    getKSolution' _ _ _ _ n | n == 0 = return ()
    getKSolution' z3Env edgeConsts nodeConsts edgeType n = do
      liftIO $ evalZ3WithEnv (getPathSolution simplifiedGraph edgeConsts nodeConsts edgeType) z3Env
      getKSolution' z3Env edgeConsts nodeConsts edgeType (n-1)
    goalTy = lastSuccinctType $ findSuccinctSymbol "__goal__"
    findSuccinctSymbol sym = outOfSuccinctAll $ HashMap.lookupDefault SuccinctAny sym $ env ^. succinctSymbols

-- | To include all the provided parameters in our solution, 
-- we first find a junction node in the graph of paths from goal type to parameters
-- start from the junction node, we separately find a path to all parameters and build a sketch from these paths
-- TODO how to ensure the SOUNDNESS when splitting of the set of parameters? 
-- there exists a permutation of these parameters to satisfy the final solution
splitGoal :: (MonadHorn s, MonadIO s) => Environment -> RType -> Explorer s RProgram
splitGoal env t = do
  -- pick up a node as the possible intersection of the paths to both the goal type and parameter types
  -- TODO how to sort these nodes to get the most likely composite node first?
  let goalTy = lastSuccinctType $ findSuccinctSymbol "__goal__"
  let graph = env ^. graphFromGoal
  let initState = DijkstraState (HashMap.singleton goalTy (0::Double)) HashMap.empty HashMap.empty
  let initQueue = MinPQ.singleton 0.0 goalTy -- nub $ goalTy:(HashMap.keys graph ++ concat (HashMap.elems $ HashMap.map HashMap.keys graph))
  dijkstraState <- dijkstraHelper graph initState initQueue []

  auxd <- asks . view $ _1 . auxDepth
  let isValidComp x = isSuccinctComposite x && if auxd == 0 then not (hasSuccinctFunction x) else True
  let allNodes = sortOn (flip (HashMap.lookupDefault 99999) $ dijkstraDist dijkstraState) $ Set.toList $ Set.filter isValidComp $ nodes True env
  -- let allNodes = Set.toList $ Set.filter isValidComp $ nodes True env
  writeLog 2 $ text "top 10 candidates are" <+> pretty (take 10 allNodes)
  -- try to find paths to all of the parameters and construct a partial program
  msum $ map (\xnode -> do
    holedProgram <- generateSketch env xnode
    -- feed it to synquid to get a complete program
    writeLog 2 $ text "program with holes" <+> pretty holedProgram
    Reconstructor _ reconstructETopLevel <- asks . view $ _3
    p <- once $ reconstructETopLevel env t $ toRProgram holedProgram
    writeLog 2 $ text "find a partial program" <+> pretty p
    return p
    ) allNodes
  where
    findSuccinctSymbol sym = outOfSuccinctAll $ HashMap.lookupDefault SuccinctAny sym $ env ^. succinctSymbols
 
withinAuxDepth env d [] = d >= 0
withinAuxDepth env d (p:ps) = if d < 0
  then False 
  else let x = p
           rtyp = toMonotype $ fromJust $ lookupSymbol x (-1) env
       in if x /= "" && isHigherOrder rtyp
         then withinAuxDepth env (d-1) ps
         else withinAuxDepth env d ps

generateSketch :: (MonadHorn s, MonadIO s) => Environment -> SuccinctType -> Explorer s SProgram
generateSketch env xnode = do
  let SuccinctComposite tys = xnode
  -- guard ((Map.size (env ^. arguments) <= 1) || cnt >= 2)
  writeLog 2 $ text "Trying" <+> pretty xnode
  -- writeLog 2 $ text $ showGraphViz True env
  -- randomly select two types as the source node from the composite set
  let hlist = Set.toList tys
  let srcOpts = [(ty1, ty2) | ty1 <- hlist, let tlist = if length hlist > 1 then delete ty1 hlist else hlist, ty2 <- tlist]
  
  msum $ map (\(src1, src2) -> do
    (goalPath, paths) <- generatePath src1 src2
    d <- asks . view $ _1 . auxDepth
    writeLog 2 $ text "auxDepth now:" <+> pretty d
    -- all the paths cannot exceed the auxDepth limitation, cannot use two nested high-order function
    -- if foldr ((&&) . not . null) (not $ null goalPath) paths
    --   then if foldr ((&&) . withinAuxDepth d) (withinAuxDepth d goalPath) (map ((++) goalPath) paths)
    --     then do
          -- writeLog 2 $ text "find a path through" <+> pretty xnode
          -- writeLog 2 $ pretty (map (map (Set.toList . Set.map getEdgeId)) paths)
    target <- buildApp "" goalPath
    writeLog 2 $ pretty target
    argProgs <- mapM (uncurry buildApp) $ zip (Map.keys $ env ^. arguments) paths
    writeLog 2 $ pretty  argProgs
    
    let td = depth target
    arg1 <- generateArg td src1 target $ head argProgs
    args <- mapM (generateArg td src2 target) $ tail argProgs
    -- writeLog 2 $ text "generated argument" <+> pretty p
    -- let typedProgs = (fillType src1 $ head argProgs) : (map (fillType src2) $ tail argProgs)
    -- writeLog 2 $ pretty Progs
    writeLog 2 $ text "target is" <+> pretty target 
    foldM fillHoleWithType target (map (toSProgram env) (arg1:args))           
      --   else (writeLog 2 $ text "path has too many high-order functions for" <+> pretty xnode) >> mzero
      -- else (writeLog 2 $ text "fail to find a path for type" <+> pretty xnode) >> mzero
    ) srcOpts
  where
    generateArg td src target p = do
      t <- findRType target src
      Reconstructor _ reconstructETopLevel <- asks . view $ _3
      -- local (set (_1 . pathSearch) False) $ reconstructETopLevel env t $ toRProgram p
      reconstructETopLevel env t $ toRProgram p

    generatePath src1 src2 = do
      d <- asks . view $ _1 . eGuessDepth
      writeLog 2 $ text "guess depth is" <+> pretty d
      generatePathAt src1 src2 1

    generatePathAt src1 src2 dp = do
      d <- asks . view $ _1 . eGuessDepth
      -- guard $ dp <= d
      auxd <- asks . view $ _1 . auxDepth
      -- let allGoalPaths = pathAtFromTo2 env goalTy xnode dp
      -- writeLog 2 $ text "the number of paths from goal is" <+> pretty (length allGoalPaths)
      -- let SuccinctComposite tys = xnode
      let goalTy = lastSuccinctType $ findSuccinctSymbol "__goal__"
      goalPath <- findPathTo 1 goalTy xnode "" -- pathAtFromTo env goalTy xnode dp
      if withinAuxDepth env auxd goalPath && not (null goalPath)
      
      -- path1 <- findPathTo (d-dp+1) src1 $ snd . Map.findMin $ env ^. arguments
      -- guard $ withinAuxDepth auxd (goalPath ++ path1) && not (null (removeTrailingEmpty path1))
      -- writeLog 2 $ text "find path" <+> pretty (map (Set.toList . Set.map getEdgeId) path1)

      -- path2 <- mapM (findPathTo (d-dp+1) src2) $ Map.elems $ Map.deleteMin $ env ^. arguments
      -- guard $ foldr ((&&) . withinAuxDepth auxd) True path2 && foldr ((&&) . not . null) True path2
      -- writeLog 2 $ text "find path2"

      -- paths <- return $ path1:path2
        then do
          writeLog 2 $ text "path from goal:" <+> pretty goalPath
          tass <- use (typingState . typeAssignment)
          writeLog 2 $ text "arguments" <+> pretty (Map.toList $ env ^. arguments)
          paths <- liftM2 (:) (uncurry (flip (findPathTo (d-dp+1) src1 . SuccinctInhabited . outOfSuccinctAll . toSuccinctType . typeSubstitute tass . toMonotype)) $ Map.findMin $ env ^. arguments)
                   $ mapM (uncurry (flip (findPathTo (d-dp+1) src2 . SuccinctInhabited . outOfSuccinctAll . toSuccinctType . typeSubstitute tass . toMonotype))) $ Map.toList $ Map.deleteMin $ env ^. arguments
          writeLog 2 $ text "paths for args:" <+> pretty paths
          if foldr ((&&) . withinAuxDepth env auxd . ((++) goalPath)) True paths && foldr ((&&) . not . null) True paths
            then do
              return (goalPath, paths)
            else mzero
        else mzero

    fillType sty (Program p (_, typ, _)) = Program p (sty, typ, typ) 
    graphWoutGoal = removeEdge "__goal__" $ env ^. graphFromGoal

    findRType prog@(Program p (sty, typ, _)) tSty = case p of
      PApp fun arg -> let (sArg, tArg, _) = typeOf arg in if tSty == sArg then return tArg else findRType arg tSty `mplus` findRType fun tSty
      PSymbol _ -> if sty == tSty then return typ else mzero
      PHole -> if sty == tSty then return typ else mzero
      _ -> error "Cannot find a type in terms more than application"

    findSuccinctSymbol sym = outOfSuccinctAll $ HashMap.lookupDefault SuccinctAny sym $ env ^. succinctSymbols

    findPathTo d src t id = do
      -- liftIO $ print $ HashMap.keys $ HashMap.lookupDefault HashMap.empty src (env ^. graphFromGoal)
      yen (keepOnly id graphWoutGoal) src t 1
      -- writeLog 2 $ pretty paths
      -- msum $ map (return . removeTrailingEmpty) paths
      -- dijkstra (removeEdge "__goal__" $ env ^. graphFromGoal) src t
      -- msum $ map (\dp -> do
      --   writeLog 2 $ text "finding path with length" <+> pretty dp <+> text "from" <+> pretty src <+> text "to" <+> pretty t
      --   pathAtFromTo env src (SuccinctInhabited 
      --                         . outOfSuccinctAll 
      --                         . toSuccinctType 
      --                         . typeSubstitute tass 
      --                         . toMonotype $ t) dp) [1..d]
    
    fillHoleWithType prog@(Program p (sty, typ, _)) term@(Program _ (tsty, ttyp, _)) = case p of
      PApp fun arg -> ifte ((\x -> Program (PApp fun x) (sty, typ, typ)) <$> fillHoleWithType arg term)
                           return
                           ((\x -> Program (PApp x arg) (sty, typ, typ)) <$> fillHoleWithType fun term)
      PHole -> if sty == tsty then return term else mzero
      _ -> mzero
    
    termWithHoles p@(Program _ (sty, typ, _)) = case typ of
      FunctionT x tArg tRet -> let SuccinctFunction paramCnt argSet retTy = sty
                                   arg = outOfSuccinctAll $ toSuccinctType (tArg)
                                   args = if paramCnt > Set.size argSet || paramCnt == 1 then Set.delete arg argSet else argSet
                               in termWithHoles (Program (PApp p (Program PHole (arg, tArg, tArg))) ((if paramCnt == 1 then retTy else SuccinctFunction (paramCnt-1) args retTy), tRet, tRet))
      _ -> p
    
    fillHoleWithTerm prog@(Program p (sty, typ, _)) term@(Program _ (tsty, ttyp, _)) = case p of
      PApp fun arg -> ifte ((\x -> Program (PApp fun x) (sty, typ, typ)) <$> fillHoleWithTerm arg term) 
                           return 
                           ((\x -> Program (PApp x arg) (sty, typ, typ)) <$> fillHoleWithTerm fun term)
      PHole -> if shape ttyp == shape typ then return term else do
        writeLog 2 $ text "type" <+> pretty ttyp <+> text "and type" <+> pretty typ <+> text "does not match"
        mzero
      _ -> mzero
    
    -- getGraphWeights $ Set.toList $ symbolsOf prog
    isNameEq name1 name2 = let pt1 = splitBy '.' name1
                               pt2 = splitBy '.' name2
                               commonNames = pt1 >.< pt2
                           in length commonNames >= 2 && (last pt1 == last pt2)

    newTerm x = do
      tass <- use $ typingState . typeAssignment
      rtyp <- typeSubstitute tass <$> instantiateWithoutConstraint env (fromJust $ lookupSymbol x (-1) env) True []
      let p = termWithHoles $ Program (PSymbol x) (findSuccinctSymbol x, rtyp, rtyp)
      return p

    buildApp arg path = case path of
      [] -> error "cannot build term from empty path"
      e:xs | length xs == 0 && arg /= "" -> newTerm arg
           | e == "" || "||" `isInfixOf` e -> buildApp arg xs
           | otherwise -> do
                nt <- newTerm e
                if length xs == 0 then return nt else do
                  p <- buildApp arg xs
                  writeLog 2 $ text "buildApp:" <+> pretty p
                  fillHoleWithTerm nt p

-- helper function to remove empty ids in a path
removeTrailingEmpty path = case path of
  [] -> []
  (x:xs) -> if "||" `isInfixOf` x || x == ""
         then removeTrailingEmpty xs 
         else x:removeTrailingEmpty xs

-- Dijkstra's algorithm
data DijkstraState = DijkstraState {
  dijkstraDist :: HashMap SuccinctType Double,
  dijkstraPrev :: HashMap SuccinctType SuccinctType,
  dijkstraEdge :: HashMap SuccinctType Id
}

dijkstra :: (MonadHorn s, MonadIO s) => SuccinctGraph -> SuccinctType -> SuccinctType -> Explorer s [Id]
dijkstra graph src dst = do
    st <- dijkstraHelper graph initState initQueue [dst]
    buildPath (dijkstraEdge st) (dijkstraPrev st)
  where
    initState = DijkstraState (HashMap.singleton src (0::Double)) HashMap.empty HashMap.empty
    initQueue = MinPQ.singleton 0.0 src -- nub $ src:(HashMap.keys graph ++ concat (HashMap.elems $ HashMap.map HashMap.keys graph))
    buildPathHelper edge prev curr path 
      | HashMap.member curr prev = do
        buildPathHelper edge prev (fromJust $ HashMap.lookup curr prev) ((HashMap.lookupDefault "" curr edge):path)
      | otherwise = return $ removeTrailingEmpty $ (HashMap.lookupDefault "" curr edge):path
    buildPath edge prev = buildPathHelper edge prev dst []
    
dijkstraHelper :: (MonadHorn s, MonadIO s) => SuccinctGraph -> DijkstraState -> MinPQueue Double SuccinctType -> [SuccinctType] -> Explorer s DijkstraState
dijkstraHelper graph state queue dsts
  | null queue = return state
  | otherwise = do
    let dist = dijkstraDist state
    let prev = dijkstraPrev state
    let edge = dijkstraEdge state
    -- liftIO $ print $ length queue
    let (curr, queue') = fromJust $ MinPQ.minView queue
    -- liftIO $ print curr
    -- let ([curr], queue') = splitAt 1 $ sortOn (flip (HashMap.lookupDefault infinity) dist) queue
    if curr `elem` dsts
      then return state
      else do
        let neighbours = map (\(ty, set) -> (ty, minimumBy succinctEdgeComp $ Set.toList set)) $ HashMap.toList $ HashMap.lookupDefault HashMap.empty curr graph
        (dist', prev', edge', queue'') <- do
          if HashMap.member curr dist -- if the current is reachable from the source node
            then return $ foldr (updateDist curr (fromJust $ HashMap.lookup curr dist)) (dist, prev, edge, queue') neighbours
            else return (dist, prev, edge, queue')
        dijkstraHelper graph (DijkstraState dist' prev' edge') queue'' dsts
  where
    updateDist parent pw (node, e) (dist, prev, edge, q)
      | not (HashMap.member node dist) || pw + e ^. weight < (fromJust $ HashMap.lookup node dist) =
        ( HashMap.insert node (pw + e ^. weight) dist
        , HashMap.insert node parent prev
        , HashMap.insert node (e ^. symbolId) edge
        , MinPQ.insert (pw + e ^. weight) node q)
      | otherwise = (dist, prev, edge, q)
    -- notEmptyEdge e = Set.size e > 0

removeEdge :: Id -> SuccinctGraph -> SuccinctGraph
removeEdge id g = 
  HashMap.foldrWithKey (\k m gr ->
    HashMap.insert k (HashMap.foldrWithKey (\k' s m' -> let s' = Set.filter ((/=) id . getEdgeId) s
                                                        in if Set.null s' then m' else HashMap.insert k' s' m') HashMap.empty m) gr
  ) HashMap.empty g

keepOnly :: Id -> SuccinctGraph -> SuccinctGraph
keepOnly id g = 
  HashMap.foldrWithKey (\k m gr ->
    HashMap.insert k (HashMap.foldrWithKey (\k' s m' -> 
      HashMap.insert k' (if Set.member id $ Set.map getEdgeId s then Set.singleton $ SuccinctEdge id 0 0 else s) m') HashMap.empty m) gr
  ) HashMap.empty g

-- | Algorithms for pruning the graph by the distant @dist@ from the source node @src@
graphWithin :: SuccinctType -> Int -> SuccinctGraph -> SuccinctGraph
graphWithin src dist graph = pruneGraphByReachability shrunkGraph $ getReachableNodes (reverseGraph shrunkGraph) $ starters shrunkGraph
  where
    shrunkGraph = pruneGraphByReachability graph $ graphWithinHelper graph Set.empty [src] (HashMap.singleton src 0)
    starters g = filter (\typ -> isSuccinctInhabited typ || isSuccinctFunction typ || hasSuccinctAny typ)
                        $ nub $ HashMap.keys g ++ (concat $ HashMap.map HashMap.keys g)
    graphWithinHelper g visited toVisit dmap = case toVisit of
      [] -> visited
      curr:xs -> if Set.member curr visited
        then graphWithinHelper g visited xs dmap
        else let children = HashMap.toList (HashMap.lookupDefault HashMap.empty curr g)
                 currDist = fromJust $ HashMap.lookup curr dmap
                 dmap' = foldr (\(t, s) m -> HashMap.insert t (if isSuccinctComposite curr then currDist else currDist + 1) m) dmap children
             in if currDist >= dist 
              then graphWithinHelper g (Set.insert curr visited) xs dmap
              else graphWithinHelper g (Set.insert curr visited) (xs ++ (fst $ unzip children)) dmap'

-- | Yen's algorithm
-- Yen's algorithm is for finding the k shortest path in a graph
yen :: (MonadHorn s, MonadIO s) => SuccinctGraph -> SuccinctType -> SuccinctType -> Int -> Explorer s [Id]
yen graph src dst k = do
  writeLog 2 $ pretty src
  writeLog 2 $ pretty dst
  startPath <- dijkstra graph src dst
  -- liftIO $ print startPath
  return startPath `mplus` yenHelper 0 [startPath] []
  -- liftIO $ print "found shortest paths"
  -- return p
  where
    -- try to find spur nodes from the first node to the next to last node
    findSpurPath resPaths path idx = do
      let spurId = path !! idx
      let spurNode = head $ getNodeById graph spurId
      let rootPath = take idx path
      -- let prefRootPath = take idx path
      -- Remove the links that are part of the previous shortest paths which share the same root path.
      let g' = foldr (\resPath g -> if rootPath `isPrefixOf` resPath then removeEdge (head $ resPath >.> rootPath) g else g) graph resPaths
      -- remove the nodes in the root path from the graph
      let rootPathNodes = foldr ((++) . getNodeById g') [] rootPath
      -- let g'' = g'
      let g'' = HashMap.foldrWithKey (\k m res -> 
                                        if elem k rootPathNodes
                                          then res 
                                          else HashMap.insert k (HashMap.filterWithKey (\k' _ -> notElem k' rootPathNodes) m) res
                                          ) HashMap.empty g'
      spurPath <- dijkstra g'' spurNode dst
      if null spurPath 
        then return [] 
        else return $ rootPath ++ spurPath
    getNodeById g id = 
      HashMap.foldrWithKey (\k m ns -> 
        (HashMap.foldrWithKey (\k' set ns' -> if Set.member id $ Set.map getEdgeId set then k:k':ns' else ns') ns m)
      ) [] g
    -- pathLength [x] = 0
    -- pathLength (x:xs) = pathLength xs + (HashMap.lookupDefault (99999::Double) (head xs) $ HashMap.lookupDefault HashMap.empty x graph)
    yenHelper idx currPaths candidatePaths 
      | idx >= k = mzero -- return currPaths
      | otherwise = do
          let curr = currPaths !! idx
          writeLog 2 $ pretty curr
          candidates' <- filter (flip notElem currPaths) . nub <$> foldM (\candidates i -> do
            sp <- findSpurPath currPaths curr i
            return $ if null sp then candidates else (candidates ++ [sp])) candidatePaths [0..(length curr - 1)]
          if null candidates'
            then mzero -- return currPaths
            else do 
              writeLog 2 $ pretty candidates'
              candWeights <- liftIO $ mapM getGraphWeights candidates'
              let weightedCands = zip (map sum candWeights) candidates'
              let ([(_, shortestP)], otherCands) = splitAt 1 $ sort weightedCands
              return shortestP `mplus` yenHelper (idx + 1) (nub $ currPaths ++ [shortestP]) (snd $ unzip otherCands) 

-- | tarjan return structure
data TarjanState = TarjanState {
  -- temporary state during the algorithm running
  index :: Int,
  indices :: Map SuccinctType Int,
  lowlink :: Map SuccinctType Int,
  stack :: [SuccinctType],
  -- result
  components :: [[SuccinctType]]
}

-- | tarjan's algorithm
-- to compute the strongly connected components
tarjan :: SuccinctGraph -> IO TarjanState
tarjan graph = do
    let nodes = Set.fromList $ (HashMap.keys graph) ++ (HashMap.foldr ((++) . HashMap.keys) [] graph)
    let emptyState = TarjanState 0 Map.empty Map.empty [] []
    foldrM (\v st -> if Map.notMember v (indices st) then strongconnect v st else return st) emptyState nodes
  where
    strongconnect src state = do
      let state' = TarjanState (index state + 1) 
                               (Map.insert src (index state) (indices state))
                               (Map.insert src (index state) (lowlink state)) 
                               ((stack state) ++ [src]) (components state)
      let neighbours = HashMap.keys $ HashMap.lookupDefault HashMap.empty src graph
      state'' <- foldrM (updateLowlink src) state' neighbours
      if fromJust (Map.lookup src $ indices state'') == fromJust (Map.lookup src $ lowlink state'')
        then return $ let (stk, comp) = collectComponents src (stack state'')
                      in state'' {
                          stack = stk, 
                          components = if null comp then components state'' else comp:(components state'')
                        }
        else return state'' 

    collectComponents src stack = 
      if src /= last stack
        then let (stack', comps) = collectComponents src (init stack) in (stack', (last stack):comps)
        else (init stack, [src])
    updateLowlink src w st = do
      if Map.notMember w (indices st) 
        then do
          st' <- strongconnect w st
          return $ st' {
            lowlink = Map.insert src (min (fromJust $ Map.lookup src $ lowlink st') (fromJust $ Map.lookup w $ lowlink st')) (lowlink st')
          }
        else if elem w (stack st)
          then return $ st {
                  lowlink = Map.insert src (min (fromJust $ Map.lookup src $ lowlink st) (fromJust $ Map.lookup w $ indices st)) (lowlink st)
                }
          else return st

-- helper function
-- | reverse a graph
reverseGraph :: SuccinctGraph -> SuccinctGraph
reverseGraph graph = 
  HashMap.foldrWithKey (\k m g ->
    HashMap.foldrWithKey (\k' edges gr -> 
      HashMap.insertWith HashMap.union k' (HashMap.singleton k edges) gr
      ) g m
    ) HashMap.empty graph

bidijkstra :: SuccinctGraph -> SuccinctType -> SuccinctType -> IO [Id]
bidijkstra graph src dst = undefined

data RTQItem = RTQItem {
  rtqProgram :: RProgram, -- the program we are at      
  rtqPosition :: SuccinctType -- the node where we reach this program
}
type RTQueue = MinPQueue Double RProgram -- the global queue for shortest path search

-- multiDijkstra :: (MonadHorn s, MonadIO s) => SuccinctGraph -> [SuccinctType] -> Explorer s RProgram
-- multiDijkstra graph args = do
--   where
--     multiDijkstraHelper [state] []
-- roundtripGenerate :: (MonadHorn s, MonadIO s) => Environment -> SuccinctType -> [SuccinctType] -> Explorer s RProgram
-- roundtripGenerate env goal args = do
--   -- build a list of backward search queue for each arg type
--   mapM_ moveForward args fwq bwq
--   where
--     moveForward curr = do
--       if isSuccinctComposite curr -- if the current node is compound, wait until all the arguments for this are prepared
--         then moveBackward curr fwq bwq -- meet with the fwq
--         else do
--               let neighbours = HashMap.toList $ HashMap.lookupDefault HashMap.empty curr $ env ^. graphFromGoal
--               foldrM (uncurry $ enqueueProgram currQueue )

--     moveBackward = undefined

--     enqueueProgram queue prog progWgt node edges = do
--       edgeWgt <- liftIO $ getGraphWeights $ Set.toList edges
--       let progs = map (applyTo prog) $ Set.toList edges
--       foldr (\(w, p) -> MinPQ.insert (progWgt + w) $ RTQItem p node) queue $ zip edgeWgt progs

--     applyTo prog id = case lookupSymbol id 1 env of
--       Just sch -> let funTy = toMonotype sch
--                   in Program (PApp (PSymbol id funTy) prog) (lastType funTy)
--       Nothing  -> error $ "Cannot find symbol " ++ id ++ " in the current scope"


-- | eppstein k-best algorithm
data HeapItem = HeapItem ((Double, Id), MinHeap HeapItem)
data EppsteinPath = EppsteinPath {
  eppPrefPath :: Int,  -- index of the pref path in the path list
  eppHeap :: EppsteinHeap  -- the heap node
}
type EppsteinHeap = MinHeap HeapItem
type EppsteinQueue = MinPQueue Double EppsteinPath
data EppsteinState = EppsteinState {
  eppQueue :: EppsteinQueue,
  eppPaths :: [[Id]]
}

-- to find the k shortest path in a graph
-- TODO: change all the heap values to their edges
-- eppstein :: (MonadHorn s, MonadIO s) => SuccinctGraph -> SuccinctType -> SuccinctType -> Explorer s ()
-- eppstein graph src dst = do
--     shortestDist <- dijkstraDist <$> computeDijkstraState
--     -- place the root heap onto the queue
--     let pathQueue = MinPQ.singleton (fromJust $ HashMap.lookup src shortestDist) 
--                                     $ EppsteinPath (-1) Heap.empty
--     pickupNPath 1 pathQueue
--   where
--     initState = DijkstraState (HashMap.singleton src (0::Double)) HashMap.empty HashMap.empty
--     initQueue = nub $ src:(HashMap.keys graph ++ concat (HashMap.elems $ HashMap.map HashMap.keys graph))

--     computeDijkstraState :: (MonadHorn s, MonadIO s) => Explorer s DijkstraState
--     computeDijkstraState = dijkstraHelper (reverseGraph graph) initState initQueue

--     -- hashmap from ids (component names) to their side track weights
--     computeSideTrack :: (MonadHorn s, MonadIO s) => Explorer s (HashMap Id Double)
--     computeSideTrack = do
--       shortestDist <- dijkstraDist <$> computeDijkstraState
--       foldrM (\(from, m) strack -> foldrM (\(to, edges) strack' -> do
--         -- search for weight of these ids
--         let edgeNames = Set.toList $ Set.map getEdgeId edges
--         ws <- liftIO $ getGraphWeights edgeNames
--         -- sidetrack(e) = len(e) + d(head(e), t) - d(tail(e), t)
--         return $ foldr (\(name, w) strack'' -> if name == "" -- skip the empty edges
--                                                 then strack''
--                                                 else HashMap.insert name 
--                                                      (w + HashMap.lookupDefault 0 to shortestDist - 
--                                                           HashMap.lookupDefault 0 from shortestDist) 
--                                                      strack'')  strack' $ zip edgeNames ws
--         ) strack $ HashMap.toList m) HashMap.empty $ HashMap.toList graph
--     -- heaps for each node of the outgoing edges from this node H_out(v)
--     -- hashMap from nodes (SuccinctType) to MinHeaps
--     buildOutheap heap edges = do
--       sidetracks <- computeSideTrack
--       -- construct the heap with sidetracks not equal to zero
--       Set.foldr Heap.insert heap $ Set.filter (((/=) 0) . fst) 
--       -- find the sidetrack for the edges from side track map
--       -- store both the side track value and the id of the edge for easier retrieval later
--                                  $ Set.map ((\elmt -> ((HashMap.lookupDefault 0 elmt sidetracks, elmt), Heap.empty)) . getEdgeId) edges
    
--     computeNodeHeaps :: (MonadHorn s, MonadIO s) => Explorer s (HashMap SuccinctType EppsteinHeap)
--     computeNodeHeaps = do
--       let heaps = HashMap.foldrWithKey (\from m heaps -> 
--                                           HashMap.insert from 
--                                           (HashMap.foldrWithKey (\to edges heap -> buildOutheap heap edges) Heap.empty m) heaps) 
--                                        HashMap.empty graph
--       return $ fromJust $ Heap.view heaps

--     computeOutrootHeapAt :: (MonadHorn s, MonadIO s) => SuccinctType -> Explorer s EppsteinHeap
--     computeOutrootHeapAt curr = do
--       shortestTree <- dijkstraPrev <$> computeDijkstraState
--       nodeHeaps <- computeNodeHeaps
--       let currHeap = fromJust $ HashMap.lookup curr nodeHeaps
--       case HashMap.lookup curr shortestTree of
--         -- if we are at the root of the shortest path tree
--         Nothing -> return $ Heap.singleton currHeap
--         -- if we are at other node of the tree, first recursively construct the parents 
--         -- and then add themselves to H_T(nextT(v))
--         Just next -> do
--           hnext <- computeOutrootHeapAt next
--           -- merge the current heap with this next heaps
--           return $ Heap.insert currHeap hnext

--     -- heaps for H_T(v)
--     -- a helper function
--     composeShare :: (a -> b -> c) -> (a -> b) -> a -> c
--     composeShare f g x = f x (g x)

--     computeOutrootHeaps :: (MonadHorn s, MonadIO s) => Explorer s (HashMap SuccinctType EppsteinHeap)
--     computeOutrootHeaps = do
--       -- H_T(v) is a heap of all of the "best" sidetrack edges for each node on the shortest path from v to T.
--       -- H_T(v) is computed by adding the lowest cost sidetrack edge of v to heap H_T(nextT(v)),
--       -- where nextT(v) is the parent node of node v in the shortest path tree rooted at the target node T.
--       -- start from the src node and recursively do this computation
--       -- to simulate the 3-heap with binary heap, we use a tuple of (Double, MinHeap)
--       let nodes = allSuccinctNodes graph
--       return $ Set.foldr (composeShare HashMap.insert computeOutrootHeapAt) HashMap.empty nodes

--     toExplicitPath :: (MonadHorn s, MonadIO s) => EppsteinPath -> Explorer s [Id]
--     toExplicitPath path = undefined

--     getToNode id = head . HashMap.keys . map (HashMap.filter (Set.member id . Set.map getEdgeId)) $ HashMap.elems graph

--     pickupNPath n _ | n == 0 = return ()
--     pickupNPath n q = pickupNPath (n-1) <$> pickupPath q

--     pickupPath :: (MonadHorn s, MonadIO s) => EppsteinState -> Explorer s EppsteinState
--     pickupPath state = do
--       let ksp = eppPaths state
--       let pathQueue = eppQueue state
--       outrootHeaps <- computeOutrootHeaps
--       let (cost, kpathImplicit) = MinPQ.findMin pathQueue
--       liftIO $ print $ toExplicitPath kpathImplicit

--       let (root, remain) = fromJust $ Heap.view $ eppHeap kpathImplicit

--       -- push the children of this path within the heap to the queue, up to 3
--       let children = (snd root) : (Heap.take 2 remain)
--       let prefPath = eppPrefPath kpathImplicit
--       prefPathCost <- liftIO $ sum <$> getGraphWeights (ksp !! prefPath)
--       let pathQueue' = foldr (\heap q -> let candidateCost = prefPathCost + (fst . fst . fromJust $ Heap.viewHead heap)
--                                          in MinPQ.insert candidateCost 
--                                                          (EppsteinPath prefPath heap) 
--                                                          q) (MinPQ.deleteMin pathQueue) children

--       -- push the cross edges to the queue, the potential 4th child of the current node
--       sidetracks <- computeSideTrack
--       let sidetrackId = snd $ fst root
--       return $ EppsteinState (if HashMap.member sidetrackId sidetracks
--                                 then MinPQ.insert (prefPathCost + HashMap.lookupDefault 0 sidetrackId sidetracks)
--                                                   (EppsteinPath (length ksp - 1) (fromJust $ HashMap.lookup (getToNode sidetrackId) outrootHeaps))
--                                                   pathQueue'
--                                 else pathQueue')
--                               ksp

pathAtFromTo :: (MonadHorn s, MonadIO s) => Environment -> SuccinctType -> SuccinctType -> Int -> Explorer s [Set SuccinctEdge]
pathAtFromTo env src dst len = pathAtFromToHelper (env ^. graphFromGoal) [Node src []]
  where
    pathAtFromToHelper g toVisit = case toVisit of
      [] -> mzero
      curr:vs | typ curr == dst && length (path curr) == len -> if null (path curr) then pathAtFromToHelper g vs else return (path curr) `mplus` pathAtFromToHelper g vs
              | length (path curr) >= len -> do
                -- writeLog 2 $ text "ruling out" <+> pretty (typ curr)
                pathAtFromToHelper g vs
              | otherwise -> do
                -- writeLog 2 $ text "processing" <+> pretty (typ curr)
                -- writeLog 2 $ text "current path" <+> pretty (map (Set.toList . Set.map getEdgeId) $ path curr)
                let newNodes = nodesWithPath curr g
                -- writeLog 2 $ text "new added" <+> pretty (length newNodes)
                pathAtFromToHelper g (vs ++ newNodes)
    nodesWithPath node graph = map (uncurry (makeNode $ path node)) 
                               $ HashMap.toList 
                               $ HashMap.filter (\s -> Set.size s /= 1 || Set.notMember "__goal__" (Set.map getEdgeId s)) 
                               $ HashMap.lookupDefault HashMap.empty (typ node) graph
    makeNode currPath t id = Node t (currPath 
                                 ++ (let s = Set.filter (\e -> (getEdgeId e /= "__goal__" && getEdgeId e /= "")) id 
                                     in if Set.null s then [] else [s]))
                   
generateEWithGraph :: (MonadHorn s, MonadIO s) => Environment -> ProgramQueue -> RType -> Bool -> Bool -> Explorer s (ProgramQueue, RProgram)
generateEWithGraph env pq typ isThenBranch isElseBranch = do
  es <- get
  res <- walkThrough env pq
  case res of
    (Nothing, _) -> mzero
    (Just (p, pes), newPQ) -> do
      put $ keepIdCount es pes
      let refinedP = toRProgram p
      writeLog 2 $ text "Checking program" <+> pretty refinedP
      let p' = refinedP
      ifte (checkE env typ p')
        (\() -> when isThenBranch (termQueueState .= newPQ) >> return (newPQ, p'))
        (get >>= (return . flip keepIdCount es) >>= put >> generateEWithGraph env newPQ typ isThenBranch isElseBranch)

mergeTypingState env ts pts = pts {
  _typingConstraints = (ts ^. typingConstraints) ++ (filter (not . isCondConstraint) $ map (updateConstraintEnv env) (pts ^. typingConstraints)),
  _typeAssignment = Map.union (ts ^. typeAssignment) (pts ^. typeAssignment),
  _predAssignment = Map.union (ts ^. predAssignment) (pts ^. predAssignment),
  _qualifierMap = Map.union (ts ^. qualifierMap) (pts ^. qualifierMap),
  _candidates = ts ^. candidates,
  _idCount = Map.unionWith max (ts ^. idCount) (pts ^. idCount),
  _isFinal = ts ^. isFinal
  }

mergeExplorerState env es pes = es {
  _typingState = mergeTypingState env (es ^. typingState) (pes ^. typingState)
}

-- | 'generateE' @env typ@ : explore all elimination terms of type @typ@ in environment @env@
-- (bottom-up phase of bidirectional typechecking)
generateE :: (MonadHorn s, MonadIO s) => Environment -> RType -> Bool -> Bool -> Bool -> Explorer s RProgram
generateE env typ isThenBranch isElseBranch isMatchScrutinee = do
  useFilter <- asks . view $ _1 . useSuccinct
  d <- asks . view $ _1 . eGuessDepth
  pq <- if isElseBranch 
    then do
      q <- use termQueueState
      es <- get
      resQ <- mapM (\(k, ProgramItem prog pes c t) -> return $ Just (k, ProgramItem prog (mergeExplorerState env es pes) c t)) (PQ.toList q)
      return $ PQ.fromList $ map fromJust $ filter isJust resQ
    else initProgramQueue env typ
  prog@(Program pTerm pTyp) <- if useFilter && (not isMatchScrutinee) then repeatUtilValid pq else generateEUpTo env typ d
  runInSolver $ isFinal .= True >> solveTypeConstraints >> isFinal .= False  -- Final type checking pass that eliminates all free type variables
  newGoals <- uses auxGoals (map gName)                                      -- Remember unsolved auxiliary goals
  generateAuxGoals                                                           -- Solve auxiliary goals
  pTyp' <- runInSolver $ currentAssignment pTyp                              -- Finalize the type of the synthesized term
  addLambdaLets pTyp' (Program pTerm pTyp') newGoals                         -- Check if some of the auxiliary goal solutions are large and have to be lifted into lambda-lets
  where
    containsAllArguments p = Set.null $ Map.keysSet (env ^. arguments) `Set.difference` symbolsOf p 
    repeatUtilValid pq = 
      ifte (generateEWithGraph env pq typ isThenBranch isElseBranch)
        (\(pq',res) -> do
          if containsAllArguments res
            then return res `mplus` repeatUtilValid pq'
            else repeatUtilValid pq'
          )
        mzero
    addLambdaLets t body [] = return body
    addLambdaLets t body (g:gs) = do
      pAux <- uses solvedAuxGoals (Map.! g)
      if programNodeCount pAux > 5
        then addLambdaLets t (Program (PLet g uHole body) t) gs
        else addLambdaLets t body gs
        
-- | 'generateEUpTo' @env typ d@ : explore all applications of type shape @shape typ@ in environment @env@ of depth up to @d@
generateEUpTo :: (MonadHorn s, MonadIO s) => Environment -> RType -> Int -> Explorer s RProgram
generateEUpTo env typ d = msum $ map (generateEAt env typ) [0..d]

-- | 'generateEAt' @env typ d@ : explore all applications of type shape @shape typ@ in environment @env@ of depth exactly to @d@
generateEAt :: (MonadHorn s, MonadIO s) => Environment -> RType -> Int -> Explorer s RProgram
generateEAt _ _ d | d < 0 = mzero
generateEAt env typ d = do
  useMem <- asks . view $ _1 . useMemoization
  if not useMem || d == 0
    then do -- Do not use memoization
      p <- enumerateAt env typ d
      checkE env typ p
      return p
    else do -- Try to fetch from memoization store
      startState <- get
      let tass = startState ^. typingState . typeAssignment
      let memoKey = MemoKey (arity typ) (shape $ typeSubstitute tass (lastType typ)) startState d
      startMemo <- getMemo
      case Map.lookup memoKey startMemo of
        Just results -> do -- Found memoized results: fetch
          writeLog 3 (text "Fetching for:" <+> pretty memoKey $+$
                      text "Result:" $+$ vsep (map (\(p, _) -> pretty p) results))
          msum $ map applyMemoized results
        Nothing -> do -- Nothing found: enumerate and memoize
          writeLog 3 (text "Nothing found for:" <+> pretty memoKey)
          p <- enumerateAt env typ d

          memo <- getMemo
          finalState <- get
          let memo' = Map.insertWith (flip (++)) memoKey [(p, finalState)] memo
          writeLog 3 (text "Memoizing for:" <+> pretty memoKey <+> pretty p <+> text "::" <+> pretty (typeOf p))

          putMemo memo'

          checkE env typ p
          return p
  where
    applyMemoized (p, finalState) = do
      put finalState
      checkE env typ p
      return p

-- | Perform a gradual check that @p@ has type @typ@ in @env@:
-- if @p@ is a scalar, perform a full subtyping check;
-- if @p@ is a (partially applied) function, check as much as possible with unknown arguments
checkE :: (MonadHorn s, MonadIO s) => Environment -> RType -> RProgram -> Explorer s ()
checkE env typ p@(Program pTerm pTyp) = do
  ctx <- asks . view $ _1 . context
  writeLog 2 $ text "Checking" <+> pretty p <+> text "::" <+> pretty typ <+> text "in" $+$ pretty (ctx (untyped PHole))
  
  -- ifM (asks $ _symmetryReduction . fst) checkSymmetry (return ())
  
  incremental <- asks . view $ _1 . incrementalChecking -- Is incremental type checking of E-terms enabled?
  consistency <- asks . view $ _1 . consistencyChecking -- Is consistency checking enabled?
  
  when (incremental || arity typ == 0) (addConstraint $ Subtype env pTyp typ False "") -- Add subtyping check, unless it's a function type and incremental checking is diasbled
  when (consistency && arity typ > 0) (addConstraint $ Subtype env pTyp typ True "") -- Add consistency constraint for function types
  fTyp <- runInSolver $ finalizeType typ
  pos <- asks . view $ _1 . sourcePos
  typingState . errorContext .= (pos, text "when checking" </> pretty p </> text "::" </> pretty fTyp </> text "in" $+$ pretty (ctx p))
  runInSolver solveTypeConstraints
  typingState . errorContext .= (noPos, empty)
    -- where      
      -- unknownId :: Formula -> Maybe Id
      -- unknownId (Unknown _ i) = Just i
      -- unknownId _ = Nothing

      -- checkSymmetry = do
        -- ctx <- asks $ _context . fst
        -- let fixedContext = ctx (untyped PHole)
        -- if arity typ > 0
          -- then do
              -- let partialKey = PartialKey fixedContext
              -- startPartials <- getPartials
              -- let pastPartials = Map.findWithDefault Map.empty partialKey startPartials
              -- let (myCount, _) = Map.findWithDefault (0, env) p pastPartials
              -- let repeatPartials = filter (\(key, (count, _)) -> count > myCount) $ Map.toList pastPartials

              -- -- Turn off all qualifiers that abduction might be performed on.
              -- -- TODO: Find a better way to turn off abduction.
              -- solverState <- get
              -- let qmap = Map.map id $ solverState ^. typingState ^. qualifierMap
              -- let qualifiersToBlock = map unknownId $ Set.toList (env ^. assumptions)
              -- typingState . qualifierMap .= Map.mapWithKey (\key val -> if elem (Just key) qualifiersToBlock then QSpace [] 0 else val) qmap

              -- writeLog 2 $ text "Checking" <+> pretty pTyp <+> text "doesn't match any of"
              -- writeLog 2 $ pretty repeatPartials <+> text "where myCount is" <+> pretty myCount

              -- -- Check that pTyp is not a supertype of any prior programs.
              -- mapM_ (\(op@(Program _ oldTyp), (_, oldEnv)) ->
                               -- ifte (solveLocally $ Subtype (combineEnv env oldEnv) oldTyp pTyp False)
                               -- (\_ -> do
                                    -- writeLog 2 $ text "Supertype as failed predecessor:" <+> pretty pTyp <+> text "with" <+> pretty oldTyp
                                    -- writeLog 2 $ text "Current program:" <+> pretty p <+> text "Old program:" <+> pretty op
                                    -- writeLog 2 $ text "Context:" <+> pretty fixedContext
                                    -- typingState . qualifierMap .= qmap
                                    -- mzero)
                               -- (return ())) repeatPartials

              -- let newCount = 1 + myCount
              -- let newPartials = Map.insert p (newCount, env) pastPartials
              -- let newPartialMap = Map.insert partialKey newPartials startPartials
              -- putPartials newPartialMap

              -- typingState . qualifierMap .= qmap
          -- else return ()

      -- combineEnv :: Environment -> Environment -> Environment
      -- combineEnv env oldEnv =
        -- env {_ghosts = Map.union (_ghosts env) (_ghosts oldEnv)}

enumerateAt :: (MonadHorn s, MonadIO s) => Environment -> RType -> Int -> Explorer s RProgram
enumerateAt env typ 0 = do
  useFilter <- asks . view $ _1 . useSuccinct
  succinctTy <- styp'
  rs <- reachableSet
  let symbols = Map.toList $ symbolsOfArity (arity typ) env
  let filteredSymbols = if useFilter && succinctTy /= SuccinctAny then filter (\(id,_) -> Set.member id rs) symbols else symbols
  -- let filteredSymbols = symbols
  useCounts <- use symbolUseCount
  let sortedSymbols = if arity typ == 0
                    then sortBy (mappedCompare (\(x, _) -> (Set.member x (env ^. constants), (Map.findWithDefault 0 x useCounts)))) filteredSymbols
                    else sortBy (mappedCompare (\(x, _) -> (not $ Set.member x (env ^. constants), (Map.findWithDefault 0 x useCounts)))) filteredSymbols
  msum $ map pickSymbol sortedSymbols
  where
    styp' = do 
      tass <- use (typingState . typeAssignment)
      let typ' = typeSubstitute tass typ
      let styp = toSuccinctType ((if arity typ' == 0 then typ' else lastType typ'))
      let subst = Set.foldr (\t acc -> Map.insert t SuccinctAny acc) Map.empty (extractSuccinctTyVars styp `Set.difference` Set.fromList (env ^. boundTypeVars))
      return $ outOfSuccinctAll $ succinctTypeSubstitute subst styp
    reachableSet = do
      sty <- styp'
      return $ HashMap.foldr (\set acc -> Set.foldr (\(SuccinctEdge id _ _) ids-> Set.insert id ids) acc set) Set.empty (findDstNodesInGraph env sty)
    pickSymbol (name, sch) = do
      when (Set.member name (env ^. letBound)) mzero
      t <- symbolType env name sch
      let p = Program (PSymbol name) t
      writeLog 2 $ text "Trying" <+> pretty p
      symbolUseCount %= Map.insertWith (+) name 1      
      case Map.lookup name (env ^. shapeConstraints) of
        Nothing -> return ()
        Just sc -> addConstraint $ Subtype env (refineBot env $ shape t) (refineTop env sc) False ""
      return p
    
enumerateAt env typ d = do
  let maxArity = fst $ Map.findMax (env ^. symbols)
  guard $ arity typ < maxArity
  generateAllApps
  where
    generateAllApps =
      generateApp (\e t -> generateEUpTo e t (d - 1)) (\e t -> generateEAt e t (d - 1)) `mplus`
        generateApp (\e t -> generateEAt e t d) (\e t -> generateEUpTo e t (d - 1))

    generateApp genFun genArg = do
      x <- freshId "X"
      fun <- inContext (\p -> Program (PApp p uHole) typ)
                $ genFun env (FunctionT x AnyT typ) -- Find all functions that unify with (? -> typ)
      let FunctionT x tArg tRes = typeOf fun

      pApp <- if isFunctionType tArg
        then do -- Higher-order argument: its value is not required for the function type, return a placeholder and enqueue an auxiliary goal
          d <- asks . view $ _1 . auxDepth
          when (d <= 0) $ writeLog 2 (text "Cannot synthesize higher-order argument: no auxiliary functions allowed") >> mzero
          arg <- enqueueGoal env tArg (untyped PHole) (d - 1)
          return $ Program (PApp fun arg) tRes
        else do -- First-order argument: generate now
          let mbCut = id -- if Set.member x (varsOfType tRes) then id else cut
          arg <- local (over (_1 . eGuessDepth) (-1 +))
                    $ inContext (\p -> Program (PApp fun p) tRes)
                    $ mbCut (genArg env tArg)
          writeLog 3 (text "Synthesized argument" <+> pretty arg <+> text "of type" <+> pretty (typeOf arg))
          let tRes' = appType env arg x tRes
          return $ Program (PApp fun arg) tRes'
      return pApp
      
-- | Make environment inconsistent (if possible with current unknown assumptions)
generateError :: (MonadHorn s, MonadIO s) => Environment -> Explorer s RProgram
generateError env = do
  ctx <- asks . view $ _1. context
  writeLog 2 $ text "Checking" <+> pretty errorProgram <+> text "in" $+$ pretty (ctx errorProgram)
  tass <- use (typingState . typeAssignment)  
  let env' = typeSubstituteEnv tass env
  addConstraint $ Subtype env (int $ conjunction $ Set.fromList $ map trivial (allScalars env')) (int ffalse) False ""
  pos <- asks . view $ _1 . sourcePos
  typingState . errorContext .= (pos, text "when checking" </> pretty errorProgram </> text "in" $+$ pretty (ctx errorProgram))  
  runInSolver solveTypeConstraints
  typingState . errorContext .= (noPos, empty)
  return errorProgram
  where
    trivial var = var |=| var

-- | 'toVar' @p env@: a variable representing @p@ (can be @p@ itself or a fresh ghost)
toVar :: (MonadHorn s, MonadIO s) => Environment -> RProgram -> Explorer s (Environment, Formula)
toVar env (Program (PSymbol name) t) = return (env, symbolAsFormula env name t)
toVar env (Program _ t) = do
  g <- freshId "G"
  return (addLetBound g t env, (Var (toSort $ baseTypeOf t) g))
  
-- | 'appType' @env p x tRes@: a type semantically equivalent to [p/x]tRes;
-- if @p@ is not a variable, instead of a literal substitution use the contextual type LET x : (typeOf p) IN tRes
appType :: Environment -> RProgram -> Id -> RType -> RType
appType env (Program (PSymbol name) t) x tRes = substituteInType (isBound env) (Map.singleton x $ symbolAsFormula env name t) tRes
appType env (Program _ t) x tRes = contextual x t tRes

isPolyConstructor (Program (PSymbol name) t) = isTypeName name && (not . Set.null . typeVarsOf $ t)

enqueueGoal env typ impl depth = do
  g <- freshVar env "f"
  auxGoals %= ((Goal g env (Monotype typ) impl depth noPos) :)
  return $ Program (PSymbol g) typ

{- Utility -}

-- | Get memoization store
getMemo :: (MonadHorn s, MonadIO s) => Explorer s Memo
getMemo = lift . lift . lift $ use termMemo

-- | Set memoization store
putMemo :: (MonadHorn s, MonadIO s) => Memo -> Explorer s ()
putMemo memo = lift . lift . lift $ termMemo .= memo

-- getPartials :: (MonadHorn s, MonadIO s) => Explorer s PartialMemo
-- getPartials = lift . lift . lift $ use partialFailures

-- putPartials :: (MonadHorn s, MonadIO s) => PartialMemo -> Explorer s ()
-- putPartials partials = lift . lift . lift $ partialFailures .= partials

throwErrorWithDescription :: (MonadHorn s, MonadIO s) => Doc -> Explorer s a   
throwErrorWithDescription msg = do
  pos <- asks . view $ _1 . sourcePos
  throwError $ ErrorMessage TypeError pos msg

-- | Record type error and backtrack
throwError :: (MonadHorn s, MonadIO s) => ErrorMessage -> Explorer s a  
throwError e = do
  writeLog 2 $ text "TYPE ERROR:" <+> plain (emDescription e)
  lift . lift . lift $ typeErrors %= (e :)
  mzero
  
-- | Impose typing constraint @c@ on the programs
addConstraint c = do
  writeLog 3 $ text "Adding constraint" <+> pretty c
  typingState %= addTypingConstraint c

-- | Embed a type-constraint checker computation @f@ in the explorer; on type error, record the error and backtrack
runInSolver :: (MonadHorn s, MonadIO s) => TCSolver s a -> Explorer s a
runInSolver f = do
  tParams <- asks . view $ _2
  tState <- use typingState  
  res <- lift . lift . lift . lift $ runTCSolver tParams tState f
  case res of
    Left err -> throwError err
    Right (res, st) -> do
      typingState .= st
      return res

freshId :: (MonadHorn s, MonadIO s) => String -> Explorer s String
freshId = runInSolver . TCSolver.freshId

freshVar :: (MonadHorn s, MonadIO s) => Environment -> String -> Explorer s String
freshVar env prefix = runInSolver $ TCSolver.freshVar env prefix

-- | Return the current valuation of @u@;
-- in case there are multiple solutions,
-- order them from weakest to strongest in terms of valuation of @u@ and split the computation
currentValuation :: (MonadHorn s, MonadIO s) => Formula -> Explorer s Valuation
currentValuation u = do
  runInSolver $ solveAllCandidates
  cands <- use (typingState . candidates)
  let candGroups = groupBy (\c1 c2 -> val c1 == val c2) $ sortBy (\c1 c2 -> setCompare (val c1) (val c2)) cands
  msum $ map pickCandidiate candGroups
  where
    val c = valuation (solution c) u
    pickCandidiate cands' = do
      typingState . candidates .= cands'
      return $ val (head cands')

inContext ctx f = local (over (_1 . context) (. ctx)) f
    
-- | Replace all bound type and predicate variables with fresh free variables
-- (if @top@ is @False@, instantiate with bottom refinements instead of top refinements)
instantiate :: (MonadHorn s, MonadIO s) => Environment -> RSchema -> Bool -> [Id] -> Explorer s RType
instantiate env sch top argNames = do
  t <- instantiate' Map.empty Map.empty sch
  writeLog 3 (text "INSTANTIATE" <+> pretty sch $+$ text "INTO" <+> pretty t)
  return t
  where
    instantiate' subst pSubst (ForallT a sch) = do
      a' <- freshId "A"
      addConstraint $ WellFormed env (vart a' ftrue)
      instantiate' (Map.insert a (vart a' (BoolLit top)) subst) pSubst sch
    instantiate' subst pSubst (ForallP (PredSig p argSorts _) sch) = do
      let argSorts' = map (sortSubstitute (asSortSubst subst)) argSorts
      fml <- if top
              then do
                p' <- freshId (map toUpper p)
                addConstraint $ WellFormedPredicate env argSorts' p'
                return $ Pred BoolS p' (zipWith Var argSorts' deBrujns)
              else return ffalse
      instantiate' subst (Map.insert p fml pSubst) sch        
    instantiate' subst pSubst (Monotype t) = go subst pSubst argNames t
    go subst pSubst argNames (FunctionT x tArg tRes) = do
      x' <- case argNames of
              [] -> freshVar env "x"
              (argName : _) -> return argName
      liftM2 (FunctionT x') (go subst pSubst [] tArg) (go subst pSubst (drop 1 argNames) (renameVar (isBoundTV subst) x x' tArg tRes))
    go subst pSubst _ t = return $ typeSubstitutePred pSubst . typeSubstitute subst $ t
    isBoundTV subst a = (a `Map.member` subst) || (a `elem` (env ^. boundTypeVars))

-- | Replace all bound type variables with fresh free variables
instantiateWithoutConstraint :: (MonadHorn s, MonadIO s) => Environment -> RSchema -> Bool -> [Id] -> Explorer s RType
instantiateWithoutConstraint env sch top argNames = do
  t <- instantiate' Map.empty Map.empty sch
  return t
  where
    instantiate' subst pSubst (ForallT a sch) = do
      a' <- freshId "A"
      instantiate' (Map.insert a (vart a' (BoolLit top)) subst) pSubst sch
    instantiate' subst pSubst (ForallP (PredSig p argSorts _) sch) = do
      let argSorts' = map (sortSubstitute (asSortSubst subst)) argSorts
      fml <- if top
              then do
                p' <- freshId (map toUpper p)
                return $ Pred BoolS p' (zipWith Var argSorts' deBrujns)
              else return ffalse
      instantiate' subst (Map.insert p fml pSubst) sch        
    instantiate' subst pSubst (Monotype t) = go subst pSubst argNames t
    -- go subst pSubst argNames (FunctionT x tArg tRes) = do
    --   x' <- case argNames of
    --           [] -> freshVar env "x"
    --           (argName : _) -> return argName
    --   liftM2 (FunctionT x') (go subst pSubst [] tArg) (go subst pSubst (drop 1 argNames) (renameVar (isBoundTV subst) x x' tArg tRes))
    go subst pSubst _ t = return $ typeSubstitutePred pSubst . typeSubstitute subst $ t
    isBoundTV subst a = (a `Map.member` subst) || (a `elem` (env ^. boundTypeVars))
    

-- | 'symbolType' @env x sch@: precise type of symbol @x@, which has a schema @sch@ in environment @env@;
-- if @x@ is a scalar variable, use "_v == x" as refinement;
-- if @sch@ is a polytype, return a fresh instance
symbolType :: (MonadHorn s, MonadIO s) => Environment -> Id -> RSchema -> Explorer s RType
symbolType env x (Monotype t@(ScalarT b _))
    | isLiteral x = return t -- x is a literal of a primitive type, it's type is precise
    | isJust (lookupConstructor x env) = return t -- x is a constructor, it's type is precise 
    | otherwise = return $ ScalarT b (varRefinement x (toSort b)) -- x is a scalar variable or monomorphic scalar constant, use _v = x
symbolType env _ sch = freshInstance sch
  where
    freshInstance sch = if arity (toMonotype sch) == 0
      then instantiate env sch False [] -- Nullary polymorphic function: it is safe to instantiate it with bottom refinements, since nothing can force the refinements to be weaker
      else instantiate env sch True []
  
-- | Perform an exploration, and once it succeeds, do not backtrack it  
cut :: (MonadHorn s, MonadIO s) => Explorer s a -> Explorer s a
cut = id

-- | Synthesize auxiliary goals accumulated in @auxGoals@ and store the result in @solvedAuxGoals@
generateAuxGoals :: (MonadHorn s, MonadIO s) => Explorer s ()
generateAuxGoals = do
  goals <- use auxGoals
  writeLog 3 $ text "Auxiliary goals are:" $+$ vsep (map pretty goals)
  case goals of
    [] -> return ()
    (g : gs) -> do
        auxGoals .= gs
        writeLog 2 $ text "PICK AUXILIARY GOAL" <+> pretty g
        Reconstructor reconstructTopLevel _ <- asks . view $ _3
        p <- reconstructTopLevel $ g {
            gEnvironment = (gEnvironment g){
              _arguments = Map.empty
            }
          }
        solvedAuxGoals %= Map.insert (gName g) (etaContract p)
        generateAuxGoals
  where
    etaContract p = case etaContract' [] (content p) of
                      Nothing -> p
                      Just f -> Program f (typeOf p)
    etaContract' [] (PFix _ p)                                               = etaContract' [] (content p)
    etaContract' binders (PFun x p)                                          = etaContract' (x:binders) (content p)
    etaContract' (x:binders) (PApp pFun (Program (PSymbol y) _)) | x == y    =  etaContract' binders (content pFun)
    etaContract' [] f@(PSymbol _)                                            = Just f
    etaContract' binders p                                                   = Nothing

writeLog level msg = do
  maxLevel <- asks . view $ _1 . explorerLogLevel
  if level <= maxLevel then traceShow (plain msg) $ return () else return ()

-- Succinct type operations
distFromNode :: (MonadHorn s, MonadIO s) => SuccinctType -> Environment -> Explorer s Environment
distFromNode sty env = distFromNodeHelper (env ^. graphFromGoal) Set.empty [sty] env
  where
    distFromNodeHelper g visited toVisit env' = case toVisit of
      [] -> return env'
      curr:xs -> if Set.member curr visited
        then distFromNodeHelper g visited xs env'
        else do
          let children = HashMap.toList (HashMap.lookupDefault HashMap.empty curr g)
          let currMt = HashMap.lookupDefault (Metadata 0 (0 :: Double)) curr (env' ^. graphMetadata)
          newEnv <- foldrM (uncurry $ \t s e -> incMt currMt s >>= \nt -> return $ (graphMetadata %~ HashMap.insertWith min t nt) e) env' children
          distFromNodeHelper g (Set.insert curr visited) (xs ++ (fst $ unzip children)) newEnv

    incMt mt edgeSet = do
      me <- maxEdge edgeSet
      return $ Metadata ((mt ^. distFromGoal) + 1) ((mt ^. mWeight) + me)

    maxEdge :: (MonadHorn s, MonadIO s) => Set SuccinctEdge -> Explorer s Double
    maxEdge edgeSet = do
      ws <- liftIO $ getGraphWeights $ Set.toList $ Set.map getEdgeId edgeSet
      return $ minimum ws

addSuccinctSymbol :: (MonadHorn s, MonadIO s) => Id -> RSchema -> Environment -> Explorer s Environment
addSuccinctSymbol name t env = do
  newt <- instantiateWithoutConstraint env t True []
  tass <- use (typingState . typeAssignment)
  let succinctTy = toSuccinctType $ typeSubstitute tass newt
  return $ (succinctSymbols %~ HashMap.insert name succinctTy) env

addSuccinctEdge :: (MonadHorn s, MonadIO s) => Id -> RSchema -> Environment -> Explorer s Environment
addSuccinctEdge name t env = do
  -- let newt = toMonotype t
  newt <- instantiateWithoutConstraint env (t) True []
  tass <- use (typingState . typeAssignment)
  let succinctTy = getSuccinctTy $ typeSubstitute tass newt
  writeLog 2 $ text "ADD" <+> text name <+> text ":" <+> pretty succinctTy <+> text "for" <+> pretty t
  case newt of 
    (LetT id tDef tBody) -> do
      env' <- addSuccinctEdge id (Monotype tDef) env
      addSuccinctEdge name (Monotype tBody) env'
    _ -> do
      let env' = addEdgeForSymbol name succinctTy env
      let goalTy = outOfSuccinctAll $ lastSuccinctType (HashMap.lookupDefault SuccinctAny "__goal__" (env' ^. succinctSymbols))
      let starters = Set.toList $ Set.filter (\typ -> isSuccinctInhabited typ || isSuccinctFunction typ || hasSuccinctAny typ) (allSuccinctNodes env')
      let reachableSet = getReachableNodes (env' ^. succinctGraphRev) starters
      let graphEnv = env' { _succinctGraph = pruneGraphByReachability (env' ^. succinctGraph) reachableSet }
      let subgraphNodes = if goalTy == SuccinctAny then allSuccinctNodes graphEnv else reachableGraphFromNode graphEnv goalTy
      return $ graphEnv { _graphFromGoal = pruneGraphByReachability (graphEnv ^. succinctGraph) subgraphNodes }    
  where
    getSuccinctTy tt = case toSuccinctType tt of
      SuccinctAll vars ty -> SuccinctAll vars (refineSuccinctDatatype name ty env)
      ty -> refineSuccinctDatatype name ty env

-- | add constructor and measure infos for a datatype
refineSuccinctDatatype :: Id -> SuccinctType -> Environment -> SuccinctType
refineSuccinctDatatype name sty env = case sty of
  SuccinctDatatype outerId ids tys cons measures -> let
    consMap = Set.foldr (\(id,_) accMap -> foldr (\c acc -> Map.insert c id acc) accMap (case (Map.lookup id (env ^. datatypes)) of
      Just dt -> if length (dt ^. constructors) > 1 then dt ^. constructors else []
      Nothing -> [])) Map.empty ids
    in if Map.member name consMap
      then SuccinctDatatype outerId ids tys (Map.singleton (fromJust (Map.lookup name consMap)) name) measures
      else SuccinctDatatype outerId ids tys cons measures
  SuccinctFunction paramCnt params ret -> SuccinctFunction paramCnt params (refineSuccinctDatatype name ret env)
  ty' -> ty'


-- termScore env p = 0
termScore :: Environment -> SProgram -> IO ProgramRank
termScore env prog@(Program p (sty, rty, _)) = do
    ws <- getGraphWeights $ Set.toList $ symbolsOf prog
    let paramSymCnt = Set.size $ symbolsOf prog `Set.intersection` Map.keysSet (env ^. arguments)
    let w = (fromIntegral paramSymCnt) * 4000 + sum (map ((-) (fromIntegral maxCnt)) ws)
    return $ ProgramRank (fromIntegral maxCnt - holes) w
    -- else 1.0 / (fromIntegral holes) +
    --   1.0 / (fromIntegral $ greatestHoleType 0 prog) + 
    --   1.0 / (fromIntegral wholes)) + 
    --   -- if (d /= 0) then 100.0 / (fromIntegral d) else 100.0 + 
    --   100.0 / (fromIntegral size) + 
    --   2 * (fromIntegral $ Set.size vars) + 
      -- (fromIntegral $ Set.size consts)
  where
    holes = countHole prog
    size = termSize prog
    wholes = Set.foldr (\t accw -> accw + sizeof t) 0 $ holeTypes prog
    consts = Set.filter (\name -> isConstant name env && Map.member name (symbolsOfArity 0 env)) (symbolsOf prog)
    vars = (symbolsOf prog) `Set.difference` consts
    -- d = depth prog
    greatestHoleType maxSize (Program p (sty, rty,_)) = case p of
      PApp fun arg -> max (greatestHoleType maxSize fun) (greatestHoleType maxSize arg)
      PHole -> max maxSize (sizeof sty)
      _ -> maxSize