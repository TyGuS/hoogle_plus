{-# LANGUAGE TemplateHaskell, FlexibleContexts, TupleSections, StandaloneDeriving #-}

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
-- import Synquid.ListMonad

import Data.Maybe
import Data.List
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
-- import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
-- import Control.Monad.List
import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative hiding (empty)
import Control.Lens
import Debug.Trace

{- Interface -}

-- | Choices for the type of terminating fixpoint operator
data FixpointStrategy =
    DisableFixpoint   -- ^ Do not use fixpoint
  | FirstArgument     -- ^ Fixpoint decreases the first well-founded argument
  | AllArguments      -- ^ Fixpoint decreases the lexicographical tuple of all well-founded argument in declaration order
  | Nonterminating    -- ^ Fixpoint without termination check

-- | Choices for the order of e-term enumeration
data PickSymbolStrategy = PickDepthFirst | PickInterleave

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
  _buildGraph :: Bool
} 

makeLenses ''ExplorerParams

type Requirements = Map Id [RType]
type ProgramQueue = MaxPQueue Double (SProgram, ExplorerState, [Constraint])

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
  -- _triedTerms :: Set SProgram                      -- ^ Used terms to avoid duplicate term checking for several match cases
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
runExplorer :: MonadHorn s => ExplorerParams -> TypingParams -> Reconstructor s -> TypingState -> Explorer s a -> s (Either ErrorMessage a)
runExplorer eParams tParams topLevel initTS go = do
  (ress, (PersistentState _ _ errs)) <- runStateT (observeManyT 1 $ runReaderT (evalStateT go initExplorerState) (eParams, tParams, topLevel)) (PersistentState Map.empty Map.empty [])
  case ress of
    [] -> return $ Left $ head errs
    res:_ -> return $ Right res
  where
    initExplorerState = ExplorerState initTS [] Map.empty Map.empty Map.empty Map.empty PQ.empty

-- | 'generateI' @env t@ : explore all terms that have refined type @t@ in environment @env@
-- (top-down phase of bidirectional typechecking)
generateI :: MonadHorn s => Environment -> RType -> Bool -> Explorer s RProgram
generateI env t@(FunctionT x tArg tRes) isElseBranch = do
  let ctx = \p -> Program (PFun x p) t
  useSucc <- asks . view $ _1 . buildGraph
  env' <- if useSucc then addSuccinctSymbol x (Monotype tArg) env else return env
  pBody <- inContext ctx $ generateI (unfoldAllVariables $ addVariable x tArg $ env') tRes False
  return $ ctx pBody
generateI env t@(ScalarT _ _) isElseBranch = do
  maEnabled <- asks . view $ _1 . abduceScrutinees -- Is match abduction enabled?
  d <- asks . view $ _1 . matchDepth
  maPossible <- runInSolver $ hasPotentialScrutinees env -- Are there any potential scrutinees in scope?
  if maEnabled && d > 0 && maPossible then generateMaybeMatchIf env t isElseBranch else generateMaybeIf env t isElseBranch           

-- | Generate a possibly conditional term type @t@, depending on whether a condition is abduced
generateMaybeIf :: MonadHorn s => Environment -> RType -> Bool -> Explorer s RProgram
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
optionalInPartial :: MonadHorn s => RType -> Explorer s RProgram -> Explorer s RProgram
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
      caseEnv <- if useSucc then foldM (\e (name, ty) -> addSuccinctSymbol name (Monotype (if Set.size scrutineeSyms == 1 then refineScalarType env ty else ty)) e) env' syms else return env'

      ifte (do -- Try to find a vacuousness condition:
              deadUnknown <- Unknown Map.empty <$> freshId "C"
              addConstraint $ WellFormedCond env deadUnknown
              err <- inContext (\p -> Program (PMatch pScrutinee [Case consName binders p]) t) $ generateError (addAssumption deadUnknown caseEnv)
              deadValuation <- conjunction <$> currentValuation deadUnknown
              ifte (generateError (addAssumption deadValuation env)) (const mzero) (return ()) -- The error must be possible only in this case
              return (err, deadValuation, unknownName deadUnknown)) 
            (\(err, deadCond, deadUnknown) -> return $ (Case consName binders err, deadCond, deadUnknown))
            (do
              pCaseExpr <- local (over (_1 . matchDepth) (-1 +)) 
                            $ inContext (\p -> Program (PMatch pScrutinee [Case consName binders p]) t)
                            $ generateI caseEnv t False
              return $ (Case consName binders pCaseExpr, ftrue, dontCare))

-- | Generate the @consName@ case of a match term with scrutinee variable @scrName@ and scrutinee type @scrType@
generateCase :: MonadHorn s => Environment -> Formula -> RProgram -> RType -> Id -> Explorer s (Case RType, Explorer s ())
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
      caseEnv <- if useSucc then foldM (\e (name, ty) -> addSuccinctSymbol name (Monotype (if Set.size scrutineeSyms == 1 then refineScalarType env ty else ty)) e) env' syms else return env'
      pCaseExpr <- optionalInPartial t $ local (over (_1 . matchDepth) (-1 +))
                                       $ inContext (\p -> Program (PMatch pScrutinee [Case consName binders p]) t)
                                       $ generateError caseEnv `mplus` generateI caseEnv t False
            
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
generateMaybeMatchIf :: MonadHorn s => Environment -> RType -> Bool -> Explorer s RProgram
generateMaybeMatchIf env t isElseBranch = (generateOneBranch >>= generateOtherBranches) `mplus` (generateMatch env t) -- might need to backtrack a successful match due to match depth limitation
  where
    -- | Guess an E-term and abduce a condition and a match-condition for it
    generateOneBranch = do
      matchUnknown <- Unknown Map.empty <$> freshId "M"
      addConstraint $ WellFormedMatchCond env matchUnknown
      condUnknown <- Unknown Map.empty <$> freshId "C"
      addConstraint $ WellFormedCond env condUnknown
      -- [TODO] cut here
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
        
    generateEOrError env typ = generateError env `mplus` generateE env typ True isElseBranch False

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
      -- [TODO] cut here
      pBaseCase' <- cut $ inContext (\p -> Program (PMatch pScrutinee [Case c [] p]) t) $
                            generateMatchesFor (addAssumption matchCond env') rest pBaseCase t
                            
      let genOtherCases previousCases ctors = 
            case ctors of
              [] -> return $ Program (PMatch pScrutinee previousCases) t
              (ctor:rest) -> do
                -- [TODO] cut here
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
  -- _triedTerms = old ^. triedTerms
  _symbolUseCount = Map.unionWith max (old ^. symbolUseCount) (new ^. symbolUseCount)
}

walkThrough :: MonadHorn s => Environment -> ProgramQueue -> Explorer s (Maybe (SProgram, ExplorerState), ProgramQueue)
walkThrough env pq =
  if PQ.size pq == 0
    then return (Nothing, PQ.empty)
    else do 
      let (score,(p, pes, constraints)) = PQ.findMax pq
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
        let styp = toSuccinctType env (rty')
        let subst = Set.foldr (\t acc -> Map.insert t SuccinctAny acc) Map.empty (extractSuccinctTyVars styp `Set.difference` Set.fromList (env ^. boundTypeVars))
        let succinctTy = outOfSuccinctAll $ succinctTypeSubstitute subst styp
        return (succinctTy, rty', typ)
      PApp fun arg -> if hasHole fun then typeOfFirstHole fun else typeOfFirstHole arg
      _ -> error "we are not handling none-application now"

    fillAndEnqueue p pq' = do
      d <- asks . view $ _1 . eGuessDepth
      es' <- get
      writeLog 2 $ text "*******************Filling holes in" <+> pretty (toRProgram p)
      holeTy <- typeOfFirstHole p
      candidates <- uncurry3 (termWithType env) holeTy
      currSt <- get
      put $ keepIdCount currSt es'
      filteredCands <- mapM (\(prog, progES) -> do
        currSt <- get
        put $ keepIdCount currSt progES
        (p', constraints') <- fillFirstHole env p prog
        fes <- get
        put (keepIdCount fes es') >> if depth p' <= d then return (Just (p', fes, constraints')) else return Nothing
        ) candidates --if hasHole p then PQ.insertBehind  prog accQ else PQ.insertBehind 1 prog accQ
      walkThrough env $ foldl (\accQ prog@(p,_,_) -> PQ.insertBehind (termScore env p) prog accQ) pq' $ map fromJust $ filter isJust filteredCands

termWithType :: MonadHorn s => Environment -> SuccinctType -> RType -> RType -> Explorer s [(SProgram, ExplorerState)]
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
      let styp = outOfSuccinctAll $ toSuccinctType env rty
      writeLog 2 $ text "Looking for succinct type" <+> text (succinct2str styp)
      let ids = Set.toList $ Set.unions $ HashMap.elems $ findDstNodesInGraph env sty
      useCounts <- use symbolUseCount
      let sortedIds = if isSuccinctFunction sty
                      then sortBy (mappedCompare (\(SuccinctEdge x _ _) -> (Set.member x (env ^. constants), (Map.findWithDefault 0 x useCounts)))) ids
                      else sortBy (mappedCompare (\(SuccinctEdge x _ _) -> (not $ Set.member x (env ^. constants), (Map.findWithDefault 0 x useCounts)))) ids    

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
                writeLog 2 $ text "Trying" <+> text id
                let succinctTy = outOfSuccinctAll (toSuccinctType env (t))
                let p = Program (PSymbol id) (succinctTy, t, typ)
                addConstraint $ Subtype env t rty False "" -- Add subtyping check, unless it's a function type and incremental checking is diasbled
                when (arity rty > 0) (addConstraint $ Subtype env t rty True "") -- Add consistency constraint for function types
                es' <- get
                put $ keepIdCount es' es
                return (p, es')
              else do
                d' <- asks . view $ _1 . eGuessDepth
                tFun <- buildFunctionType pc rty
                let succinctTy = outOfSuccinctAll (toSuccinctType env (t))
                let p = Program (PSymbol id) (succinctTy, t, tFun)
                writeLog 2 $ text "Trying" <+> text id
                addConstraint $ Subtype env t tFun False "" -- Add subtyping check, unless it's a function type and incremental checking is diasbled
                when (arity tFun > 0) (addConstraint $ Subtype env t tFun True "") -- Add consistency constraint for function types
                let p' = buildApp pc (Program (PSymbol id) (succinctTy,t, tFun))
                es' <- get
                put $ keepIdCount es' es
                return (p', es')
        ) $ filter (\(SuccinctEdge id _ _) -> id /= "__goal__" && id /= "") sortedIds
  where
    buildApp 0 p = p
    buildApp paramCnt p@(Program _ (styp,rtyp,typ)) = case styp of
      SuccinctFunction _ argSet retTy -> let
        FunctionT x tArg tRet = rtyp
        FunctionT x' tArg' tRet' = typ
        arg = outOfSuccinctAll $ toSuccinctType env (tArg)
        args = if paramCnt > Set.size argSet || paramCnt == 1 then Set.delete arg argSet else argSet
        in buildApp (paramCnt - 1) (Program (PApp p (Program PHole (arg, tArg, tArg'))) ((if paramCnt == 1 then retTy else SuccinctFunction (paramCnt-1) args retTy), tRet, tRet'))
      _ -> p -- buildApp args (Program (PApp p (Program PHole arg)) (styp, rtyp))

    buildFunctionType 0 typ = return typ
    buildFunctionType paramCnt typ = do
      x <- freshId "X"
      buildFunctionType (paramCnt - 1) (FunctionT x AnyT typ)

fillFirstHole :: MonadHorn s => Environment -> SProgram -> SProgram -> Explorer s (SProgram, [Constraint])
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
  PApp fun arg -> Program (PApp (toSProgram env fun) (toSProgram env arg)) (outOfSuccinctAll (toSuccinctType env (typ)),typ, typ)
  PSymbol id -> Program (PSymbol id) (outOfSuccinctAll (toSuccinctType env (typ)), typ, typ)
  PHole -> Program PHole (outOfSuccinctAll (toSuccinctType env (typ)), typ, typ)

checkArguments :: MonadHorn s => Environment -> RProgram -> Explorer s RProgram
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

-- refineProgram :: MonadHorn s => Environment -> SProgram -> Explorer s RProgram
-- refineProgram env (Program p (styp,rtyp)) = case p of
--   PSymbol id -> case lookupSymbol id (arity rtyp) env of
--     Nothing -> error ("symbol " ++ id ++ "not in the scope")
--     Just sch -> do
--       t <- symbolType env id sch -- instantiate the type with fresh names
--       incremental <- asks . view $ _1 . incrementalChecking -- Is incremental type checking of E-terms enabled?
--       consistency <- asks . view $ _1 . consistencyChecking -- Is consistency checking enabled?
--       -- when (incremental || arity rtyp == 0) (addConstraint $ Subtype env t rtyp False "")
--       -- when (consistency && arity rtyp > 0) (addConstraint $ Subtype env t rtyp True "")
--       return (Program (PSymbol id) t)
--         -- else return $ Program (PSymbol id) rtyp
--   PApp fun arg -> do
--     fun' <- refineProgram env fun
--     arg' <- refineProgram env arg
--     let FunctionT x tArg tRet = typeOf fun'
--     -- let tRet' = if hasAny (typeOf arg') then tRet else appType env arg' x tRet
--     let tRet' = appType env arg' x tRet
--     let (_, funTy) = typeOf fun
--     -- incremental <- asks . view $ _1 . incrementalChecking -- Is incremental type checking of E-terms enabled?
--     -- consistency <- asks . view $ _1 . consistencyChecking -- Is consistency checking enabled?
--     addConstraint $ Subtype env (typeOf arg') tArg False ""
--     -- when (consistency && arity funTy > 0) (addConstraint $ Subtype env (typeOf fun') funTy True "")
--     return $ Program (PApp fun' arg') tRet'
--   _ -> return (Program PHole AnyT)

initProgramQueue :: MonadHorn s => Environment -> RType -> Explorer s ProgramQueue
initProgramQueue env typ = do
  tass <- use (typingState . typeAssignment)
  let typ' = typeSubstitute tass typ
  writeLog 2 $ text "Looking for type" <+> pretty typ'
  let styp = toSuccinctType env (typ')
  let subst = Set.foldr (\t acc -> Map.insert t SuccinctAny acc) Map.empty (extractSuccinctTyVars styp `Set.difference` Set.fromList (env ^. boundTypeVars))
  let succinctTy = outOfSuccinctAll $ succinctTypeSubstitute subst styp
  let p = Program PHole (succinctTy, typ, AnyT)
  -- ts <- use typingState
  es <- get
  let pq = PQ.singleton (termScore env p) (p, es, [])
  return pq

generateEWithGraph :: MonadHorn s => Environment -> ProgramQueue -> RType -> Bool -> Bool -> Explorer s RProgram
generateEWithGraph env pq typ isThenBranch isElseBranch = do
  -- ts <- use typingState
  es <- get
  res <- walkThrough env pq
  case res of
    (Nothing, _) -> mzero
    (Just (p, pes), newPQ) -> do
      -- typingState .= pts 
      put $ keepIdCount es pes
      let refinedP = toRProgram p
      writeLog 2 $ text "Checking program" <+> pretty refinedP
      let p' = refinedP
      -- p' <- if isElseBranch then checkArguments env refinedP else return refinedP
      ifte (checkE env typ p')
        (\() -> when isThenBranch (termQueueState .= newPQ) >> return p')
        -- (typingState .= ts >> generateEWithGraph env newPQ typ isThenBranch isElseBranch)
        (do
          -- triedTerms %= Set.insert p
          currSt <- get
          put $ keepIdCount currSt es
          generateEWithGraph env newPQ typ isThenBranch isElseBranch)

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
generateE :: MonadHorn s => Environment -> RType -> Bool -> Bool -> Bool -> Explorer s RProgram
generateE env typ isThenBranch isElseBranch isMatchScrutinee = do
  useFilter <- asks . view $ _1 . useSuccinct
  d <- asks . view $ _1 . eGuessDepth
  pq <- if isElseBranch 
    then do
      q <- use termQueueState
      -- ts <- use typingState
      es <- get
      resQ <- mapM (\(k, (prog, pes, c)) -> return $ Just (k, (prog, mergeExplorerState env es pes, c))) (PQ.toList q)
      return $ PQ.fromList $ map fromJust $ filter isJust resQ
    else initProgramQueue env typ
  prog@(Program pTerm pTyp) <- if useFilter && (not isMatchScrutinee) then generateEWithGraph env pq typ isThenBranch isElseBranch else generateEUpTo env typ d
  -- (Program pTerm pTyp) <- generateEUpTo env typ d
  runInSolver $ isFinal .= True >> solveTypeConstraints >> isFinal .= False  -- Final type checking pass that eliminates all free type variables
  newGoals <- uses auxGoals (map gName)                                      -- Remember unsolved auxiliary goals
  generateAuxGoals                                                           -- Solve auxiliary goals
  pTyp' <- runInSolver $ currentAssignment pTyp                              -- Finalize the type of the synthesized term
  addLambdaLets pTyp' (Program pTerm pTyp') newGoals                         -- Check if some of the auxiliary goal solutions are large and have to be lifted into lambda-lets
  where
    addLambdaLets t body [] = return body
    addLambdaLets t body (g:gs) = do
      pAux <- uses solvedAuxGoals (Map.! g)
      if programNodeCount pAux > 5
        then addLambdaLets t (Program (PLet g uHole body) t) gs
        else addLambdaLets t body gs
        
-- | 'generateEUpTo' @env typ d@ : explore all applications of type shape @shape typ@ in environment @env@ of depth up to @d@
generateEUpTo :: MonadHorn s => Environment -> RType -> Int -> Explorer s RProgram
generateEUpTo env typ d = msum $ map (generateEAt env typ) [0..d]

-- | 'generateEAt' @env typ d@ : explore all applications of type shape @shape typ@ in environment @env@ of depth exactly to @d@
generateEAt :: MonadHorn s => Environment -> RType -> Int -> Explorer s RProgram
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
checkE :: MonadHorn s => Environment -> RType -> RProgram -> Explorer s ()
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

enumerateAt :: MonadHorn s => Environment -> RType -> Int -> Explorer s RProgram
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
      let styp = toSuccinctType env ((if arity typ' == 0 then typ' else lastType typ'))
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
generateError :: MonadHorn s => Environment -> Explorer s RProgram
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
toVar :: MonadHorn s => Environment -> RProgram -> Explorer s (Environment, Formula)
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
getMemo :: MonadHorn s => Explorer s Memo
getMemo = lift . lift . lift $ use termMemo

-- | Set memoization store
putMemo :: MonadHorn s => Memo -> Explorer s ()
putMemo memo = lift . lift . lift $ termMemo .= memo

-- getPartials :: MonadHorn s => Explorer s PartialMemo
-- getPartials = lift . lift . lift $ use partialFailures

-- putPartials :: MonadHorn s => PartialMemo -> Explorer s ()
-- putPartials partials = lift . lift . lift $ partialFailures .= partials

throwErrorWithDescription :: MonadHorn s => Doc -> Explorer s a   
throwErrorWithDescription msg = do
  pos <- asks . view $ _1 . sourcePos
  throwError $ ErrorMessage TypeError pos msg

-- | Record type error and backtrack
throwError :: MonadHorn s => ErrorMessage -> Explorer s a  
throwError e = do
  writeLog 2 $ text "TYPE ERROR:" <+> plain (emDescription e)
  lift . lift . lift $ typeErrors %= (e :)
  mzero
  
-- | Impose typing constraint @c@ on the programs
addConstraint c = do
  writeLog 3 $ text "Adding constraint" <+> pretty c
  typingState %= addTypingConstraint c

-- | Embed a type-constraint checker computation @f@ in the explorer; on type error, record the error and backtrack
runInSolver :: MonadHorn s => TCSolver s a -> Explorer s a
runInSolver f = do
  tParams <- asks . view $ _2
  tState <- use typingState  
  res <- lift . lift . lift . lift $ runTCSolver tParams tState f
  case res of
    Left err -> throwError err
    Right (res, st) -> do
      typingState .= st
      return res

freshId :: MonadHorn s => String -> Explorer s String
freshId = runInSolver . TCSolver.freshId

freshVar :: MonadHorn s => Environment -> String -> Explorer s String
freshVar env prefix = runInSolver $ TCSolver.freshVar env prefix

-- | Return the current valuation of @u@;
-- in case there are multiple solutions,
-- order them from weakest to strongest in terms of valuation of @u@ and split the computation
currentValuation :: MonadHorn s => Formula -> Explorer s Valuation
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
instantiate :: MonadHorn s => Environment -> RSchema -> Bool -> [Id] -> Explorer s RType
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
instantiateWithoutConstraint :: MonadHorn s => Environment -> RSchema -> Bool -> [Id] -> Explorer s RType
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
    go subst pSubst argNames (FunctionT x tArg tRes) = do
      x' <- case argNames of
              [] -> freshVar env "x"
              (argName : _) -> return argName
      liftM2 (FunctionT x') (go subst pSubst [] tArg) (go subst pSubst (drop 1 argNames) (renameVar (isBoundTV subst) x x' tArg tRes))
    go subst pSubst _ t = return $ typeSubstitutePred pSubst . typeSubstitute subst $ t
    isBoundTV subst a = (a `Map.member` subst) || (a `elem` (env ^. boundTypeVars))
    

-- | 'symbolType' @env x sch@: precise type of symbol @x@, which has a schema @sch@ in environment @env@;
-- if @x@ is a scalar variable, use "_v == x" as refinement;
-- if @sch@ is a polytype, return a fresh instance
symbolType :: MonadHorn s => Environment -> Id -> RSchema -> Explorer s RType
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
cut :: MonadHorn s => Explorer s a -> Explorer s a
cut = once

-- | Synthesize auxiliary goals accumulated in @auxGoals@ and store the result in @solvedAuxGoals@
generateAuxGoals :: MonadHorn s => Explorer s ()
generateAuxGoals = do
  goals <- use auxGoals
  writeLog 3 $ text "Auxiliary goals are:" $+$ vsep (map pretty goals)
  case goals of
    [] -> return ()
    (g : gs) -> do
        auxGoals .= gs
        writeLog 2 $ text "PICK AUXILIARY GOAL" <+> pretty g
        Reconstructor reconstructTopLevel _ <- asks . view $ _3
        p <- reconstructTopLevel g
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
baseToSuccinctType :: Environment -> BaseType Formula -> SuccinctType
baseToSuccinctType env typ@(DatatypeT id ts _) = if "_" == id 
  then SuccinctAny --SuccinctAll (Set.singleton "_") (SuccinctScalar (TypeVarT Map.empty "_"))
  else SuccinctDatatype (id, length ts) resIds resTys Map.empty (usedMeasures)
  where
    fmls = extractBaseRefinements typ
    measureNames = Map.keysSet $ allMeasuresOf id env
    -- symbolNames = Map.keysSet $ allSymbols env
    usedMeasures = Set.unions $ map (fmlMeasure measureNames) (Set.toList fmls)
    -- usedVars = Set.unions $ map (fmlVars symbolNames) (Set.toList fmls)
    mergeDt t (accIds, accTys) = case outOfSuccinctAll (toSuccinctType env t) of
      SuccinctDatatype id' ids tys _ _ -> (Set.insert id' (ids `Set.union` accIds), tys `Set.union` accTys)
      ty                         -> (accIds, Set.singleton ty `Set.union` accTys)
    (resIds, resTys) = foldr mergeDt (Set.empty, Set.empty) ts
baseToSuccinctType _ t = SuccinctScalar t

toSuccinctType :: Environment -> RType -> SuccinctType
toSuccinctType env t@(ScalarT bt _) = let 
  vars = extractSTyVars t `Set.difference` Set.fromList (env ^. boundTypeVars)
  ty = case baseToSuccinctType env bt of
    SuccinctDatatype (id, l) ids tys cons measures -> let
      fmls = extractRefinements t
      measureNames = Map.keysSet $ allMeasuresOf id env
      -- symbolNames = Map.keysSet $ allSymbols env
      usedMeasures = Set.unions $ map (fmlMeasure measureNames) (Set.toList fmls)
      -- usedVars = Set.unions $ map (fmlVars symbolNames) (Set.toList fmls)
      -- usedMeasures = Set.empty
      in SuccinctDatatype (id,l) ids tys cons (measures `Set.union` usedMeasures)
    sbt -> sbt
  in if Set.size vars == 0 then ty else simplifySuccinctType $ SuccinctAll vars ty
toSuccinctType env t@(FunctionT _ param ret) = let
  vars = extractSTyVars t `Set.difference` Set.fromList (env ^. boundTypeVars)
  ty = case outOfSuccinctAll (toSuccinctType env ret) of
    SuccinctFunction paramCnt paramSet retTy -> SuccinctFunction (paramCnt+1) (Set.insert (toSuccinctType env param) paramSet) retTy
    ty'                                      -> SuccinctFunction 1 (Set.singleton (toSuccinctType env param)) ty'
  in if Set.size vars == 0 then ty else simplifySuccinctType $ SuccinctAll vars ty
-- toSuccinctType env t@(LetT id varty bodyty) = let
--   vars = extractSTyVars t
--   ty = SuccinctLet id (toSuccinctType env varty) (toSuccinctType env bodyty)
--   in if Set.size vars == 0 then ty else simplifySuccinctType $ SuccinctAll vars ty
toSuccinctType env t@(LetT id varty bodyty) = let
  vars = extractSTyVars t `Set.difference` Set.fromList (env ^. boundTypeVars)
  ty = toSuccinctType env bodyty
  in if Set.size vars == 0 then ty else simplifySuccinctType $ SuccinctAll vars ty
toSuccinctType _ AnyT = SuccinctAny

addEdgeForSymbol :: Id -> SuccinctType -> Environment -> Environment
addEdgeForSymbol name succinctTy env = let
  envWithSelf = addEdge name succinctTy $ (succinctSymbols %~ HashMap.insert name succinctTy) env
  iteratedEnv = iteration env envWithSelf
  goalTy = lastSuccinctType (HashMap.lookupDefault SuccinctAny "__goal__" (iteratedEnv ^. succinctSymbols))
  diffTys = Set.filter isSuccinctConcrete $ ((allSuccinctNodes iteratedEnv)) `Set.difference` ((allSuccinctNodes env))
  -- finalEnv = iteratedEnv
  finalEnv = foldr (\sty accEnv -> addFromEdges sty $ addToEdges sty accEnv) iteratedEnv diffTys
  subgraphNodes = if goalTy == SuccinctAny then allSuccinctNodes finalEnv else reachableGraphFromGoal finalEnv
  reachableSet = (getReachableNodes iteratedEnv) `Set.intersection` subgraphNodes
  prunedEnv = finalEnv { _graphFromGoal = pruneGraphByReachability (finalEnv ^. succinctGraph) reachableSet }
  in prunedEnv
  where
    iteration oldEnv newEnv = let
      diffTys = Set.filter isSuccinctConcrete $ ((allSuccinctNodes newEnv)) `Set.difference` ((allSuccinctNodes oldEnv))
      in if Set.size diffTys == 0
        then newEnv
        else let
          env' = HashMap.foldrWithKey (\name ty accEnv -> addPolyEdge name ty accEnv (Set.filter isSuccinctConcrete (allSuccinctNodes newEnv))) newEnv (HashMap.filter isSuccinctAll (newEnv ^. succinctSymbols))
          -- goal = lastSuccinctType (HashMap.lookupDefault SuccinctAny "__goal__" (env' ^. succinctSymbols))
          -- subgraphNodes = if goal == SuccinctAny then allSuccinctNodes env' else reachableGraphFromGoal env'
          in iteration newEnv env'
    measureNames = case succinctTy of
      SuccinctDatatype (id,_) _ _ _ _ -> Map.keysSet $ allMeasuresOf id env
      _ -> Set.empty
    addToEdges typ oldEnv = let
      filter_fun k v = (isStrongerThan k typ) && (not (isSuccinctComposite k)) && k /= typ
      candidates = HashMap.keys $ HashMap.filterWithKey filter_fun (oldEnv ^. succinctGraph)
      in foldr (\t accEnv -> let 
        revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union t (Set.singleton typ)) accEnv
        in (succinctGraph %~ HashMap.insertWith mergeMapOfSet typ (HashMap.singleton t (Set.singleton (SuccinctEdge {_symbolId = "", _params = 0, _weight = HashMap.empty})))) revEnv
        ) oldEnv candidates
    addFromEdges typ oldEnv = let
      filter_fun k v = (isStrongerThan typ k) && (not (isSuccinctComposite k)) && k /= typ
      candidates = HashMap.keys $ HashMap.filterWithKey filter_fun (oldEnv ^. succinctGraph)
      in foldr (\t accEnv -> let 
        revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union typ (Set.singleton t)) accEnv
        in (succinctGraph %~ HashMap.insertWith mergeMapOfSet t (HashMap.singleton typ (Set.singleton (SuccinctEdge {_symbolId = "", _params = 0, _weight = HashMap.empty})))) revEnv
        ) oldEnv candidates

addSuccinctSymbol :: MonadHorn s => Id -> RSchema -> Environment -> Explorer s Environment
addSuccinctSymbol name t env = do
  newt <- instantiateWithoutConstraint env (t) True []
  tass <- use (typingState . typeAssignment)
  let succinctTy = getSuccinctTy (refineScalarType env (typeSubstitute tass newt))
  writeLog 2 $ text "ADD" <+> text name <+> text ":" <+> text (succinct2str succinctTy)
  case newt of 
    (LetT id tDef tBody) -> do
      env' <- addSuccinctSymbol id (Monotype tDef) env
      addSuccinctSymbol name (Monotype tBody) env'
    _ -> return $ addEdgeForSymbol name succinctTy env
  where
    getSuccinctTy tt = case toSuccinctType env tt of
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

-- addDestructors :: Id -> Set SuccinctType -> Environment -> Environment
-- addDestructors name destructors env = let
--   (resEnv, _) = Set.foldr (\ty (accEnv,idx) -> (addEdge (name++"_match_"++(show idx)) ty accEnv, idx+1)) (env, 0) destructors
--   (env', _) = Set.foldr (\ty (accEnv,idx) -> ((succinctSymbols %~ HashMap.insert (name++"_match_"++(show idx)) ty) accEnv, idx+1)) (resEnv, 0) destructors
--   in env'

-- datatypeEq env ty ty' = case (ty, ty') of
--   (SuccinctDatatype outerId ids tys cons _, SuccinctDatatype outerId' ids' tys' cons' _) -> ids == ids' && (Set.size ((extractSuccinctTyVars ty) `Set.difference` Set.fromList (env ^. boundTypeVars)) == Set.size ((extractSuccinctTyVars ty') `Set.difference` Set.fromList (env ^. boundTypeVars))) && cons == cons'
--   (SuccinctComposite tys, SuccinctComposite tys') -> foldr (\(x,y) acc -> (datatypeEq env x y) && acc) True (zip (Set.toList tys) (Set.toList tys'))
--   _ -> ty == ty'

-- anyToTypeVar :: MonadHorn s => SuccinctType -> Explorer s SuccinctType
-- anyToTypeVar (SuccinctFunction targ tret) = do
--   targ' <- mapM anyToTypeVar targ
--   tret' <- anyToTypeVar tret
--   return $ SuccinctFunction (Set.fromList targ') tret'
-- anyToTypeVar (SuccinctDatatype ids tys cons) = do
--   tys' <- mapM anyToTypeVar (Set.toList tys)
--   return $ SuccinctDatatype ids (Set.fromList tys') cons
-- anyToTypeVar (SuccinctComposite tys) = do
--   tys' <- mapM anyToTypeVar (Set.toList tys)
--   return $ SuccinctComposite (Set.fromList tys')
-- anyToTypeVar SuccinctAny = do
--   id <- freshId "A"
--   return $ SuccinctScalar (TypeVarT Map.empty id)
-- anyToTypeVar t = return t

-- addAnyEdge :: MonadHorn s => Environment -> Explorer s Environment
-- addAnyEdge env = do
--   mapM_ anyToTypeVar anyNodes
--   where
--     allNodes = allSuccinctNodes env
--     concreteNodes = Set.filter isSuccinctConcrete allNodes
--     anyNodes = Set.toList $ Set.filter hasSuccinctAny allNodes

addPolyEdge :: Id -> SuccinctType -> Environment -> Set SuccinctType -> Environment
addPolyEdge name (SuccinctAll idSet ty) env targets = 
  -- if all the type vars are bound in the env, treat it as none-all type
  if isAllBound 
    then addEdge name ty env 
    else case ty of 
      SuccinctFunction paramCnt pty rty -> let 
        fold_fun sty accEnv = let
          (unified, substitutions) = unifySuccinct rty sty (accEnv ^. boundTypeVars)
          pty' = Set.fromList $ map (\substitution -> Set.map (succinctTypeSubstitute substitution) pty) substitutions -- list of possible ptys
          in if unified 
            then Set.foldr (\ptySet acc -> let
              tyVars = foldr (\t set  -> set `Set.union` ((extractSuccinctTyVars t) `Set.difference` Set.fromList (accEnv ^. boundTypeVars))) Set.empty ptySet
              in if Set.size tyVars > 0
              then let 
                subst = Set.foldr (\tv macc -> Map.insert tv SuccinctAny macc) Map.empty tyVars
                ptySet' = Set.map (succinctTypeSubstitute subst) ptySet
                in addEdge name (SuccinctFunction paramCnt ptySet' sty) acc
              else addEdge name (SuccinctFunction paramCnt ptySet sty) acc
            ) accEnv pty'
            else accEnv
        in Set.foldr fold_fun env targets
      _                        -> let 
        fold_fun sty accEnv = let 
          (unified, substitutions) = unifySuccinct ty sty (accEnv ^. boundTypeVars)
          tys = Set.fromList $ map (\substitution -> succinctTypeSubstitute substitution ty) substitutions
          in if unified 
            then Set.foldr (\ty' acc -> let
              tyVars = (extractSuccinctTyVars ty') `Set.difference` Set.fromList (accEnv ^. boundTypeVars)
              in if Set.size tyVars > 0
                then let
                  subst = Set.foldr (\tv macc -> Map.insert tv SuccinctAny macc) Map.empty tyVars
                  substedTy = succinctTypeSubstitute subst ty'
                  revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union (SuccinctInhabited substedTy) (Set.singleton sty)) acc
                  in (succinctGraph %~ HashMap.insertWith mergeMapOfSet sty (HashMap.singleton (SuccinctInhabited substedTy) (Set.singleton (SuccinctEdge {_symbolId = name, _params = 0, _weight = HashMap.empty})))) revEnv
                else let
                  revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union (SuccinctInhabited ty') (Set.singleton sty)) acc
                  in (succinctGraph %~ HashMap.insertWith mergeMapOfSet sty (HashMap.singleton (SuccinctInhabited ty') (Set.singleton (SuccinctEdge {_symbolId = name, _params = 0, _weight = HashMap.empty})))) revEnv
            ) accEnv tys 
            else accEnv
        in Set.foldr fold_fun env targets
  where
    isAllBound = Set.foldr (\id acc -> (isBound env id) && acc) True idSet

addEdge :: Id -> SuccinctType -> Environment -> Environment
addEdge name (SuccinctFunction paramCnt argSet retTy) env = 
  let
    argTy = if Set.size argSet == 1 then Set.findMin argSet else SuccinctComposite argSet
    addedRevEnv = (succinctGraphRev %~ HashMap.insertWith Set.union argTy (Set.singleton retTy)) env
    addedRetEnv = (succinctGraph %~ HashMap.insertWith mergeMapOfSet retTy (HashMap.singleton argTy (Set.singleton (SuccinctEdge {_symbolId = name, _params = paramCnt, _weight = HashMap.empty})))) addedRevEnv
  in if Set.size argSet == 1
    then addedRetEnv
    else Set.foldr (\elem acc -> let revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union elem (Set.singleton argTy)) acc
      in (succinctGraph %~ HashMap.insertWith mergeMapOfSet argTy (HashMap.singleton elem (Set.singleton (SuccinctEdge {_symbolId = "", _params = 0, _weight = HashMap.empty})))) revEnv) addedRetEnv argSet
addEdge name typ@(SuccinctAll idSet ty) env = 
  let 
    polyEnv = addPolyEdge name typ env $ Set.filter isSuccinctConcrete (allSuccinctNodes env)
  in case ty of
    SuccinctFunction paramCnt pty rty -> if Set.null ((extractSuccinctTyVars rty) `Set.difference` Set.fromList (env ^. boundTypeVars))
      then let
        tyVars = foldr (\t set -> set `Set.union` ((extractSuccinctTyVars t) `Set.difference` Set.fromList (env ^. boundTypeVars))) Set.empty pty
        subst = Set.foldr (\tv macc -> Map.insert tv SuccinctAny macc) Map.empty tyVars
        substedTys = Set.map (succinctTypeSubstitute subst) pty
        in addEdge name (SuccinctFunction paramCnt substedTys rty) env
      else polyEnv
    _ -> polyEnv
addEdge name typ env = 
  let
    inhabitedEnvRev = (succinctGraphRev %~ HashMap.insertWith Set.union (SuccinctInhabited typ) (Set.singleton typ)) env
    inhabitedEnv = (succinctGraph %~ HashMap.insertWith mergeMapOfSet typ (HashMap.singleton (SuccinctInhabited typ) (Set.singleton (SuccinctEdge {_symbolId = name, _params = 0, _weight = HashMap.empty})))) inhabitedEnvRev
    in inhabitedEnv

isReachable :: Environment -> SuccinctType -> Bool
isReachable env typ = isReachableHelper (env ^. succinctGraph) Set.empty typ
  where
    isReachableHelper g visited typ' = case typ' of
      SuccinctInhabited _ -> True
      SuccinctAny -> True
      SuccinctComposite tys -> Set.foldr (\t acc -> acc && isReachableHelper g (Set.insert typ' visited) t) True tys
      _ -> HashMap.foldrWithKey (\i _ acc -> acc || isReachableHelper g (Set.insert typ' visited) i) False (if Set.member typ' visited then HashMap.empty else HashMap.lookupDefault HashMap.empty typ' g)

getReachableNodes :: Environment -> Set SuccinctType
getReachableNodes env = 
  getReachableNodesHelper (env ^. succinctGraphRev) Set.empty [] $ Set.toList $ Set.filter (\typ -> isSuccinctInhabited typ || isSuccinctFunction typ || typ == (SuccinctScalar BoolT) || hasSuccinctAny typ) (allSuccinctNodes env)
  where
    isCompositeReachable reachableSet typ = case typ of
      SuccinctComposite tySet -> Set.foldr (\b acc -> acc && (Set.member b reachableSet)) True tySet
      _ -> True
    getReachableNodesWithoutComposite g visited toVisit = case toVisit of
      [] -> visited
      curr:xs -> if Set.member curr visited
        then getReachableNodesWithoutComposite g visited xs
        else let newVisited = Set.insert curr visited 
          in getReachableNodesWithoutComposite g newVisited (xs ++ (Set.toList (Set.filter (isCompositeReachable newVisited) (HashMap.lookupDefault Set.empty curr g))))
    getReachableNodesHelper g visited waitingList toVisit = case toVisit of
      [] -> visited `Set.union` (getReachableNodesWithoutComposite g visited (filter (isCompositeReachable visited) waitingList))
      curr:xs -> if Set.member curr visited 
        then getReachableNodesHelper g visited waitingList xs
        else case curr of
          SuccinctComposite _ -> getReachableNodesHelper g visited (waitingList++[curr]) xs
          _ -> getReachableNodesHelper g (Set.insert curr visited) waitingList (xs ++ (Set.toList (HashMap.lookupDefault Set.empty curr g)))

assignWeights :: Environment -> Environment
assignWeights env = env

reachableGraphFromGoal :: Environment -> Set SuccinctType
reachableGraphFromGoal env = reachableGraphFromGoalHelper (env ^. succinctGraph) Set.empty startTys
  where
    goalTy = outOfSuccinctAll $ lastSuccinctType (HashMap.lookupDefault SuccinctAny "__goal__" (env ^. succinctSymbols))
    startTys = (SuccinctScalar BoolT):(Set.toList $ Set.filter (\t -> succinctAnyEq goalTy t) (allSuccinctNodes env))
    isCompositeReachable reachableSet typ = case typ of
      SuccinctComposite tySet -> Set.foldr (\b acc -> acc && (Set.member b reachableSet)) True tySet
      _ -> True
    reachableGraphFromGoalHelper g visited toVisit = case toVisit of
      [] -> visited
      curr:xs -> if Set.member curr visited
        then reachableGraphFromGoalHelper g visited xs
        else reachableGraphFromGoalHelper g (Set.insert curr visited) (xs ++ (HashMap.keys (HashMap.lookupDefault HashMap.empty curr g)))

rmUnreachableComposite :: Environment -> Set SuccinctType -> Set SuccinctType
rmUnreachableComposite env reachableSet = Set.foldr (\t acc -> if isCompositeReachable t then acc else Set.delete t acc) reachableSet (compositeNodes)
  where
    isCompositeNode ty = case ty of
      SuccinctComposite _ -> True
      _ -> False
    compositeNodes = Set.filter isCompositeNode reachableSet
    isCompositeReachable t = let SuccinctComposite tySet = t in 
      Set.foldr (\b acc -> acc && (Set.member b reachableSet)) True tySet

findDstNodesInGraph :: Environment -> SuccinctType -> HashMap SuccinctType (Set SuccinctEdge)
findDstNodesInGraph env typ = case typ of
  SuccinctLet _ _ ty -> findDstNodesInGraph env ty
  SuccinctAll _ ty -> findDstNodesInGraph env ty
  _ -> let
    filter_fun k v = (succinctAnyEq typ k) && (not (isSuccinctComposite k))
    -- measureNames = case typ of
    --   SuccinctDatatype (id,_) _ _ _ _ -> Map.keysSet $ allMeasuresOf id env
    --   _ -> Set.empty
    -- checkVars k m = (not (isSuccinctComposite k)) && (succinctAnyEq k typ) && (isStrongerThan measureNames k typ || 
    --                  not (Set.null (Set.map getEdgeId (Set.unions (HashMap.elems m)) `Set.intersection` ((measuresOf typ) `Set.difference` (measuresOf k)))))
    candidateMap = HashMap.filterWithKey filter_fun (env ^. graphFromGoal)
    in HashMap.foldr (\m acc -> HashMap.foldrWithKey (\kty set accM -> HashMap.insertWith Set.union kty set accM) acc m) HashMap.empty candidateMap

pruneGraphByReachability g reachableSet = HashMap.foldrWithKey (\k v acc -> if Set.member k reachableSet then HashMap.insert k (HashMap.filterWithKey (\k' s -> Set.member k' reachableSet) v) acc else acc) HashMap.empty g

termScore env prog@(Program p (sty, rty, _)) =
  (if holes == 0 
    then 99999 
    else 1.0 / (fromIntegral holes) +
      1.0 / (fromIntegral $ greatestHoleType 0 prog) + 
      1.0 / (fromIntegral wholes)) + 
      -- if (d /= 0) then 100.0 / (fromIntegral d) else 100.0 + 
      100.0 / (fromIntegral size) + 
      2 * (fromIntegral $ Set.size vars) + 
      (fromIntegral $ Set.size consts)
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

refineScalarType env typ = case measureRefinement typ of
  Nothing -> typ
  Just argLt -> addRefinement typ argLt
  where
    measureRefinement (ScalarT dt@(DatatypeT name _ _) fml) = let
      measureNames = Map.keysSet $ allMeasuresOf name env
      metric mName x = Pred IntS mName [x]
      argSort = toSort dt
      in Just (Set.foldr (\name accPred -> metric name (Var argSort valueVarName) |>=| IntLit 0  |&| accPred) ftrue measureNames)
    measureRefinement _ = Nothing

allSuccinctNodes :: Environment -> Set SuccinctType
allSuccinctNodes env = Set.fromList $ (HashMap.keys (env ^. succinctGraph)) ++ (HashMap.foldr (\m acc -> acc ++ (HashMap.keys m)) [] (env ^. succinctGraph))

edges env = HashMap.foldrWithKey (\k v acc -> (map (\(k',v') -> (k,v',k')) (HashMap.toList v)) ++ acc) [] (env ^. graphFromGoal)

nodes env = allSuccinctNodes env

showGraphViz env =
  "digraph name{\n" ++
  "layout=dot;\n" ++
  "splines=true;\n" ++ 
  "margin=\"0.5,0.5\";\n" ++
  "fontsize=16;\n" ++
  "dpi=250;\n"++
  "concentrate=True;\n" ++
  "rankdir=BT;\n" ++
  "ratio=fill;\n" ++
  "size=\"25,25\";\n" ++
  "node  [style=\"rounded,filled,bold\", shape=box, width=2, fontsize=20];\n"++
  "edge [fontsize=20]\n"++
  (concatMap showNode $ nodes env) ++
  (concatMap showEdge $ edges env) ++
  "}\n"
  where showEdge (from, t, to) = "\"" ++ (succinct2str from) ++ "\"" ++ " -> " ++ "\"" ++(succinct2str to) ++"\"" ++
                                 " [label = \"" ++ (Set.foldr (\(SuccinctEdge s params _) str -> str++","++s) "" t) ++ "\"];\n"
        showNode v = "\"" ++(succinct2str v) ++ "\"" ++"\n"
