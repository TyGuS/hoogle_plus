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
import Synquid.Graph hiding (instantiate)
import Database.GraphWeightsProvider
import Database.Util
import Synquid.GraphConstraintSolver
import Types.Abstract
import PetriNet.PNSolver (PathSolver)
import qualified PetriNet.Abstraction as Abstraction
import qualified PetriNet.PNSolver as PNSolver
import qualified HooglePlus.Encoder as HEncoder

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
import Data.String (fromString)

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
  | PetriNet -- ^ Use PetriNet and SyPet
  | PNSMT -- ^ Use PetriNet and SMT solver
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
  _buildGraph :: Bool,
  _solutionCnt :: Int,
  _pathSearch :: PathStrategy,
  _useHO :: Bool,
  _encoderType :: HEncoder.EncoderType,
  _useRefine :: PNSolver.RefineStrategy
}

makeLenses ''ExplorerParams

type Requirements = Map Id [RType]

data ProgramRank = ProgramRank {
  holes :: Int,
  weights :: Double
} deriving(Ord, Eq, Show)

data ProgramItem = ProgramItem {
  iExpoState :: ExplorerState,
  iConstraints :: [Constraint]
} deriving(Ord, Eq)

type ProgramQueue = MaxPQueue ProgramRank ProgramItem

-- | State of program exploration
data ExplorerState = ExplorerState {
  _typingState :: TypingState,                     -- ^ Type-checking state
  _auxGoals :: [Goal],                             -- ^ Subterms to be synthesized independently
  _solvedAuxGoals :: Map Id RProgram,              -- ^ Synthesized auxiliary goals, to be inserted into the main program
  _lambdaLets :: Map Id (Environment, UProgram),   -- ^ Local bindings to be checked upon use (in type checking mode)
  _requiredTypes :: Requirements,                  -- ^ All types that a variable is required to comply to (in repair mode)
  _symbolUseCount :: Map Id Int                   -- ^ Number of times each symbol has been used in the program so far
  -- temporary storage of the queue state
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

-- | 'runExplorer' @eParams tParams initTS go@ : execute exploration @go@ with explorer parameters @eParams@, typing parameters @tParams@ in typing state @initTS@
runExplorer :: (MonadHorn s, MonadIO s) => ExplorerParams -> TypingParams -> Reconstructor s -> TypingState -> Explorer s a -> s (Either ErrorMessage [a])
runExplorer eParams tParams topLevel initTS go = do
  (ress, (PersistentState _ _ errs)) <- runStateT (observeManyT 1 $ runReaderT (evalStateT go initExplorerState) (eParams, tParams, topLevel)) (PersistentState Map.empty Map.empty [])
  -- (ress, (PersistentState _ _ errs)) <- runStateT (observeManyT 1 $ runReaderT (evalStateT go initExplorerState) (eParams, tParams, topLevel)) (PersistentState Map.empty Map.empty [])
  case ress of
    [] -> return $ Left $ head errs
    res:_ -> return $ Right ress
  where
    initExplorerState = ExplorerState initTS [] Map.empty Map.empty Map.empty Map.empty

-- | 'generateI' @env t@ : explore all terms that have refined type @t@ in environment @env@
-- (top-down phase of bidirectional typechecking)
generateI :: (MonadHorn s, MonadIO s) => Environment -> RType -> Bool -> Explorer s RProgram
generateI env t@(FunctionT x tArg tRes) isElseBranch = do
  let ctx = \p -> Program (PFun x p) t
  useSucc <- asks . view $ _1 . buildGraph
  env' <- return env
  pBody <- inContext ctx $ generateI (unfoldAllVariables $ addVariable x tArg $ addArgument x tArg $ env') tRes False
  return $ ctx pBody
generateI env t@(ScalarT _ _) isElseBranch = do
  pathEnabled <- asks . view $ _1 . pathSearch
  cnt <- asks . view $ _1 . solutionCnt
  case pathEnabled of
      -- liftIO $ writeFile "test.log" $ showGraphViz True env
    PetriNet    -> do
      useHO <- asks . view $ _1 . useHO
      let env' = if useHO then env
                          else env { _symbols = Map.map (Map.filter (not . isHigherOrder . toMonotype)) $ env ^. symbols }
      let args = (Monotype t):(Map.elems $ env' ^. arguments)
      -- start with all the datatypes defined in the components, first level abstraction
      maxLevel <- asks . view $ _1 . explorerLogLevel
      cnt <- asks . view $ _1 . solutionCnt
      rs <- asks . view $ _1 . useRefine
      maxDepth <- asks . view $ _1 . eGuessDepth
      let is = PNSolver.emptySolverState {
                 PNSolver._logLevel = maxLevel
               , PNSolver._maxApplicationDepth = maxDepth
               , PNSolver._refineStrategy = rs
               , PNSolver._abstractionTree = case rs of
                   PNSolver.NoRefine -> Abstraction.firstLvAbs env' (Map.elems (allSymbols env))
                   PNSolver.AbstractRefinement -> PNSolver.emptySolverState ^. PNSolver.abstractionTree
                   PNSolver.Combination -> Abstraction.firstLvAbs env' (Map.elems (allSymbols env))
                   PNSolver.QueryRefinement -> Abstraction.specificAbstractionFromTypes env' (args)
               }
      evalStateT (PNSolver.runPNSolver env' cnt t) is
    PNSMT -> do
      cnt <- asks . view $ _1 . solutionCnt
      encoder <- asks. view $ _1 . encoderType
      let tvs = env ^. boundTypeVars
      let args = map toMonotype (Map.elems (env ^. arguments))
      z3env <- liftIO HEncoder.initialZ3Env
      dummyTyp <- liftIO (HEncoder.dummyType z3env)
      let initialSt = HEncoder.EncoderState {
        HEncoder.z3env = z3env,
        HEncoder.signatures = foldr Map.delete (allSymbols env) (Map.keys (env ^. arguments)),
        HEncoder.datatypes = Map.empty,
        HEncoder.typeSort = dummyTyp,
        HEncoder.boundTvs = Set.fromList tvs,
        HEncoder.places = [],
        HEncoder.names = [],
        HEncoder.nameCounter = Map.empty,
        HEncoder.encoderType = encoder,
        HEncoder.okaySet = [ ScalarT (TypeVarT Map.empty "a") () -- a
                           , ScalarT (DatatypeT "List" [ScalarT (DatatypeT "Maybe" [ScalarT (TypeVarT Map.empty "a") ()] []) ()] []) () -- List (Maybe a)
                           , ScalarT (DatatypeT "List" [ScalarT (TypeVarT Map.empty "a") ()] []) () -- List a
                           , ScalarT (DatatypeT "Maybe" [ScalarT (TypeVarT Map.empty "a") ()] []) () -- List a
                           ]
      }
      liftIO $ evalStateT (HEncoder.runTest tvs args t) initialSt
      error "test"


keepIdCount old new = new {
  _typingState = (new ^. typingState) { _idCount = Map.unionWith max ((old ^. typingState) ^. idCount) ((new ^. typingState) ^. idCount) },
  _symbolUseCount = Map.unionWith max (old ^. symbolUseCount) (new ^. symbolUseCount)
}


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


withinAuxDepth env d [] = d >= 0
withinAuxDepth env d (p:ps) = if d < 0
  then False
  else let x = p
           rtyp = toMonotype $ fromJust $ lookupSymbol x (-1) env
       in if x /= "" && isHigherOrder rtyp
         then withinAuxDepth env (d-1) ps
         else withinAuxDepth env d ps

-- helper function to remove empty ids in a path
removeTrailingEmpty path = case path of
  [] -> []
  (x:xs) -> if "||" `isInfixOf` x || x == ""
         then removeTrailingEmpty xs
         else x:removeTrailingEmpty xs

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

writeLog level msg = do
  maxLevel <- asks . view $ _1 . explorerLogLevel
  if level <= maxLevel then traceShow (plain msg) $ return () else return ()
