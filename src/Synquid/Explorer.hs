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
import Database.GraphWeightsProvider
import Database.Util
import PetriNet.AbstractType
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

-- | Choices for the type of path search
data PathStrategy =
  MaxSAT -- ^ Use SMT solver to find a path
  | PetriNet -- ^ Use PetriNet and SyPet
  | PNSMT -- ^ Use PetriNet and SMT solver
  deriving (Eq, Show, Data)

-- | Parameters of program exploration
data ExplorerParams = ExplorerParams {
  _eGuessDepth :: Int,                    -- ^ Maximum depth of application trees
  _scrutineeDepth :: Int,                 -- ^ Maximum depth of application trees inside match scrutinees
  _matchDepth :: Int,                     -- ^ Maximum nesting level of matches
  _auxDepth :: Int,                       -- ^ Maximum nesting level of auxiliary functions (lambdas used as arguments)
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
  iProgram :: SProgram,
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
