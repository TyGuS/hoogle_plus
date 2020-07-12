{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Encoder.EncoderTypes where

import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.State
import Data.Data
import Data.Typeable
import Data.Function
import Control.Lens
-- for ILP
-- import Numeric.Limp.Program hiding (_constraints)
-- import Numeric.Limp.Rep
-- for z3
import qualified Z3.Base as Z3
import Z3.Monad hiding(Z3Env, newEnv)
-- for prolog
import qualified Encoder.PrologTypes as Prolog
-- for souffle
-- import Encoder.SouffleTypes

import Types.Common
import Types.Type
import Types.Experiments
import Encoder.ConstraintEncoder (FunctionCode)

--------------------------------------------------------------------------------
-- | Encoder State Definitions
--------------------------------------------------------------------------------

-- | Constraints used to encode searching conditions
-- c is the type of one constraint
-- b is the type of the block constraint, used in enumeration
-- v is the type of boundaries, used only in ILP mode 
data Constraints c b v = Constraints
    { _persistConstraints :: [c]  -- ^ constraints that keep between refinements and depth change
    , _optionalConstraints :: [c] -- ^ constraints that abandon between refinements
    , _finalConstraints :: [c]    -- ^ final conditions, abandon when refinement and depth change
    , _mustConstraints :: [c]     -- ^ transitions must be fired, abandon condition depends on solvers
    , _blockConstraints :: [b]    -- ^ constraints to block some paths, abandon between depth change
    , _varBounds :: [v]           -- ^ only for ILP, describe variable boundaries
    }

emptyConstraints = Constraints
    { _persistConstraints = []
    , _optionalConstraints = []
    , _finalConstraints = []
    , _mustConstraints = []
    , _blockConstraints = []
    , _varBounds = []
    }

makeLenses ''Constraints

-- | Incremental modified states
-- b is the type of block constraint
data IncrementState b = IncrementState
    { _block :: b                    -- ^ path to be blocked
    , _prevChecked :: Bool           -- ^ does previous path type checked
    , _loc :: Int                    -- ^ current length of path
    , _encodedSigs :: [FunctionCode] -- ^ functions already in the petri net
    }

emptyIncrements = IncrementState
    { _block = undefined
    , _prevChecked = False
    , _loc = 1
    , _encodedSigs = []
    }

makeLenses ''IncrementState

-- | Variables and mappings
-- pv: place variable type
-- tv: transition variable type
-- vc: variable constraint type
data EncodeVariables pv tv vc = EncodeVariables
    { _transitionNb :: Int                                  -- ^ transition indices
    , _variableNb :: Int                                    -- ^ variable indices
    , _cancelNb :: Int                                      -- ^ cancel assumption name indices
    , _place2variable :: HashMap pv vc                      -- ^ place name and timestamp
    , _trans2variable :: HashMap tv vc                      -- ^ transition name and abstraction level
    , _time2variable :: HashMap Int vc                      -- ^ timestamp and abstraction level
    , _type2transition :: HashMap AbstractSkeleton (Set Id) -- ^ mapping from places to transitions
    , _id2transition :: HashMap Int Id                      -- ^ mapping from transition index to transition names
    } deriving(Eq)

emptyVariables = EncodeVariables
    { _transitionNb = 0
    , _variableNb = 1
    , _cancelNb = 0
    , _place2variable = HashMap.empty
    , _trans2variable = HashMap.empty
    , _time2variable = HashMap.empty
    , _type2transition = HashMap.empty
    , _id2transition = HashMap.empty
    }

makeLenses ''EncodeVariables

-- | Refinement information
data RefineInfo = RefineInfo
    { _mustFirers :: HashMap Id [Id]    -- ^ higher order arguments, can be used as transitions or tokens
    , _disabledTrans :: [Id]            -- ^ transitions that deleted after refinements, keep disabled
    , _returnTyps :: [AbstractSkeleton] -- ^ list of acceptable return types, ordered from specific to abstract
    , _inputTyps :: [AbstractSkeleton]  -- ^ list of input types
    } deriving(Eq)

emptyRefine = RefineInfo
    { _mustFirers = HashMap.empty
    , _disabledTrans = []
    , _returnTyps = []
    , _inputTyps = []
    }

makeLenses ''RefineInfo

-- | A general encoder state
data Z3Env = Z3Env {
    envSolver  :: Z3.Solver,
    envContext :: Z3.Context
} deriving(Eq)

data EncoderState c b v pv tv vc = EncoderState 
    { _z3env :: Z3Env
    , _encSearchParams :: SearchParams
    , _increments :: IncrementState b
    , _variables :: EncodeVariables pv tv vc
    , _constraints :: Constraints c b v
    , _refinements :: RefineInfo
    }

emptyEncoderState = EncoderState
    { _z3env = undefined
    , _encSearchParams = defaultSearchParams
    , _increments = emptyIncrements
    , _variables = emptyVariables
    , _constraints = emptyConstraints
    , _refinements = emptyRefine
    }

makeLenses ''EncoderState

--------------------------------------------------------------------------------
-- | CBC State Synonyms
--------------------------------------------------------------------------------

-- type LinearConstraint = Constraint Int Double IntDouble
-- type VarBoundary = Bounds Int Double IntDouble
-- type CBCState = EncoderState LinearConstraint [Int] VarBoundary (AbstractSkeleton, Int) (Id, Int) Int

--------------------------------------------------------------------------------
-- | Z3 SMT State Synonyms
--------------------------------------------------------------------------------

type Z3SMTState = EncoderState Z3.AST Z3.AST Z3.AST (AbstractSkeleton, Int) Id Z3.AST

newEnv :: Maybe Logic -> Opts -> IO Z3Env
newEnv mbLogic opts =
  Z3.withConfig $ \cfg -> do
    setOpts cfg opts
    ctx <- Z3.mkContext cfg
    solver <- maybe (Z3.mkSolver ctx) (Z3.mkSolverForLogic ctx) mbLogic
    return $ Z3Env solver ctx

initialZ3Env = newEnv Nothing stdOpts

--------------------------------------------------------------------------------
-- | Z3 SAT State Synonyms
--------------------------------------------------------------------------------

type Z3SATState = EncoderState Z3.AST Z3.AST Z3.AST (AbstractSkeleton, Int, Int) (Id, Int) Z3.AST

--------------------------------------------------------------------------------
-- | Prolog State Synonyms
--------------------------------------------------------------------------------

type PrologState = EncoderState Prolog.Constraint Prolog.Constraint Prolog.Constraint AbstractSkeleton Id Int

--------------------------------------------------------------------------------
-- | Souffle State Synonyms
--------------------------------------------------------------------------------

type SouffleState = EncoderState Int Int Int Int Int Int
