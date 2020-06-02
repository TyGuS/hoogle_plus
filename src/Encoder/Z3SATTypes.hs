{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Encoder.Z3SATTypes where

import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Z3.Base as Z3
import Z3.Monad hiding(Z3Env, newEnv)
import Control.Monad.State
import Data.Data
import Data.Typeable
import Data.Function
import Control.Lens

import Types.Common
import Types.Abstract
import Types.Experiments
import Encoder.ConstraintEncoder

data Z3Env = Z3Env {
    envSolver  :: Z3.Solver,
    envContext :: Z3.Context
} deriving(Eq)

data Constraints = Constraints {
    _persistConstraints :: [Z3.AST],
    _optionalConstraints :: [Z3.AST],
    _finalConstraints :: [Z3.AST],
    _blockConstraints :: [Z3.AST]
} deriving(Eq)

emptyConstraints = Constraints {
    _persistConstraints = [],
    _optionalConstraints = [],
    _finalConstraints = [],
    _blockConstraints = []
}

makeLenses ''Constraints

data IncrementState = IncrementState {
    _counter :: Int,
    _block :: Z3.AST,
    _prevChecked :: Bool,
    _loc :: Int,
    _encodedSigs :: [FunctionCode]
} deriving(Eq)

emptyIncrements = IncrementState {
    _counter = 0,
    _block = undefined,
    _prevChecked = False,
    _loc = 1,
    _encodedSigs = []
}

makeLenses ''IncrementState

data EncodeVariables = EncodeVariables {
    _transitionNb :: Int,
    _variableNb :: Int,
    _place2variable :: HashMap (AbstractSkeleton, Int, Int) Z3.AST, -- place name, timestep, token number
    _trans2variable :: HashMap (Id, Int) Z3.AST, -- transition name, timestep
    _type2transition :: HashMap AbstractSkeleton (Set Id)
} deriving(Eq)

emptyVariables = EncodeVariables {
    _transitionNb = 0,
    _variableNb = 1,
    _place2variable = HashMap.empty,
    _trans2variable = HashMap.empty,
    _type2transition = HashMap.empty
}

makeLenses ''EncodeVariables

data RefineInfo = RefineInfo {
    _mustFirers :: HashMap Id [Id],
    _disabledTrans :: [Id],
    _returnTyps :: [AbstractSkeleton],
    _inputTyps :: [AbstractSkeleton]
} deriving(Eq)

emptyRefine = RefineInfo {
    _mustFirers = HashMap.empty,
    _disabledTrans = [],
    _returnTyps = [],
    _inputTyps = []
}

makeLenses ''RefineInfo

data Z3SATState = Z3SATState {
    _z3env :: Z3Env,
    _encSearchParams :: SearchParams,
    _increments :: IncrementState,
    _variables :: EncodeVariables,
    _constraints :: Constraints,
    _refinements :: RefineInfo
} deriving(Eq)

emptyZ3SATState = Z3SATState {
    _z3env = undefined,
    _encSearchParams = defaultSearchParams,
    _increments = emptyIncrements,
    _variables = emptyVariables,
    _constraints = emptyConstraints,
    _refinements = emptyRefine
}

makeLenses ''Z3SATState

newEnv :: Maybe Logic -> Opts -> IO Z3Env
newEnv mbLogic opts =
  Z3.withConfig $ \cfg -> do
    setOpts cfg opts
    ctx <- Z3.mkContext cfg
    solver <- maybe (Z3.mkSolver ctx) (Z3.mkSolverForLogic ctx) mbLogic
    return $ Z3Env solver ctx

initialZ3Env = newEnv Nothing stdOpts

type Encoder = StateT Z3SATState IO
