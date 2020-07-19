{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Encoder.Z3SMTTypes where

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
import Types.Experiments
import Types.Type

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
    _loc :: Int
} deriving(Eq)

emptyIncrements = IncrementState {
    _counter = 0,
    _block = undefined,
    _prevChecked = False,
    _loc = 1
}

makeLenses ''IncrementState

data EncodeVariables = EncodeVariables {
    _transitionNb :: Int,
    _variableNb :: Int,
    _place2variable :: HashMap (AbstractSkeleton, Int) Z3.AST, -- place name and timestamp
    _time2variable :: HashMap Int Z3.AST, -- timestamp and abstraction level
    _transition2id :: HashMap Id Z3.AST, -- transition name and abstraction level
    _id2transition :: HashMap Int Id,
    _type2transition :: HashMap AbstractSkeleton (Set Id)
} deriving(Eq)

emptyVariables = EncodeVariables {
    _transitionNb = 0,
    _variableNb = 1,
    _place2variable = HashMap.empty,
    _time2variable = HashMap.empty,
    _transition2id = HashMap.empty,
    _id2transition = HashMap.empty,
    _type2transition = HashMap.empty
}

makeLenses ''EncodeVariables

data RefineInfo = RefineInfo {
    _mustFirers :: HashMap Id [Id],
    _disabledTrans :: [Id],
    _returnTyps :: [AbstractSkeleton]
} deriving(Eq)

emptyRefine = RefineInfo {
    _mustFirers = HashMap.empty,
    _disabledTrans = [],
    _returnTyps = []
}

makeLenses ''RefineInfo

data Z3SMTState = Z3SMTState {
    _z3env :: Z3Env,
    _encSearchParams :: SearchParams,
    _increments :: IncrementState,
    _variables :: EncodeVariables,
    _constraints :: Constraints,
    _refinements :: RefineInfo
} deriving(Eq)

emptyZ3SMTState = Z3SMTState {
    _z3env = undefined,
    _encSearchParams = defaultSearchParams,
    _increments = emptyIncrements,
    _variables = emptyVariables,
    _constraints = emptyConstraints,
    _refinements = emptyRefine
}

makeLenses ''Z3SMTState

newEnv :: Maybe Logic -> Opts -> IO Z3Env
newEnv mbLogic opts =
  Z3.withConfig $ \cfg -> do
    setOpts cfg opts
    ctx <- Z3.mkContext cfg
    solver <- maybe (Z3.mkSolver ctx) (Z3.mkSolverForLogic ctx) mbLogic
    return $ Z3Env solver ctx

initialZ3Env = newEnv Nothing stdOpts

type Encoder = StateT Z3SMTState IO
