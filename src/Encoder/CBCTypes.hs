{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Encoder.CBCTypes where

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
import Numeric.Limp.Program hiding (_constraints)
import Numeric.Limp.Rep

import Types.Common
import Types.Abstract
import Types.Experiments
import Encoder.ConstraintEncoder (FunctionCode)

type LinearConstraint = Constraint Int Double IntDouble
type VarBoundary = Bounds Int Double IntDouble

data Constraints = Constraints {
    _persistConstraints :: [LinearConstraint],
    _optionalConstraints :: [LinearConstraint],
    _finalConstraints :: [LinearConstraint],
    _blockConstraints :: [LinearConstraint],
    _varBounds :: [VarBoundary]
}

emptyConstraints = Constraints {
    _persistConstraints = [],
    _optionalConstraints = [],
    _finalConstraints = [],
    _blockConstraints = [],
    _varBounds = []
}

makeLenses ''Constraints

data IncrementState = IncrementState {
    _counter :: Int,
    _block :: LinearConstraint,
    _prevChecked :: Bool,
    _loc :: Int,
    _encodedSigs :: [FunctionCode]
}

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
    _place2variable :: HashMap (AbstractSkeleton, Int) Int, -- place name and timestamp
    _trans2variable :: HashMap (Id, Int) Int, -- transition name and abstraction level
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
    _returnTyps :: [AbstractSkeleton]
} deriving(Eq)

emptyRefine = RefineInfo {
    _mustFirers = HashMap.empty,
    _disabledTrans = [],
    _returnTyps = []
}

makeLenses ''RefineInfo

data CBCState = CBCState {
    _encSearchParams :: SearchParams,
    _increments :: IncrementState,
    _variables :: EncodeVariables,
    _constraints :: Constraints,
    _refinements :: RefineInfo
}

emptyCBCState = CBCState {
    _encSearchParams = defaultSearchParams,
    _increments = emptyIncrements,
    _variables = emptyVariables,
    _constraints = emptyConstraints,
    _refinements = emptyRefine
}

makeLenses ''CBCState

type Encoder = StateT CBCState IO
