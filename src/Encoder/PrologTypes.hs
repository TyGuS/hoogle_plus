{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Encoder.PrologTypes where

import Data.List (intercalate)
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
import Text.Printf

import Types.Common
import Types.Type
import Types.Experiments
import Encoder.ConstraintEncoder (FunctionCode)

data Direction = In | Out

instance Show Direction where
    show In = "in"
    show Out = "out"

type Place = Int
type Transition = Int
type Tokens = Int
type TimeStep = Int
type Marking = [(Int, Int)]
data Constraint = Node Id Int
                | Arc Direction Place Transition Tokens
                | FireAt TimeStep Transition
                | NotFireAt TimeStep Transition
                | MarkingAt TimeStep Marking
                | NotMarkingAt TimeStep Marking
                | Choices [Constraint]
                | Comment String

instance Show Constraint where
    show (Node name i) = printf "%s(%d)." name i
    show (Arc dir p tr w) = printf "arc(%d, %s, %d, %d)." tr (show dir) p w
    show (FireAt t tr) = printf "T%d = %d" t tr
    show (NotFireAt t tr) = printf "T%d \\= %d" t tr
    show (MarkingAt t m) = printf "M%d = %s" t (show $ map (\(a,b) -> [a,b]) m)
    show (NotMarkingAt t m) = printf "M%d \\= %s"  t (show $ map (\(a,b) -> [a,b]) m)
    show (Choices cs) = printf "(%s)" (intercalate "; " (map show cs))
    show (Comment str) = printf "%% %s" str

data Constraints = Constraints {
    _persistConstraints :: [Constraint],
    _initConstraints :: Constraint,
    _finalConstraints :: Constraint,
    _blockConstraints :: [Constraint]
}

emptyConstraints = Constraints {
    _persistConstraints = [],
    _initConstraints = undefined,
    _finalConstraints = undefined,
    _blockConstraints = []
}

makeLenses ''Constraints

data IncrementState = IncrementState {
    _counter :: Int,
    _block :: Constraint,
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
    _place2variable :: HashMap AbstractSkeleton Int, -- place name and timestamp
    _trans2variable :: HashMap Id Int, -- transition name and abstraction level
    _variable2trans :: HashMap Int Id,
    _type2transition :: HashMap AbstractSkeleton (Set Id)
} deriving(Eq)

emptyVariables = EncodeVariables {
    _transitionNb = 0,
    _variableNb = 1,
    _place2variable = HashMap.empty,
    _trans2variable = HashMap.empty,
    _variable2trans = HashMap.empty,
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

data PrologState = PrologState {
    _encSearchParams :: SearchParams,
    _increments :: IncrementState,
    _variables :: EncodeVariables,
    _constraints :: Constraints,
    _refinements :: RefineInfo
}

emptyPrologState = PrologState {
    _encSearchParams = defaultSearchParams,
    _increments = emptyIncrements,
    _variables = emptyVariables,
    _constraints = emptyConstraints,
    _refinements = emptyRefine
}

makeLenses ''PrologState

type Encoder = StateT PrologState IO
