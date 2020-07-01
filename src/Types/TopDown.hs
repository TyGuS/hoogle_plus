{-# LANGUAGE TemplateHaskell #-}
module Types.TopDown where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Lens
import Control.Monad.State
import Control.Concurrent.Chan

import Types.Program
import Types.Abstract
import Types.Experiments hiding (PetriNet)
import Types.Type
import Types.Common
import Types.Encoder

rootNode = AScalar (ATypeVarT varName)
pairProj = "pair_match"

type AbstractCover = HashMap AbstractSkeleton (Set AbstractSkeleton)

data Comps = Comps {
    _components :: [(Id, SType)],
    _memoize :: Map SType [(Id, SType)],
    _counter :: Int
}

emptyComps :: Comps
emptyComps = Comps {
    _components = [],
    _memoize = Map.empty,
    _counter = 0
}

makeLenses ''Comps