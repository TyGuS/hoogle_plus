{-# LANGUAGE DeriveGeneric #-}
module Types.PetriNet where

import Types.Common

import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import Data.Aeson (ToJSON(..), genericToEncoding, defaultOptions)
import qualified Data.HashMap.Strict as HashMap
import Data.Serialize (Serialize)
import GHC.Generics
import Data.Hashable

-- for encoding abstractions into JSON string
type Param = String -- parameter type

data FunctionCode = FunctionCode {
  funName   :: String,  -- function name
  hoParams  :: [FunctionCode],
  funParams :: [Param], -- function parameter types and their count
  funReturn :: [String]   -- function return type
} deriving(Eq, Ord, Show, Generic)

instance ToJSON FunctionCode where
    toEncoding = genericToEncoding defaultOptions
instance Serialize FunctionCode

areEqFuncs fc1 fc2 = let
  ho1 = Set.fromList (hoParams fc1)
  ho2 = Set.fromList (hoParams fc2)
  params1 = Set.fromList (funParams fc1)
  params2 = Set.fromList (funParams fc2)
  ret1 = Set.fromList (funReturn fc1)
  ret2 = Set.fromList (funReturn fc2)
  in
      ho1 == ho2 && params1 == params2 && ret1 == ret2

data Place = Place {
  placeId :: Id,
  placePreset :: Set Id, -- set of transition ids
  placePostset :: Set Id,
  placeMaxToken :: Int
} deriving(Eq, Ord, Show)

instance Hashable Place where
    hashWithSalt s (Place id pre post token) = hashWithSalt s (id, Set.toList pre, Set.toList post, token)

data Transition = Transition {
  transitionId :: Id,
  transitionPreset :: Set Id, -- set of flow ids
  transitionPostset :: Set Id
} deriving(Eq, Ord, Show)

instance Hashable Transition where
    hashWithSalt s (Transition id pre post) = hashWithSalt s (id, Set.toList pre, Set.toList post)

data Flow = Flow {
  flowId :: Id, -- flow id is the string version of pair (Place:<placeid>,Transition:<transitionid>) or reversed
  flowFrom :: Id,
  flowTo :: Id,
  flowPlace :: Id,
  flowTransition :: Id,
  flowWeight :: Int
} deriving(Eq, Ord, Show)

data PetriNet = PetriNet {
  pnPlaces :: HashMap Id Place,
  pnTransitions :: HashMap Id Transition,
  pnFlows :: HashMap Id Flow
} deriving(Eq, Ord, Show)
