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
import Data.List (sort, find)
import Data.Maybe

-- for encoding abstractions into JSON string
type Param = String -- parameter type

data FunctionCode = FunctionCode {
  funName   :: String,  -- function name
  hoParams  :: [FunctionCode],
  funParams :: [Param], -- function parameter types and their count
  funReturn :: [String]   -- function return type
} deriving(Eq, Show, Generic)

instance Ord FunctionCode where
  compare fc1 fc2 = let
    using d a b f = d (f a) (f b)
    myCompare = compare fc1 fc2
    ho = using compare fc1 fc2 hoParams
    params = using compare fc1 fc2 funParams
    rets = using compare fc1 fc2 funReturn
    in fromMaybe EQ (find (/= EQ) [ho, params, rets])

instance ToJSON FunctionCode where
    toEncoding = genericToEncoding defaultOptions
instance Serialize FunctionCode

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
