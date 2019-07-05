{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}
module Types.Environment where

import Types.Type
import Types.Common
import Types.Generate

import GHC.Generics hiding (to)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Lens

-- | User-defined datatype representation
data DatatypeDef = DatatypeDef {
  _typeParams :: [Id],              -- ^ Type parameters
  _predVariances :: [Bool],         -- ^ For each predicate parameter, whether it is contravariant
  _constructors :: [Id]            -- ^ Constructor names
} deriving (Eq, Ord, Generic, Show)

makeLenses ''DatatypeDef


-- | Typing environment
data Environment = Environment {
  _symbols :: Map Id RSchema,          -- ^ Variables and constants (with their refinement types), indexed by arity
  _arguments :: Map Id RSchema,            -- ^ Function arguments, required in all the solutions
  _typeClasses :: Map Id (Set Id),         -- ^ Type class instances
  _boundTypeVars :: [Id],                  -- ^ Bound type variables
  -- | Constant part:
  _constants :: Set Id,                    -- ^ Subset of symbols that are constants
  _datatypes :: Map Id DatatypeDef,        -- ^ Datatype definitions
  _typeSynonyms :: Map Id ([Id], RType),   -- ^ Type synonym definitions
  _unresolvedConstants :: Map Id RSchema,  -- ^ Unresolved types of components (used for reporting specifications with macros)
  _included_modules :: Set String,          -- ^ The set of modules any solution would need to import
  _typClassInstances :: [(String, String)],
  _condTypClasses :: [([(String, [Set String])], (String, String))],
  _hoCandidates :: [Id]
  } deriving(Generic)

makeLenses ''Environment


instance Eq Environment where
  (==) e1 e2 = (e1 ^. symbols) == (e2 ^. symbols)

instance Ord Environment where
  (<=) e1 e2 = (e1 ^. symbols) <= (e2 ^. symbols)


-- | Empty environment
emptyEnv = Environment {
  _symbols = Map.empty,
  _arguments = Map.empty,
  _typeClasses = Map.empty,
  _boundTypeVars = [],
  _constants = Set.empty,
  _datatypes = Map.empty,
  _typeSynonyms = Map.empty,
  _unresolvedConstants = Map.empty,
  _included_modules = Set.empty,
  _typClassInstances = [],
  _condTypClasses = [],
  _hoCandidates = []
}
