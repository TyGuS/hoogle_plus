{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}
module Types.Environment where

import Types.Type
import Types.Common
import Types.Generate
import Types.IOFormat

import GHC.Generics hiding (to)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Control.Lens

-- | User-defined datatype representation
data DatatypeDef = DatatypeDef {
  _typeParams :: [Id],              -- ^ Type parameters
  _constructors :: [Id]            -- ^ Constructor names
} deriving (Eq, Ord, Generic, Show)

makeLenses ''DatatypeDef


-- | Typing environment
data Environment = Environment {
  _symbols :: Map Id SchemaSkeleton,       -- ^ Variables and constants (with their refinement types), indexed by arity
  _arguments :: [(Id, SchemaSkeleton)],    -- ^ Function arguments, required in all the solutions
  _typeClasses :: Map Id (Set Id),         -- ^ Type class instances
  _boundTypeVars :: [Id],                  -- ^ Bound type variables
  -- | Group concrete types
  _groups :: Map Id TypeSkeleton,
  _symbolGroups :: Map Id (Set Id),
  -- | Constant part:
  _constants :: Set Id,                    -- ^ Subset of symbols that are constants
  _datatypes :: Map Id DatatypeDef,        -- ^ Datatype definitions
  _typeSynonyms :: Map TypeSkeleton TypeSkeleton,   -- ^ Type synonym definitions
  _unresolvedConstants :: Map Id SchemaSkeleton,  -- ^ Unresolved types of components (used for reporting specifications with macros)
  _included_modules :: Set String,          -- ^ The set of modules any solution would need to import
  _typClassInstances :: [(String, String)],
  _condTypClasses :: [([(String, [Set String])], (String, String))],
  _hoCandidates :: [Id],
  _queryCandidates :: Map SchemaSkeleton [Example]
  } deriving(Generic)

makeLenses ''Environment


instance Eq Environment where
  (==) e1 e2 = (e1 ^. symbols) == (e2 ^. symbols)

instance Ord Environment where
  (<=) e1 e2 = (e1 ^. symbols) <= (e2 ^. symbols)


-- | Empty environment
emptyEnv = Environment {
  _symbols = Map.empty,
  _arguments = [],
  _typeClasses = Map.empty,
  _boundTypeVars = [],
  _groups = Map.empty,
  _symbolGroups = Map.empty,
  _constants = Set.empty,
  _datatypes = Map.empty,
  _typeSynonyms = Map.empty,
  _unresolvedConstants = Map.empty,
  _included_modules = Set.empty,
  _typClassInstances = [],
  _condTypClasses = [],
  _hoCandidates = [],
  _queryCandidates = Map.empty 
}
