module Types.Resolver where

import           GHC.Generics                   ( Generic )

import           Types.Common
import           Types.Type

-- | User-defined datatype representation
type Parameter = Id
type ConstructorName = Id
data DatatypeDef = DatatypeDef Id [Parameter] [ConstructorName]
  deriving (Eq, Ord, Generic, Show)

getParameters :: DatatypeDef -> [Parameter]
getParameters (DatatypeDef _ ps _) = ps

getConstructors :: DatatypeDef -> [ConstructorName]
getConstructors (DatatypeDef _ _ cs) = cs

data TypeSynonym = TypeSynonym Id [Id] TypeSkeleton
  deriving (Eq, Ord, Generic, Show)

data ResolverState = ResolverState
  { getSynonyms  :: [TypeSynonym]
  , getDatatypes :: [DatatypeDef]
  , getIdCount   :: Int
  }
  deriving Show
