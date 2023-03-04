module Application.TermSearch.Type
  ( TypeSkeleton(..)
  , Benchmark(..)
  , Argument
  , Mode(..)
  , AblationType(..)
  ) where

import           Data.Data                      ( Data )
import           Data.Hashable                  ( Hashable )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

import           Data.ECTA
import           Data.ECTA.Term

data TypeSkeleton
  = TVar Text
  | TFun TypeSkeleton TypeSkeleton
  | TCons Text [TypeSkeleton]
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable TypeSkeleton

data Benchmark = Benchmark { bmName      :: Text
                           , bmSize      :: Int
                           , bmSolution  :: Term
                           , bmArguments :: [(Text, TypeSkeleton)]
                           , bmGoalType  :: TypeSkeleton
                           }
  deriving (Eq, Ord, Show, Read)

type Argument = (Symbol, Node)

data Mode
  = Normal
  | HKTV
  | Lambda
  deriving (Eq, Ord, Show, Data, Generic)

data AblationType
  = Default
  | NoReduction
  | NoEnumeration
  | NoOptimize
  deriving (Eq, Ord, Show, Data, Generic)