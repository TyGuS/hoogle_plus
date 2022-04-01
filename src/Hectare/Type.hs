module Hectare.Type
  ( Argument
  , Mode(..)
  ) where

import           Data.Data                      ( Data )
import           Data.Hashable                  ( Hashable )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

import           Data.ECTA
import           Data.ECTA.Term


type Argument = (Symbol, Node)

data Mode
  = Normal
  | HKTV
  | Lambda
  deriving (Eq, Ord, Show, Data, Generic)