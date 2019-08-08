module Types.Filtering where

import Control.Exception
import Data.Typeable
import Text.Printf

data ArgumentType =
    Concrete String
  | Polymorphic String
  | ArgTypeList ArgumentType
  deriving (Eq)

instance Show ArgumentType where
  show (Concrete name) = name
  show (Polymorphic _) = "Int"
  show (ArgTypeList sub) = printf "[%s]" (show sub)

newtype NotSupportedException = NotSupportedException String
  deriving (Show, Typeable)

instance Exception NotSupportedException

type TypeConstraints = [(ArgumentType, String)]
type FunctionSigniture = (TypeConstraints, [ArgumentType], ArgumentType)
