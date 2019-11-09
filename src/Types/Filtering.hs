module Types.Filtering where

import Control.Exception
import Control.Monad.State
import Data.Typeable
import Text.Printf
import Data.List (intercalate)

defaultTimeoutMicro = 5 * 10^4 :: Int
defaultInterpreterTimeoutMicro = 6 * 10^6 :: Int
defaultMaxOutputLength = 100 :: Int

quickCheckModules =
  zip [ "Test.QuickCheck"
  , "Test.QuickCheck.Gen"
  , "Test.QuickCheck.Random"
  , "Test.QuickCheck.Monadic" ] (repeat Nothing)

  ++ [("Test.ChasingBottoms", Just "CB")]

supportedInnerType =
  [ "Int"
  , "Float"
  , "Double"
  , "Char"
  , "String" ]

data FunctionCrashKind = 
    AlwaysSucceed
  | AlwaysFail
  | PartialFunction
  deriving (Show)

data ArgumentType =
    Concrete    String
  | Polymorphic String
  | ArgTypeList ArgumentType
  | ArgTypeTuple [ArgumentType]
  | ArgTypeApp  ArgumentType ArgumentType
  | ArgTypeFunc ArgumentType ArgumentType
  deriving (Eq)

instance Show ArgumentType where
  show (Concrete    name) = name
  show (Polymorphic name) = name
  show (ArgTypeList sub)  = printf "[%s]" (show sub)
  show (ArgTypeApp  l r)  = printf "((%s) (%s))"  (show l) (show r)
  show (ArgTypeTuple types) =
    (printf "(%s)" . intercalate ", " . map show) types
  show (ArgTypeFunc src dst) = printf "((%s) -> (%s))" (show src) (show dst)

newtype NotSupportedException = NotSupportedException String
  deriving (Show, Typeable)

instance Exception NotSupportedException

data TypeConstraint = TypeConstraint String String

instance Show TypeConstraint where
  show (TypeConstraint name constraint) = printf "%s %s" constraint name

data FunctionSignature =
  FunctionSignature { _constraints :: [TypeConstraint]
                    , _argsType :: [ArgumentType]
                    , _returnType :: ArgumentType
  }

instance Show FunctionSignature where
  show (FunctionSignature constraints argsType returnType) =
    printf "(%s) => %s" constraintsExpr argsExpr
      where
        constraintsExpr = (intercalate ", " . map show) constraints
        argsExpr = (intercalate " -> " . map show) (argsType ++ [returnType])

data FilterState = FilterState {
  inputs :: [String],
  solutions :: [String]
} deriving (Eq, Show)

emptyFilterState = FilterState {
  inputs = [],
  solutions = []
}

type FilterTest m = StateT FilterState m