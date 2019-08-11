module Types.Filtering where

import Control.Exception
import Data.Typeable
import Text.Printf
import Data.List (intercalate)

defaultTimeout = 1000000 :: Int
defaultNumChecks = 10 :: Int
defaultEvalLength = 100 :: Int

data ArgumentType =
    Concrete    String
  | Polymorphic String
  | ArgTypeList ArgumentType
  | ArgTypeTuple [ArgumentType]
  | ArgTypeApp  ArgumentType ArgumentType
  deriving (Eq)

instance Show ArgumentType where
  show (Concrete    name) = name
  show (Polymorphic name) = name
  show (ArgTypeList sub)  = printf "[%s]" (show sub)
  show (ArgTypeApp  l r)  = printf "(%s) %s"  (show l) (show r)
  show (ArgTypeTuple types) = 
    (printf "(%s)" . intercalate ", " . map show) types

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

data SampleResult =
    Sample { _inputs :: [String]
           , _results :: [[String]]}
  | EmptySample
  deriving (Eq, Show)