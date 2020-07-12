module Types.Filtering where

import Control.Exception
import Control.Monad.State
import Data.Typeable
import Text.Printf
import Data.List (intercalate)
import Types.IOFormat (Example)

import Test.SmallCheck.Drivers

defaultTimeoutMicro = 3 * 10^6 :: Int
defaultDepth = 3 :: Int
defaultInterpreterTimeoutMicro = 5 * 10^6 :: Int
defaultMaxOutputLength = 10 :: Int 
defaultGenerationTimeoutMicro = 30 * 10^6 :: Int
defaultGenerationDepth = 4 :: Int

frameworkModules =
  zip [ "Test.SmallCheck"
  , "Test.SmallCheck.Drivers"
  , "Test.LeanCheck.Function.ShowFunction"
  , "System.IO.Silently"
  , "Control.Exception"
  , "Control.Monad"
  , "Control.Monad.State"
  ] (repeat Nothing)

  ++ [("Test.ChasingBottoms", Just "CB")]

type SmallCheckResult = (Maybe PropertyFailure, [Example])
type GeneratorResult = [Example]

type AssociativeExamples = [(String, [Example])]

data FunctionCrashDesc = 
    AlwaysSucceed Example
  | AlwaysFail Example
  | PartialFunction [Example]
  | UnableToCheck String
  deriving (Eq)

instance Show FunctionCrashDesc where
  show (AlwaysSucceed i) = show i
  show (AlwaysFail i) = show i
  show (PartialFunction xs) = unlines (map show xs)
  show (UnableToCheck ex) = "Exception: " ++ show ex

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

type TypeConstraint = ArgumentType

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
  inputs :: [[String]],
  solutions :: [String],
  solutionExamples :: [(String, FunctionCrashDesc)],
  differentiateExamples :: [(String, Example)]
} deriving (Eq, Show)

emptyFilterState = FilterState {
  inputs = [],
  solutions = [],
  solutionExamples = [],
  differentiateExamples = []
}

type FilterTest m = StateT FilterState m
