{-# LANGUAGE LambdaCase #-}
module Types.Filtering where

import Control.Exception
import Control.Monad.State
import Data.Typeable
import Text.Printf
import Data.List (intercalate)
import Test.QuickCheck (Result)
import qualified Data.Map as Map

import Types.IOFormat (Example)

defaultInterpreterTimeoutMicro = 10 * 10^6 :: Int
defaultGenerationTimeoutMicro = 30 * 10^6 :: Int

-- todo: remove the constant as not used -- example checker
defaultTimeoutMicro = 100 :: Int
defaultNumExtraExamples = 2 :: Int

hoogleQueryModuleList = ["Prelude", "Data.List", "Data.Maybe", "Data.Either"]
hoogleQueryTypeclassList = ["Eq", "Ord"]
higherOrderGenMaxSize = 5 :: Int

frameworkModules =
  zip [ "Test.QuickCheck"
  , "Test.QuickCheck.Monadic"
  , "Test.QuickCheck.Function"
  , "Control.Exception"
  , "Control.Monad"
  ] (repeat Nothing)

  ++ [("Test.ChasingBottoms", Just "CB")]

type Candidate = String
type BackendResult = Result
type GeneratorResult = [Example]

type AssociativeExamples = [(Candidate, [Example])]

data CandidateValidDesc =
    Total   [Example]
  | Partial [Example]
  | Invalid
  | Unknown String
  deriving (Eq)

data CandidateDuplicateDesc =
    New         AssociativeExamples
  | DuplicateOf Candidate
  deriving (Show, Eq)

instance Show CandidateValidDesc where
  show = \case
      Total   examples -> showExamples examples
      Partial examples -> showExamples examples
      Invalid          -> "<bottom>"
      Unknown ex       -> "<exception> " ++ ex
    where
      showExamples examples = unlines $ map show $ take 3 examples

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

newtype NotSupportedException = NotSupportedException String deriving (Show, Typeable)
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
  solutionDescriptions :: [(String, CandidateValidDesc)],
  differentiateExamples :: Map.Map String [Example],
  discardedSolutions :: [String],
  higherOrderArgumentCache :: Map.Map String [String]
} deriving (Eq, Show)

emptyFilterState = FilterState {
  inputs = [],
  solutions = [],
  solutionDescriptions = [],
  differentiateExamples = Map.empty,
  discardedSolutions = [],
  higherOrderArgumentCache = Map.fromList [
    ("((Int) -> (Int))", ["const 5", "\\x -> x * x", "id"]),
    ("[Int] -> Int", ["head", "last", "length", "\\xs -> xs !! 1"]),
    ("((Int) -> (Bool))", ["\\x -> x < 0", "\\x -> x > 0"])
  ]
}

type FilterTest m = StateT FilterState m

class TestPassable a where isSuccess :: a -> Bool
instance TestPassable CandidateValidDesc where
  isSuccess = \case
    Invalid -> False
    _       -> True

instance TestPassable CandidateDuplicateDesc where
  isSuccess = \case
    New _ -> True
    _   -> False
