{-# LANGUAGE LambdaCase, DeriveDataTypeable #-}
module Types.Filtering where

import Control.Exception
import Control.Monad.State
import Data.List (groupBy, intercalate)
import Data.Typeable
import Text.Printf
import Test.QuickCheck (Result)
import qualified Data.Map as Map

import Types.IOFormat (Example(Example))

defaultInterpreterTimeoutMicro = 10 * 10^6 :: Int
defaultTimeoutMicro = 100 :: Int

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

type AssociativeInternalExamples = [(Candidate, [InternalExample])]
type AssociativeExamples = [(Candidate, [Example])]

data InternalExample = InternalExample {
    inputs :: [String],
    inputConstrs :: [String],
    output :: String,
    outputConstr :: String
} deriving(Eq, Read)

instance Show InternalExample where
    show e = unwords [unwords (inputs e), "==>", output e]

toExample :: InternalExample -> Example
toExample (InternalExample i _ o _) = Example i o

data CandidateValidDesc =
    Total   [InternalExample]
  | Partial [InternalExample]
  | Invalid
  | Unknown String
  deriving (Eq)

data CandidateDuplicateDesc =
    New         AssociativeInternalExamples
  | DuplicateOf Candidate
  deriving (Show, Eq)

instance Show CandidateValidDesc where
  show = \case
      Total   examples -> unlines $ map show examples
      Partial examples -> unlines $ map show examples
      Invalid          -> "<bottom>"
      Unknown ex       -> "<exception> " ++ ex

data ArgumentType =
    Concrete      String
  | Polymorphic   String
  | Instantiated  String
  | ArgTypeList   ArgumentType
  | ArgTypeTuple  [ArgumentType]
  | ArgTypeApp    ArgumentType ArgumentType
  | ArgTypeFunc   ArgumentType ArgumentType
  deriving (Eq)

instance Show ArgumentType where
  show (Concrete    name) = name
  show (Polymorphic name) = name
  show (Instantiated name) = name
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
  solutions :: [String],
  solutionDescriptions :: [(String, CandidateValidDesc)],
  differentiateExamples :: Map.Map String [InternalExample],
  discardedSolutions :: [String],
  higherOrderArgumentCache :: Map.Map String [String]
} deriving (Eq, Show)

emptyFilterState = FilterState {
  solutions = [],
  solutionDescriptions = [],
  differentiateExamples = Map.empty,
  discardedSolutions = [],
  higherOrderArgumentCache = Map.fromList [
    ("((Int) -> (Int))", ["const 5", "\\x -> x * x", "id"]),
    ("[Int] -> Int", ["head", "last", "length", "\\xs -> xs !! 1"]),
    ("((Int) -> (Bool))", ["\\x -> x < 0", "\\x -> x > 0"]),
    ("((Int) -> (String))", ["show"])
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


pickExamples :: [InternalExample] -> [InternalExample]
pickExamples examples =
  let examplesGroupedByOutput = groupOn outputConstr (filter noGenerated examples) in
  let examplesGroupedByInput  = map (map last . groupOn inputConstrs) examplesGroupedByOutput in
    concatMap (take 5 . reverse) examplesGroupedByInput
    
    where
      noGenerated :: InternalExample -> Bool
      noGenerated ex = "<Generated>" `notElem` inputs ex

      groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
      groupOn f =
        let unpack = fmap snd . Map.toList
            fld m a = case Map.lookup (f a) m of
              Nothing -> Map.insert (f a) [a] m
              Just as -> Map.insert (f a) (a:as) m
        in unpack . foldl fld Map.empty
