{-# LANGUAGE DeriveGeneric #-}

module Types.IOFormat where

import Types.Type
import Types.Program

import GHC.Generics
import Data.Aeson

outputPrefix = "RESULTS:"

data QueryType = SearchPrograms
               | SearchTypes
               | SearchResults
  deriving(Eq, Show, Generic)

instance FromJSON QueryType
instance ToJSON QueryType

data Example = Example {
    inputs :: [String],
    output :: String
} deriving(Eq, Generic)

instance Show Example where
    show e = unwords [unwords (inputs e), "==>", output e]

instance ToJSON Example
instance FromJSON Example

type ErrorMessage = String
type TypeQuery = String

data QueryInput = QueryInput {
    query :: TypeQuery,
    inExamples :: [Example]
} deriving(Eq, Show, Generic)

instance FromJSON QueryInput

data QueryOutput = QueryOutput {
    solution :: String,
    outExamples :: [Example]
} deriving(Eq, Generic)

instance ToJSON Output

data ExecInput = ExecInput {
    execQuery :: TypeQuery,
    arguments :: Example,
    execProg :: String
} deriving(Eq, Generic)

instance FromJSON ExecInput

data ExecOutput = ExecOutput {
    execError :: String,
    execResult :: String
} deriving(Eq, Generic)

instance ToJSON ExecOutput
