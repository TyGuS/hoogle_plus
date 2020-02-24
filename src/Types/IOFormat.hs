{-# LANGUAGE DeriveGeneric #-}

module Types.IOFormat where

import Types.Type
import Types.Program

import GHC.Generics
import Data.Aeson

data Example = Example {
    inputs :: [String],
    output :: String
} deriving(Eq, Generic)

instance Show Example where
    show e = unwords [unwords (inputs e), "==>", output e]

type ErrorMessage = String
type TypeQuery = String

data Input = Input {
    query :: TypeQuery,
    inExamples :: [Example]
} deriving(Eq, Show, Generic)

type Tag = (RSchema, RType)

data Output = Output {
    solution :: RProgram,
    outExamples :: [Example]
} deriving(Eq, Generic)


instance ToJSON Example
instance FromJSON Example
instance ToJSON Input
instance FromJSON Input
instance ToJSON Output
instance FromJSON Output
