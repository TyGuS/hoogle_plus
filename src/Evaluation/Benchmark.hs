module Evaluation.Benchmark where

import           GHC.Generics                   ( Generic )

import           Data.Aeson                     ( FromJSON )

import           Types.Filtering                ( Example )


data Benchmark = Benchmark
    { name     :: String
    , query    :: String
    , solution :: String
    , source   :: String
    , examples :: [Example]
    }
    deriving (Eq, Generic, Show)

instance FromJSON Benchmark where
