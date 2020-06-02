{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Aeson

data TygarQuery = TygarQuery
    {
      query_uuid :: String,
      typeSignature :: String
    } deriving (Show, Generic)

instance FromJSON TygarQuery

data ResultEntry = ResultEntry{
  query :: String,
  solution :: String,
  packages :: [String],
  result_uuid :: String
} deriving(Generic)

instance ToJSON ResultEntry

data TygarResult = TygarResult{
  results :: [ResultEntry]
} deriving(Generic)

instance ToJSON TygarResult

defaultLogFile = "data/hplus.log"
time_limit = 60 * 10^6 :: Int
