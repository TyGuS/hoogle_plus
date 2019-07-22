{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text (Text)
import GHC.Generics
import Yesod.Core

data Tier = Partial | Total
    deriving (Show, Eq)

data TygarQuery = TygarQuery
    {
      typeSignature :: String
    } deriving (Show, Generic)

instance FromJSON TygarQuery

data ResultEntry = ResultEntry{
  solution :: String,
  packages :: [String]
} deriving(Generic)

instance ToJSON ResultEntry

data TygarResult = TygarResult{
  results :: [ResultEntry]
} deriving(Generic)

instance ToJSON TygarResult

defaultLogFile = "data/hplus.log"
