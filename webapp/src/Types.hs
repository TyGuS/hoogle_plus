{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Types where

import Data.Text (Text)

data ChosenModules = ChosenModules
  {
    dMaybe :: Bool,
    dEither :: Bool,
    dList :: Bool,
    tShow :: Bool,
    gChar :: Bool,
    dInt :: Bool,
    dBSLazy  :: Bool,
    dBSLazyBuilder :: Bool
  } deriving (Show)



data Tier = Partial | Total
    deriving (Show, Eq)

data TygarQuery = TygarQuery
    {
      typeSignature :: Text,
      modules :: ChosenModules,
      tier :: Tier
    } deriving (Show)
