{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Types where

import Data.Text (Text)
--import Yesod.Core
--import Yesod.Core.Types      as Import (loggerSet)
--import ClassyPrelude.Yesod
--import Yesod.Core.Types

-- import Import
-- TYGAR query params
-- TODO: Determine if these are all the supported modules

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
