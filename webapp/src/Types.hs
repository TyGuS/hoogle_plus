--{-# LANGUAGE NoImplicitPrelude #-}
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

data SupportedModules = SupportedModules
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



data Tier = Tier1 | Tier2
    deriving (Show, Eq, Enum, Bounded)

data TygarQuery = TygarQuery
    {
      typeSignature :: Text,
      modules :: SupportedModules,
      tier :: Tier
    } deriving (Show)
