{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}


module Types.Types where

import Import
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
  }

data Tier = Tier1 | Tier2
    deriving (Show, Eq, Enum, Bounded)


data TygarQuery = TygarQuery
    {
      signature :: Text,
      modules :: SupportedModules,
      tier :: Tier
    }

