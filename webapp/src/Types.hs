{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


module Types where

import Data.Text (Text)

data Tier = Partial | Total
    deriving (Show, Eq)

data TygarQuery = TygarQuery
    {
      typeSignature :: Text,
      chosenModules :: [String],
      tier :: Tier
    } deriving (Show)
