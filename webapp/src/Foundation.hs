{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Types.Experiments

import Yesod.Core
import Data.Text (Text)
import Yesod.Form
import Yesod.Form.Jquery
import Data.Map.Strict (Map)
import Data.IORef
import System.IO
import System.Process

data App = App {
    threadMap :: IORef (Map String (Handle, ProcessHandle))
}

mkYesodData "App" $(parseRoutesFile "webapp/routes")

instance Yesod App
instance YesodJquery App

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage
