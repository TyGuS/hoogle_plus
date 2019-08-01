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
import Control.Concurrent.Chan
import Data.Map (Map)
import Data.IORef

data App = App {
    threadMap :: IORef (Map String (Chan Message))
}

mkYesodData "App" $(parseRoutesFile "webapp/routes")

instance Yesod App
instance YesodJquery App

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage
