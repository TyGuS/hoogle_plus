{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Search where

import Foundation
import Yesod.Core
import Text.Lucius
import Yesod.Form
import Types hiding (modules)
import Data.Text (unpack)
import Home
import HooglePlus.Synthesize (synthesize, envToGoal)
import Database.Environment (generateEnv)
import Types.Experiments
import Types.Generate


runQuery queryOpts = do
    env <- generateEnv genOptsTier1
    goal <- envToGoal env (unpack $ typeSignature queryOpts)
    (queryResults, _) <- fmap unzip $ synthesize defaultSearchParams goal
    return queryResults

postSearchR :: Handler Html
postSearchR = do
    ((res, formWidget), formEnctype) <- runFormPostNoToken searchForm
    case res of
        FormSuccess queryOpts -> defaultLayout $ do
                            mcurrentRoute <- getCurrentRoute
                            candidates <- liftIO $ runQuery queryOpts
                            setTitle "TYGAR Demo | Search"
                            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
                            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                            toWidget $(luciusFile "webapp/src/templates/style.lucius")
                            $(whamletFile "webapp/src/templates/default.hamlet")
        FormFailure err -> error (show err)
        FormMissing  -> error "Form Missing. Please Resubmit"

genOptsTier1 = defaultGenerationOpts {
  modules = myModules,
  pkgFetchOpts = Local {
      files = ["libraries/tier1/base.txt", "libraries/tier1/bytestring.txt", "libraries/ghc-prim.txt"]
      }
  }

myModules = [
  -- base
  "Data.Int",
  "Data.Bool",
  "Data.Maybe",
  "Data.Either",
  "Data.Tuple",
  "GHC.Char",
  "Text.Show",
  -- ByteString
  "Data.ByteString.Lazy",
  "Data.ByteString.Builder"
  ]