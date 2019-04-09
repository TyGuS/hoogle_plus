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
import Types
import Data.Text (unpack)
import Home
import HooglePlus.Synthesize (synthesize, envToGoal)
import Database.Environment (generateEnv)
import Types.Experiments
import Types.Generate
import Types.Program (RProgram)

runQuery :: TygarQuery -> IO [RProgram]
runQuery queryOpts = do
    env <- generateEnv options
    goal <- envToGoal env (unpack $ typeSignature queryOpts)
    (queryResults, _) <- fmap unzip $ synthesize defaultSearchParams goal
    return queryResults
    where options = defaultGenerationOpts {
      modules = (chosenModules queryOpts),
      pkgFetchOpts = Local {
        files = ["libraries/tier1/base.txt", "libraries/tier1/bytestring.txt", "libraries/ghc-prim.txt"]
        }
    }

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
