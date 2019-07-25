{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Home where

import Yesod.Core
import Text.Lucius
import Text.Julius
import Yesod.Form
import Data.Text (Text)
import Control.Monad (filterM)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Haskell.Exts.Pretty

import Database.Environment
import Database.Presets
import Types.Generate
import Types
import Foundation

getDatabase :: IO [((String, String), [String])]
getDatabase = do
    files <- getFiles (pkgFetchOpts genOptsTier2)
    let mdls = modules genOptsTier2
    mp <- filesToEntries files False
    let mp' = Map.restrictKeys mp (Set.fromList mdls)
    let mplist = zip [0,1..] $ Map.toList mp'
    return $ map (\(i, (a, b)) -> (("mid" ++ show i, a), map unpackEntry b)) mplist
    where
      unpackEntry (EPackage pkg) = pkg
      unpackEntry (EModule mdl) = mdl
      unpackEntry (EDecl decl) = prettyPrint decl

getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        mcurrentRoute <- getCurrentRoute
        let candidates = [] :: [String]
        db <- liftIO getDatabase
        setTitle "TYGAR Demo - Home"
        addScriptRemote "https://use.fontawesome.com/releases/v5.9.0/js/all.js"
        addScriptRemote "https://code.jquery.com/jquery-3.4.1.min.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
        addScriptRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/highlight.min.js"
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.8/styles/default.min.css"
        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
        toWidget $(luciusFile "webapp/src/templates/homepage.lucius")
        toWidget $(juliusFile "webapp/src/templates/homepage.julius")
        $(whamletFile "webapp/src/templates/homepage.hamlet")
