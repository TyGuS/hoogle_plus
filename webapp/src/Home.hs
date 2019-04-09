{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Home where

import Foundation
import Yesod.Core
import Text.Lucius
import Yesod.Form
import Types
import Data.Text (Text)

tiers :: [(Text, Tier)]
tiers = [("Tier1", Tier1), ("Tier2", Tier2)]

searchForm :: Html -> MForm Handler (FormResult TygarQuery, Widget)
searchForm _ = do
    (signatureRes, signatureView) <- mreq textField settings Nothing
    (tierRes, tierView) <- mreq (radioFieldList tiers) defaultSettings Nothing
    (dMaybeRes, dMaybeView) <- mreq checkBoxField defaultSettings Nothing
    (dEitherRes, dEitherView) <- mreq checkBoxField defaultSettings Nothing
    (dListRes, dListView) <- mreq checkBoxField defaultSettings Nothing
    (tShowRes, tShowView) <- mreq checkBoxField defaultSettings Nothing
    (gCharRes, gCharView) <- mreq checkBoxField defaultSettings Nothing
    (dIntRes, dIntView) <- mreq checkBoxField defaultSettings Nothing
    (dBSLazyRes, dBSLazyView) <- mreq checkBoxField defaultSettings Nothing
    (dBSLazyBuilderRes, dBSLazyBuilderView) <- mreq checkBoxField defaultSettings Nothing

    let sm = SupportedModules <$> dMaybeRes <*> dEitherRes <*> dListRes <*> tShowRes <*> gCharRes <*> dIntRes <*> dBSLazyRes <*> dBSLazyBuilderRes
    let personRes = TygarQuery <$> signatureRes <*> sm <*> tierRes
    let widget = $(whamletFile "webapp/src/templates/form.hamlet")
    return (personRes, widget)
    where settings = defaultSettings {
            fsAttrs   = [
                ("class", "form-control"), 
                ("placeholder", "Search by type singature!")
                ]
            }
          defaultSettings = FieldSettings {
            fsLabel   = "", 
            fsTooltip = Nothing,
            fsId      = Nothing,
            fsName    = Nothing,
            fsAttrs   = []
          }


getHomeR :: Handler Html
getHomeR = do
    ((_, formWidget), formEnctype) <- runFormGet searchForm
    defaultLayout $ do
        mcurrentRoute <- getCurrentRoute
        let candidates = []::(String)
        setTitle "TYGAR Demo - Home"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        toWidget $(luciusFile "webapp/src/templates/style.lucius")
        $(whamletFile "webapp/src/templates/default.hamlet")
