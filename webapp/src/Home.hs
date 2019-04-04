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
    (tierRes, tierView) <- mreq (radioFieldList tiers) settings2 Nothing
    (dMaybeRes, dMaybeView)   <- mreq checkBoxField settings2 Nothing
    (dEitherRes, dEitherView) <- mreq checkBoxField settings2 Nothing
    (dListRes, dListView) <- mreq checkBoxField settings2 Nothing
    (tShowRes, tShowView) <- mreq checkBoxField settings2 Nothing
    (gCharRes, gCharView) <- mreq checkBoxField settings2 Nothing
    (dIntRes, dIntView) <- mreq checkBoxField settings2 Nothing
    (dBSLazyRes, dBSLazyView) <- mreq checkBoxField settings2 Nothing
    (dBSLazyBuilderRes, dBSLazyBuilderView) <- mreq checkBoxField settings2 Nothing


    let sm = SupportedModules <$> dMaybeRes <*> dEitherRes <*> dListRes <*> tShowRes <*> gCharRes <*> dIntRes <*> dBSLazyRes <*> dBSLazyBuilderRes
    let personRes = TygarQuery <$> signatureRes <*> sm <*> tierRes
    let widget = [whamlet|
                <p>
                    <b>
                        Search in these modules
                <label .checkbox-inline>
                    ^{fvInput dMaybeView}
                        Data.Maybe
                <label .checkbox-inline>
                    ^{fvInput dEitherView}
                        Data.Either
                <label .checkbox-inline>
                    ^{fvInput dListView}
                        Data.List
                <label .checkbox-inline>
                    ^{fvInput tShowView}
                        Text.Show
                <label .checkbox-inline>
                    ^{fvInput gCharView}
                        GHC.Char
                <label .checkbox-inline>
                    ^{fvInput dIntView}
                        Data.Int
                <label .checkbox-inline>
                    ^{fvInput dBSLazyView}
                        Data.ByteString.Lazy
                <label .checkbox-inline>
                    ^{fvInput dBSLazyBuilderView}
                        Data.ByteString.Lazy.Builder
                <br>
                <br>
                <p>
                    <b>
                        Select tier
                <label class="radio-inline">
                    ^{fvInput tierView}
                <br>
                <br>
                ^{fvInput signatureView}
                <br>
                <div id="search-button" .input-group-append>
                    <button .btn .btn-outline-secondary type="submit" id="button-addon2">
                        Search
            |]
    return (personRes, widget)
    where settings = FieldSettings {
                        fsLabel   = "", 
                        fsTooltip = Nothing,
                        fsId      = Nothing,
                        fsName    = Nothing,
                        fsAttrs   = [
                            ("class", "form-control"), 
                            ("placeholder", "Search by type singature!")
                            ]
                        }
          settings2 = FieldSettings {
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
        setTitle "TYGAR Demo - Home"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"


        toWidget [lucius|
            .hplus_font{
                font-family: 'Roboto Condensed', sans-serif;
                color: #59057B;
            }

            main-style {
                min-height: 90vh;
            }

            .footer-style{
                color: white;
                background-color: #1b3764;
                
                display: flex;
                align-items: center;
                
                position: absolute;
                bottom: 0;
                min-height: 10vh;
                width: 100%;
            }
        |]
        [whamlet|
            <main .container .main-style>
                <h1 .hplus_font>
                    TYGAR
                    <br>
                <h5>
                    Welcome to the TYGAR Demo
                <p>
                    TYGAR (pronounced like tiger) is proof-of-concept tool aspiring to introduce new functionality in Haskell API search engines, most notably Hoogle.
                <form method=post action=@{SearchR} enctype=#{formEnctype}>
                    ^{formWidget}
            <footer .footer .footer-style .text-center>
                <div .container>
                <span>
                    Made with blood, sweat, and tears by Zheng, David, Michael, Joe, Ranjit and Nadia.

        |]
