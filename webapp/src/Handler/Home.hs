{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home where

import Import

-- TYGAR query params
-- TODO: Determine if these are all the supported modules
-- TODO: Refactor modules into a list of sorts (DRY)
-- TODO: add subfields
data TygarQuery = TygarQuery 
    {
        signature :: Text,

        -- Supported modules. Needs to be refactored
        dMaybe :: Bool,
        dEither :: Bool,
        dList :: Bool,
        tShow :: Bool, 
        gChar :: Bool,
        dInt :: Bool,
        dBSLazy  :: Bool,
        dBSLazyBuilder :: Bool,

        tier :: Tier
    }
    
-- What tier to search in
data Tier = Tier1 | Tier2
    deriving (Show, Eq, Enum, Bounded)

tiers :: [(Text, Tier)]
tiers = [("Tier1", Tier1), ("Tier2", Tier2)]

personForm :: Html -> MForm Handler (FormResult TygarQuery, Widget)
personForm _ = do
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

    let personRes = TygarQuery <$> signatureRes <*> dMaybeRes <*> dEitherRes <*> dListRes <*> tShowRes <*> gCharRes <*> dIntRes <*> dBSLazyRes <*> dBSLazyBuilderRes <*> tierRes  
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
        

-- Homepage GET handler
getHomeR :: Handler Html
getHomeR = do
    ((_, formWidget), formEnctype) <- runFormGet personForm
    defaultLayout $ do
        setTitle "TYGAR Demo | Home"
        $(widgetFile "homepage")

-- POST not defined for HOME
-- TODO: remove?
postHomeR :: Handler Html
postHomeR = error "todo"