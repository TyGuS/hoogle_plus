{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Search where

import Import
import Handler.Home

postSearchR :: Handler Html
postSearchR = do
    -- Move this line below back to Home. HOW CAN I MAKE THIS WORK W/ TOKEN?
    ((res, formWidget), formEnctype) <- runFormPostNoToken personForm
    case res of
        FormSuccess _ -> defaultLayout $ do
                            setTitle "TYGAR Demo | Search"
                            $(widgetFile "search") 
        FormFailure err -> error (show err)
        FormMissing  -> error "Not Implemented Yet2"

