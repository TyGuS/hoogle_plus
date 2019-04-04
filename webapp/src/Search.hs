--{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Search where

import Foundation
import Yesod.Core
import Text.Lucius
import Yesod.Form
import Types
import Data.Text (Text)
import Home
-- import HooglePlus.Synthesize (synthesize)
-- import HooglePlus (defaultSearchParams)

postSearchR :: Handler Html
postSearchR = do
    -- Move this line below back to Home. HOW CAN I MAKE THIS WORK W/ TOKEN?
    ((res, formWidget), formEnctype) <- runFormPostNoToken searchForm
    case res of
        FormSuccess _ -> defaultLayout $ do
                            --setTitle "TYGAR Demo | Search"
                            error "hi" 
        FormFailure err -> error (show err)
        FormMissing  -> error "Not Implemented Yet2"