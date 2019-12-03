module Stop where

import Types
import Foundation
import Util
import Types.Experiments

import Yesod.Core
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Lens
import Data.IORef
import System.Process

postStopR :: Handler String
postStopR = do
    queryOpts <- requireCheckJsonBody :: Handler TygarQuery
    let uuid = query_uuid queryOpts
    yesod <- getYesod
    tm <- liftIO $ readIORef $ threadMap yesod
    case Map.lookup uuid tm of
        Nothing -> return "not found uuid"
        Just (hdl, proc) -> liftIO $ do
            terminateProcess proc
            atomicModifyIORef (threadMap yesod)
                              (\m -> (Map.delete (query_uuid queryOpts) m, ()))
            endFile uuid hdl
            return "thread killed"
