module Stop where

import Types
import Foundation

import Yesod.Core
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Data.IORef
import Control.Concurrent

putStopR :: Handler String
putStopR = do
    queryOpts <- requireCheckJsonBody :: Handler TygarQuery
    yesod <- getYesod
    tm <- liftIO $ readIORef $ threadMap yesod
    case Map.lookup (query_uuid queryOpts) tm of
        Nothing -> return "not found uuid"
        Just tid -> liftIO $ do
            killThread tid
            atomicModifyIORef (threadMap yesod)
                              (\m -> (Map.delete (query_uuid queryOpts) m, ()))
            return "thread killed"