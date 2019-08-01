module Stop where

import Types
import Foundation
import Types.Experiments

import Yesod.Core
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Data.IORef
import Control.Concurrent.Chan

putStopR :: Handler String
putStopR = do
    queryOpts <- requireCheckJsonBody :: Handler TygarQuery
    yesod <- getYesod
    tm <- liftIO $ readIORef $ threadMap yesod
    case Map.lookup (query_uuid queryOpts) tm of
        Nothing -> return "not found uuid"
        Just chan -> liftIO $ do
            writeChan chan (MesgClose CSTimeout)
            atomicModifyIORef (threadMap yesod)
                              (\m -> (Map.delete (query_uuid queryOpts) m, ()))
            return "thread killed"