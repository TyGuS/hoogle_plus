{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Application -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Yesod.Static
import qualified Data.Map as Map
import Data.IORef
import System.Console.CmdArgs.Implicit

data Args = Args {
    portNumArg :: Int
} deriving (Show, Data, Typeable)

defaultArgs = Args {
    portNumArg=3000 &= name "port"
}

main :: IO ()
main = do
    res <- cmdArgs defaultArgs
    threadMap <- newIORef Map.empty
    static@(Static settings) <- static "webapp/assets"
    warp (portNumArg res) $ App threadMap static
