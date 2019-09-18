import Application -- for YesodDispatch instance
import Foundation
import Yesod.Core
import qualified Data.Map as Map
import Data.IORef

main :: IO ()
main = do
    threadMap <- newIORef Map.empty
    warp 3000 $ App threadMap
