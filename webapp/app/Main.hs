import Application -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Yesod.Static
import qualified Data.Map as Map
import Data.IORef

main :: IO ()
main = do
    threadMap <- newIORef Map.empty
    static@(Static settings) <- static "webapp/assets"
    warp 3000 $ App threadMap static
