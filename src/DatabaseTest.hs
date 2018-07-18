import Database.Generate
import System.Environment
import Data.List.Extra
import Data.Either

main = do
    [f] <- getArgs
    s   <- readFile f
    codes <- return $ splitOn "\n" s
    codes' <- return $ concat . rights . (map parseLine) $ codes
    print $ nub . (concatMap getNames) $ codes'