{-# LANGUAGE ScopedTypeVariables #-}

import Database.Generate
import System.Environment
import Data.List.Extra
import Data.Either
import Control.Monad.State
import Language.Haskell.Exts
import Database.Succinct
import Synquid.Type
import Synquid.Pretty

printSigs decl = case decl of
    EDecl (TypeSig _ names ty) -> do
        signature <- return $ show $ evalState (toSynquidSchema ty) 0
        putStrLn $ (nameStr ((!!) names 0)) ++ " :: " ++ signature
        -- print decl
    EDecl decl -> print decl
    EPackage pkg -> putStrLn pkg
    EModule mdl -> putStrLn mdl

main = do
    [f] <- getArgs
    s   <- readFile f
    codes <- return $ splitOn "\n" s
    codes' <- return $ concat . rights . (map parseLine) $ codes
    -- typExample <- return $ (!!) codes' (read g :: Int)
    mapM_ printSigs codes'
    