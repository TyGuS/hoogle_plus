{-# LANGUAGE DeriveGeneric #-}

module Database.Download where

import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import Network.HTTP.Conduit
import Data.Conduit (runConduit, (.|), ($$+-))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Generics
import Data.Maybe
import Data.List
import Network.HTTP.Types.Status
import Control.Monad.Extra
import System.Directory
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Package
import Distribution.ModuleName

import Database.Util

docVersionsUrl = "https://hackage.haskell.org/packages/docs"
docDownloadUrl = "https://hackage.haskell.org/package/"

-- | check whether the doc of the given version is available by querying to https://hackage.haskell.org/packages/docs
checkVersion :: PkgName -> Version -> IO Bool
checkVersion pkg version = do
    let vpkg = pkg ++ "-" ++ version
    putStrLn $ "Checking availability for package " ++ vpkg
    availability <- simpleHttp docVersionsUrl
    let res = decode availability :: Maybe [(String, Bool)] -- the JSON format here is a list of (String, Bool) tuples
    case res of
        Nothing -> error "Connection error"
        Just arr | Map.findWithDefault False vpkg $ Map.fromList arr -> putStrLn "package is available" >> return True
                 | otherwise -> error $ vpkg ++ " is not available"

packageNameWithVersion :: PkgName -> Maybe Version -> IO PkgName
packageNameWithVersion pkg version = case version of
    Nothing -> return pkg
    Just v  -> ifM (checkVersion pkg v) (return $ pkg ++ "-" ++ v) (return pkg)

downloadFile :: String -> FilePath -> IO Bool
downloadFile url dst = do
    doesExist <- doesFileExist dst
    if not doesExist
        then do
            putStrLn $ "Downloading file from " ++ url ++ "..."
            request <- parseRequest url
            manager <- newManager tlsManagerSettings
            runResourceT $ do
                response <- http request manager
                if responseStatus response /= ok200
                    then return False -- error "Connection Error"
                    else runConduit (responseBody response .| sinkFile dst) >> return True
        else return True

downloadCabal :: PkgName -> Maybe Version -> IO Bool
downloadCabal pkg version = do
    vpkg <- packageNameWithVersion pkg (if pkg == "base" then Just "4.9.0.0" else version)
    doesExist <- doesFileExist $ downloadDir ++ pkg ++ ".cabal"
    if not doesExist
        then do
            putStrLn $ "Downloading cabal information of " ++ vpkg ++ " from Hackage..."
            request <- parseRequest $ docDownloadUrl ++ vpkg ++ "/" ++ pkg ++ ".cabal"
            manager <- newManager tlsManagerSettings
            runResourceT $ do
                response <- http request manager
                if responseStatus response /= ok200
                    then return False -- error "Connection Error"
                    else runConduit (responseBody response .| sinkFile (downloadDir ++ pkg ++ ".cabal")) >> return True
        else return True

-- | get all the exposed modules from the cabal file
downloadPkgIndex :: PkgName -> Maybe Version -> IO [String]
downloadPkgIndex pkg version = do
    putStrLn $ "Downloading package indices for " ++ pkg
    ifM (downloadCabal pkg version)
        (do
            gPackageDesc <- readGenericPackageDescription silent $ downloadDir ++ pkg ++ ".cabal"
            case condLibrary gPackageDesc of
                Nothing -> putStrLn "Nothing here" >> return []
                Just (CondNode lib _ _) -> return $ map moduleNameStr $ exposedModules lib
        )
        (return [])
  where
    moduleNameStr mn = intercalate "." $ components mn

-- | download all the exposed source code from Hackage for parse preparations
downloadPkgSource :: PkgName -> Maybe Version -> IO [String]
downloadPkgSource pkg version = do
    mdls <- downloadPkgIndex pkg version
    putStrLn $ "Downloading source code for package " ++ pkg
    mapM_ (\mdl -> do
        let url = docDownloadUrl ++ (if pkg == "base" then "base-4.9.0.0" else pkg) ++ "/docs/src/" ++ mdl ++ ".html"
        let dst = downloadDir ++ mdl
        downloadFile url dst
        ) mdls
    return mdls