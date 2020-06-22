{-# LANGUAGE DeriveGeneric #-}

module Database.Download where

import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import Network.HTTP.Conduit
import Data.Conduit (runConduit, (.|), ($$+-))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Generics
import Data.Maybe
import Network.HTTP.Types.Status
import Control.Monad.Extra
import System.Directory
import System.IO

import Types.Generate
import Database.Utils
import Synquid.Utils (getTmpDir)

docVersionsUrl = "https://hackage.haskell.org/packages/docs"
docDownloadUrl = "https://hackage.haskell.org/package/"

-- | check whether the doc of the given version is available by querying to https://hackage.haskell.org/packages/docs
checkVersion :: PkgName -> Version -> IO Bool
checkVersion pkg version = do
    let vpkg = pkg ++ "-" ++ version
    hPutStrLn stderr $ "Checking availability for package " ++ vpkg
    availability <- simpleHttp docVersionsUrl
    let res = decode availability :: Maybe [(String, Bool)] -- the JSON format here is a list of (String, Bool) tuples
    case res of
        Nothing -> error "Connection error"
        Just arr | Map.findWithDefault False vpkg $ Map.fromList arr -> hPutStrLn stderr "package is available" >> return True
                 | otherwise -> error $ vpkg ++ " is not available"

packageNameWithVersion :: PkgName -> Maybe Version -> IO PkgName
packageNameWithVersion pkg version = case version of
    Nothing -> return pkg
    Just v  -> ifM (checkVersion pkg v) (return $ pkg ++ "-" ++ v) (return pkg)


-- getPkg downloads packages and their dependencies to files.
getPkg :: String -> IO [FilePath]
getPkg pkgName = do
    filePath <- downloadFile pkgName Nothing
    cabalPath <- downloadCabal pkgName Nothing
    if isNothing filePath || isNothing cabalPath
        then error $ "Problem downloading docs for: " ++ pkgName
        else return $ [fromJust filePath]


downloadFile :: PkgName -> Maybe Version -> IO (Maybe FilePath)
downloadFile pkg version = do
    downloadDir <- getTmpDir
    vpkg <- packageNameWithVersion pkg version
    let filepath = downloadDir ++ vpkg ++ ".txt"
    doesExist <- doesFileExist $ filepath
    if not doesExist
        then do
            hPutStrLn stderr $ "Downloading file " ++ vpkg ++ " from Hackage..."
            request <- parseRequest $ docDownloadUrl ++ vpkg ++ "/docs/" ++ pkg ++ ".txt"
            manager <- newManager tlsManagerSettings
            runResourceT $ do
                response <- http request manager
                if responseStatus response /= ok200
                    then return Nothing -- error "Connection Error"
                    else runConduit (responseBody response .| sinkFile filepath) >> return (Just filepath)
        else return (Just filepath)

downloadCabal :: PkgName -> Maybe Version -> IO (Maybe FilePath)
downloadCabal pkg version = do
    downloadDir <- getTmpDir
    vpkg <- packageNameWithVersion pkg version
    let filepath = downloadDir ++ pkg ++ ".cabal"
    doesExist <- doesFileExist $ filepath
    if not doesExist
        then do
            hPutStrLn stderr $ "Downloading cabal information of " ++ vpkg ++ " from Hackage..."
            request <- parseRequest $ docDownloadUrl ++ vpkg ++ "/" ++ pkg ++ ".cabal"
            manager <- newManager tlsManagerSettings
            runResourceT $ do
                response <- http request manager
                if responseStatus response /= ok200
                    then return Nothing -- error "Connection Error"
                    else runConduit (responseBody response .| sinkFile (downloadDir ++ pkg ++ ".cabal")) >> return (Just filepath)
        else return (Just filepath)

cleanTmpFiles :: PackageFetchOpts -> [FilePath] -> IO ()
cleanTmpFiles (Local {}) _ = return ()
cleanTmpFiles (Hackage{}) fs = do
    mapM_ removeFile fs
    return ()
