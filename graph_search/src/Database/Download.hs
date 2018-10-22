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
import Network.HTTP.Types.Status
import Control.Monad.Extra
import System.Directory
import System.IO

import Database.Util

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

downloadFile :: PkgName -> Maybe Version -> IO Bool
downloadFile pkg version = do
    vpkg <- packageNameWithVersion pkg version
    doesExist <- doesFileExist $ downloadDir ++ vpkg ++ ".txt"
    if not doesExist 
        then do
            hPutStrLn stderr $ "Downloading file " ++ vpkg ++ " from Hackage..."
            request <- parseRequest $ docDownloadUrl ++ vpkg ++ "/docs/" ++ pkg ++ ".txt"
            manager <- newManager tlsManagerSettings
            runResourceT $ do
                response <- http request manager
                if responseStatus response /= ok200
                    then return False -- error "Connection Error"
                    else responseBody response $$+- sinkFile (downloadDir ++ vpkg ++ ".txt") >> return True
        else return True

downloadCabal :: PkgName -> Maybe Version -> IO Bool
downloadCabal pkg version = do
    vpkg <- packageNameWithVersion pkg version
    doesExist <- doesFileExist $ downloadDir ++ pkg ++ ".cabal"
    if not doesExist
        then do
            hPutStrLn stderr $ "Downloading cabal information of " ++ vpkg ++ " from Hackage..."
            request <- parseRequest $ docDownloadUrl ++ vpkg ++ "/" ++ pkg ++ ".cabal"
            manager <- newManager tlsManagerSettings
            runResourceT $ do
                response <- http request manager
                if responseStatus response /= ok200
                    then return False -- error "Connection Error"
                    else responseBody response $$+- sinkFile (downloadDir ++ pkg ++ ".cabal") >> return True
        else return True