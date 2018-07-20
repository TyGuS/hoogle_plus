{-# LANGUAGE TupleSections #-}

module Database.Download where

import System.FilePath
import System.Directory
import Control.Monad.Extra
import Data.Conduit.Binary (sinkFile)
import qualified Network.HTTP.Conduit as C
import Network.Connection
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource


-- | Download all the input files to input/
downloadInput :: Bool -> Maybe Bool -> FilePath -> String -> URL -> IO FilePath
downloadInput insecure download dir name url = do
    let file = dir </> "input-" ++ name
    exists <- doesFileExist file
    when (not exists && download == Just False) $
        error $ "File is not already downloaded and --download=no given, downloading " ++ url ++ " to " ++ file
    when (not exists || download == Just True) $ do
        downloadFile insecure (file <.> "part") url
        renameFile (file <.> "part") file
    return file

downloadFile :: Bool -> FilePath -> String -> IO ()
downloadFile insecure file url = do
    let request = C.parseRequest_ url
    manager <- C.newManager $ C.mkManagerSettings (TLSSettingsSimple insecure False False) Nothing
    runResourceT $ do
        response <- C.http request manager
        C.runConduit $ C.responseBody response C..| sinkFile file

readHaskellOnline :: Settings -> Download -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellOnline settings download = do
    stackage <- download "haskell-stackage.txt" "https://www.stackage.org/lts/cabal.config"
    platform <- download "haskell-platform.txt" "https://raw.githubusercontent.com/haskell/haskell-platform/master/hptool/src/Releases2015.hs"
    cabals   <- download "haskell-cabal.tar.gz" "https://hackage.haskell.org/packages/index.tar.gz"
    hoogles  <- download "haskell-hoogle.tar.gz" "https://hackage.haskell.org/packages/hoogle.tar.gz"

    -- peakMegabytesAllocated = 2
    setStackage <- Set.map strPack <$> setStackage stackage
    setPlatform <- Set.map strPack <$> setPlatform platform
    setGHC <- Set.map strPack <$> setGHC platform

    cbl <- parseCabalTarball settings cabals
    let want = Set.insert (strPack "ghc") $ Set.unions [setStackage, setPlatform, setGHC]
    cbl <- return $ flip Map.mapWithKey cbl $ \name p ->
        p{packageTags =
            [(strPack "set",strPack "included-with-ghc") | name `Set.member` setGHC] ++
            [(strPack "set",strPack "haskell-platform") | name `Set.member` setPlatform] ++
            [(strPack "set",strPack "stackage") | name `Set.member` setStackage] ++
            packageTags p}

    let source = do
            tar <- liftIO $ tarballReadFiles hoogles
            forM_ tar $ \(strPack . takeBaseName -> name, src) ->
                yield (name, hackagePackageURL name, src)
    return (cbl, want, source)