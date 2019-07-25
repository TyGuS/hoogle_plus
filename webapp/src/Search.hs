{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Search where

import Foundation
import Types
import Home
import HooglePlus.Synthesize (synthesize, envToGoal)
import Types.Experiments
import Types.Generate
import Types.Program
import Types.Type
import Types.Environment
import Synquid.Type
import Synquid.Pretty
import PetriNet.Util
import PetriNet.GHCChecker
import Database.Util

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.Timeout
import Control.Exception (catch)
import Control.Monad
import Data.Bits
import Data.ByteString.Lazy.Builder
import Data.List
import qualified Data.Aeson as Aeson
import Data.Serialize
import Data.Text (unpack)
import Data.Time
import Foreign.C.String
import qualified Data.Array as A
import qualified Data.ByteString.Lazy.Char8 as BChar
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import System.Directory
import System.IO
import System.Process
import Text.Lucius
import Text.Printf
import Text.Regex
import Text.Regex.Posix.Wrap
import Yesod.Core
import Yesod.Core.Content
import Yesod.Form
import Network.HTTP.Types.Status
import qualified Data.Map as Map
import Control.Lens

time_limit = 120 * 10^6 :: Int

runQuery :: TygarQuery -> IO (Chan Message, Goal)
runQuery queryOpts = do
    let query = typeSignature queryOpts
    env <- readEnv
    goal <- envToGoal env query
    messageChan <- newChan
    let params = defaultSearchParams {
          _stopRefine=True
        , _threshold=5
        , _solutionCnt = 10 }
    tid <- forkIO $ synthesize params goal messageChan
    forkIO $ threadDelay time_limit >> writeChan messageChan (MesgClose CSTimeout)
                                    >> killThread tid
    return (messageChan, goal)
    -- queryResults <- readChan messageChan >>= collectResults messageChan []
    where
        readEnv = do
            let envPathIn = defaultEnvPath
            doesExist <- doesFileExist envPathIn
            unless doesExist (error "No env file")
            envRes <- decode <$> B.readFile envPathIn
            case envRes of
                Left err -> error err
                Right env -> return env

transformSolution :: Goal -> RProgram -> IO ResultEntry
transformSolution goal queryResult = do
    let defaultOpt = compNewline .|. compExtended
    pkgRegex <- newCString "([^\\(\\ ]+)\\."
    pkgExtraction <- wrapCompile defaultOpt execBlank pkgRegex
    let pkgComp = case pkgExtraction of
                    Left err -> error $ snd err
                    Right r -> r
    let sol = toHaskellSolution $ show queryResult
    -- logQuery query sols
    getPkg pkgComp sol
    where
        logQuery q s = do
            time <- getCurrentTime
            let str = printf "[%s]: Query: %s Solutions: [%s]\n" (show time) q (intercalate ", " s)
            appendFile defaultLogFile str

        getRes str (offset, len) = take len $ drop offset str

        getPkg regex solution = do
            csol <- newCString solution
            pkgs <- wrapMatchAll regex csol
            case pkgs of
                Left err -> error $ snd err
                Right ps -> let
                    pkgNames = nub $ map (getRes solution . (A.! 1)) ps
                    stripSol = foldr (\p -> replaceId (p++".") "") solution pkgNames
                    goalTyp = toMonotype $ gSpec goal
                    argNames = argNamesOf goalTyp
                    lambdaSol = "\\" ++ unwords argNames ++ " -> " ++ stripSol
                    strQuery = showGoal goalTyp
                    in return (ResultEntry strQuery lambdaSol pkgNames)

        argNamesOf (FunctionT x tArg tRes)
            | tyclassArgBase `isPrefixOf` x = argNamesOf tRes
            | otherwise = x : argNamesOf tRes
        argNamesOf _ = []

        showGoal (FunctionT x tArg tRes)
            | isFunctionType tArg = x ++ ": (" ++ showGoal tArg ++ ") -> " ++ showGoal tRes
            | tyclassArgBase `isPrefixOf` x = showGoal tRes
            | otherwise = x ++ ": " ++ showGoal tArg ++ " -> " ++ showGoal tRes
        showGoal t = show t

postSearchR :: Handler TypedContent
postSearchR = do
    queryOpts <- requireCheckJsonBody :: Handler TygarQuery
    (chan, goal) <- liftIO $ runQuery queryOpts
    respondSource typeJson $ liftIO (readChan chan) >>= collectResults goal chan
    where
        collectResults goal ch (MesgClose _) = C.sinkNull
        collectResults goal ch (MesgP (program, _)) = do
            strProg <- liftIO $ transformSolution goal program
            liftIO $ print (BChar.unpack $ Aeson.encode strProg)
            C.yield $ C.Chunk $ lazyByteString $ Aeson.encode strProg
            sendFlush
            liftIO (readChan ch) >>= collectResults goal ch
        collectResults goal ch _ = liftIO (readChan ch) >>= collectResults goal ch

