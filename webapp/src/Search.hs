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
import Synquid.Program
import PetriNet.Util
import PetriNet.GHCChecker
import Database.Util

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (catch)
import Control.Monad
import Data.Bits
import Data.ByteString.Lazy.Builder
import Data.List
import qualified Data.Aeson as Aeson
import Data.Serialize
import Data.Text (unpack)
import Data.Time
import Data.Maybe
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
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Data.IORef
import Control.Concurrent.Async

runQuery :: TygarQuery -> IORef (Map String (Chan Message, ThreadId)) -> IO (Chan Message, Goal)
runQuery queryOpts tm = do
    let query = typeSignature queryOpts
    env <- readEnv
    goal <- envToGoal env query
    messageChan <- newChan
    let params = defaultSearchParams {
          _stopRefine = True
        , _threshold = 10
        , _solutionCnt = 10 }
    tid <- forkIO $ synthesize params goal messageChan
    -- tid <- asyncThreadId asyncId
    atomicModifyIORef tm (\m -> (Map.insert (query_uuid queryOpts) (messageChan, tid) m, ()))
    forkIO $ threadDelay time_limit >> writeChan messageChan (MesgClose CSTimeout) >> killThread tid
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

transformSolution :: Goal -> RProgram -> IO (String -> ResultEntry)
transformSolution goal queryResult = do
    print queryResult
    let defaultOpt = compNewline .|. compExtended
    pkgRegex <- newCString "([^\\(\\ ]+)\\."
    pkgExtraction <- wrapCompile defaultOpt execBlank pkgRegex
    let pkgComp = case pkgExtraction of
                    Left err -> error $ snd err
                    Right r -> r
    let sol = toHaskellSolution $ show queryResult
    logQuery (show $ toMonotype $ gSpec goal) sol
    getPkg pkgComp sol
    where
        getRes str (offset, len) = take len $ drop offset str

        getPkg regex solution = do
            csol <- newCString solution
            pkgs <- wrapMatchAll regex csol
            case pkgs of
                Left err -> error $ snd err
                Right ps -> let
                    pkgNames = nub $ map (getRes solution . (A.! 1)) ps
                    env = gEnvironment goal
                    htmlCode = toHtml env queryResult
                    stripSol = foldr (\p -> replaceId (p++".") "") htmlCode pkgNames
                    goalTyp = toMonotype $ gSpec goal
                    argNames = argNamesOf goalTyp
                    lambdaSol = "\\" ++ unwords argNames ++ " -> " ++ stripSol
                    strQuery = showGoal goalTyp
                    in return (ResultEntry strQuery lambdaSol pkgNames)

        argNamesOf (FunctionT x tArg tRes)
            | tyclassArgBase `isPrefixOf` x = argNamesOf tRes
            | otherwise = x : argNamesOf tRes
        argNamesOf _ = []

        tooltip = "<span class=\"my-tooltip\" data-toggle=\"tooltip\" title data-html=\"true\" data-original-title=\"<code>%s :: %s</code>\">%s</span>"

        toHtml env (Program (PSymbol "Nil") _) = printf tooltip ("[]" :: String) ("[a]" :: String) ("[]" :: String)
        toHtml env (Program (PSymbol s) t)
            | tyclassArgBase `isPrefixOf` s = ""
            | otherwise = case lookupSymbol s 0 env of
                Just sch -> printf tooltip s (removeTypeclasses $ show (toMonotype sch)) s
                Nothing -> error $ "cannot find symbol " ++ s
        toHtml env (Program (PApp f args) _) = let
            optParens p = case p of
                Program (PSymbol _) _ -> " " ++ toHtml env p
                _ -> " (" ++ toHtml env p ++ ")"
            in (case f of
                "Cons" -> printf tooltip ("(:)" :: String) ("a -> [a] -> [a]" :: String) ("(:)" :: String)
                "Pair" -> printf tooltip ("(,)" :: String) ("a -> b -> (a, b)" :: String) ("(,)" :: String)
                _      -> case lookupSymbol f 0 env of
                    Just sch -> printf tooltip f (removeTypeclasses $ show (toMonotype sch)) f
                    Nothing -> error $ "cannot find symbol " ++ f
                ) ++ concatMap optParens args

        removeTypeclasses = go (mkRegex (tyclassPrefix ++ "([a-zA-Z\\ ]*)\\->\\ "))

        go regex input =
            if (isJust $ matchRegex regex input)
            then (go regex $ subRegex regex input "\\1 => ")
            else input

        showGoal t@(FunctionT x tArg tRes)
            | isFunctionType tArg = x ++ ": (" ++ showGoal tArg ++ ") -> " ++ showGoal tRes
            | tyclassArgBase `isPrefixOf` x = let
                allTyclass tt = case tt of
                                    FunctionT x' tArg' tRes' ->
                                        if tyclassArgBase `isPrefixOf` x'
                                            then let
                                                ScalarT (DatatypeT id [ScalarT (TypeVarT _ tyvarName) _] _) _ = tArg'
                                                (res, tRes'') = allTyclass tRes'
                                                classNameRegex = mkRegex $ tyclassPrefix ++ "([a-zA-Z]*)"
                                                className = subRegex classNameRegex id "\\1"
                                                constraint = className ++ " " ++ tyvarName
                                                in (constraint : res, tRes'')
                                            else ([], tt)
                                    _ -> ([], tt)
                (constraints, t') = allTyclass t
                in "(" ++ intercalate ", " constraints ++ ") => " ++ showGoal t'
            | otherwise = x ++ ": " ++ showGoal tArg ++ " -> " ++ showGoal tRes
        showGoal t = show t

logQuery :: String -> String -> IO ()
logQuery q s = do
    time <- getCurrentTime
    let str = printf "[%s]: Query: %s Solution: %s\n" (show time) q s
    appendFile defaultLogFile str

postSearchR :: Handler TypedContent
postSearchR = do
    queryOpts <- requireCheckJsonBody :: Handler TygarQuery
    yesod <- getYesod
    (chan, goal) <- liftIO $ runQuery queryOpts $ threadMap yesod
    respondSource typeJson $ liftIO (readChan chan) >>= collectResults goal (query_uuid queryOpts) chan
    where
        collectResults goal uuid ch (MesgClose _) = C.sinkNull
        collectResults goal uuid ch (MesgP (program, _)) = do
            strProg <- liftIO $ transformSolution goal program
            let prog = strProg uuid
            liftIO $ print (BChar.unpack $ Aeson.encode prog)
            C.yield $ C.Chunk $ lazyByteString $ Aeson.encode prog
            sendFlush
            liftIO (readChan ch) >>= collectResults goal uuid ch
        -- collectResults goal uuid ch (MesgLog lv name log) = when (lv <= 1) (liftIO (putStrLn (printf "[%s]: %s" name log)))
        --                                                   >> liftIO (readChan ch) >>= collectResults goal uuid ch
        collectResults goal uuid ch _ = liftIO (readChan ch) >>= collectResults goal uuid ch