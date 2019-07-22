{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Search where

import Foundation
import Types
import Home
import HooglePlus.Synthesize (synthesize, envToGoal)
import Types.Experiments
import Types.Generate
import Types.Program (RProgram)
import PetriNet.Util

import System.Directory
import Data.Serialize
import Control.Monad
import qualified Data.ByteString as B
import Text.Regex.Posix.Wrap
import System.Process
import Foreign.C.String
import Text.Regex
import qualified Data.Array as A
import Data.Bits
import Data.List
import Data.Text (unpack)
import Text.Printf
import System.IO
import Yesod.Core
import Text.Lucius
import Yesod.Form
import Data.Time

runQuery :: TygarQuery -> IO [ResultEntry]
runQuery queryOpts = do
    let query = typeSignature queryOpts
    let cmd = "python3"
    let opts = ["scripts/run_query.py", query]
    (exit, out, err) <- readProcessWithExitCode cmd opts ""
    let defaultOpt = compNewline .|. compExtended
    -- find the solutions in the stdout
    cregex <- newCString "SOLUTION\\:\\ (.+)"
    cout <- newCString out
    comp <- wrapCompile defaultOpt execBlank cregex
    let regex = case comp of
                  Left err -> error $ snd err
                  Right r -> r
    matches <- wrapMatchAll regex cout
    case matches of
        Left err -> error $ snd err
        Right res -> do
            -- transform the solutions into (solution, pkgs)
            pkgRegex <- newCString "([^\\(\\ ]+)\\."
            pkgExtraction <- wrapCompile defaultOpt execBlank pkgRegex
            let pkgComp = case pkgExtraction of
                            Left err -> error $ snd err
                            Right r -> r
            let sols = map (getRes out . (A.! 1)) res
            logQuery query sols
            mapM (getPkg pkgComp) sols
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
                    in return (ResultEntry stripSol pkgNames)

postSearchR :: Handler Value
postSearchR = do
    queryOpts <- requireCheckJsonBody :: Handler TygarQuery
    searchRes <- liftIO $ runQuery queryOpts
    returnJson $ TygarResult searchRes
