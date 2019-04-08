{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Search where

import Foundation
import Yesod.Core
import Text.Lucius
import Yesod.Form
import Types hiding (modules)
import Data.Text (Text)
import Home
import HooglePlus.Synthesize (synthesize, envToGoal)

import Database.Environment (newGenerateEnv)

import Types.Experiments
import Types.Generate-- (defaultGenerationOpts, PackageFetchOpts(..), GenerationOpts)
import Synquid.Error
import Types.Encoder


-- Imports to remove once mj_benchmark gets merged
--import Synquid.Resolver (_environment)--remove later
--import Types.Environment (Environment) -- remove later
--import Types.Program  (BareDeclaration( .. ), Goal(..), gEnvironment, gSpec)-- remove later
--import Text.Parsec.Pos (initialPos)
--import Control.Monad.Trans.State.Lazy (evalStateT, evalState)
--import qualified Data.ByteString as B -- remove later
--import Text.PrettyPrint.ANSI.Leijen.Internal (putDoc, pretty, empty)
--import System.Directory (doesFileExist)
--import Data.Serialize (decode)
--import System.Exit (exitFailure)
--import Control.Monad.Trans.Except (runExcept)
--import Synquid.Parser (toErrorMessage, parseProgram)
--import GHC.Base (when)
--import Synquid.Resolver (initResolverState, resolveSchema)
--import Text.Parsec.Indent (runIndentParserT)
--programName = "hoogleplus" -- remove later

--defaultSearchParams = SearchParams {
--  _eGuessDepth = 3,
--  _sourcePos = noPos,
--  _explorerLogLevel = 0,
--  _solutionCnt = 1,
--  _pathSearch = PetriNet,
--  _useHO = False,
--  _encoderType = Normal,
--  _useRefine = QueryRefinement
--}

test = do
    env <- newGenerateEnv genOptsTier1
    goal <- envToGoal env "a -> a -> a"
    print goal
    print "before synthezie"
    a <- synthesize defaultSearchParams goal
    print "after synthesize"
    return ()

postSearchR :: Handler Html
postSearchR = do
    -- Move this line below back to Home. HOW CAN I MAKE THIS WORK W/ TOKEN?
    liftIO $ test 
    ((res, formWidget), formEnctype) <- runFormPostNoToken searchForm
    case res of
        FormSuccess _ -> defaultLayout $ do
                            --setTitle "TYGAR Demo | Search"
                            error "hi" 
        FormFailure err -> error (show err)
        FormMissing  -> error "Not Implemented Yet2"

genOptsTier1 = defaultGenerationOpts {
  modules = myModules,
  pkgFetchOpts = Local {
      files = ["libraries/tier1/base.txt", "libraries/tier1/bytestring.txt", "libraries/ghc-prim.txt"]
      }
  }

myModules = [
  -- base
  "Data.Int",
  "Data.Bool",
  "Data.Maybe",
  "Data.Either",
  "Data.Tuple",
  "GHC.Char",
  "Text.Show",
  -- ByteString
  "Data.ByteString.Lazy",
  "Data.ByteString.Builder"
  ]