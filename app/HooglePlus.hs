{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Synquid.Logic
import Synquid.Type
import Synquid.Program
import Synquid.Error
import Synquid.Pretty
import Types.Generate hiding (files)
import Types.Experiments
import Types.IOFormat
import Types.Environment
import Types.Program
import Types.Solver
import Types.Type
import Database.Presets
import Database.Environment
import Database.Convert
import Database.Generate
import Database.Download
import Database.Util
import Synquid.Util (showme)
import TopDown.Synthesize
-- import HooglePlus.Synthesize
import HooglePlus.Stats
import Types.Encoder
import HooglePlus.GHCChecker
import HooglePlus.Utils
import HooglePlus.IOFormat
import Examples.ExampleChecker
import Types.Filtering
import Evaluation.EvalTypeInf
import Evaluation.ReadBenchmark

import Control.Exception
import Control.Monad
import Control.Lens ((^.))
import System.Exit
import System.Console.CmdArgs hiding (Normal)
import System.Console.ANSI
import System.FilePath
import Text.Parsec.Pos
import Text.Printf
import Text.Pretty.Simple
import Control.Monad.State (runState, evalStateT, execStateT, evalState)
import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Aeson as Aeson
import Data.Char
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Time.Calendar
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Language.Haskell.Exts (Decl(TypeSig))
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromJust)
import Distribution.PackDeps
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import System.Directory
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen (fill, column)

import Data.List.Split

programName = "hoogleplus"
versionName = "0.1"
releaseDate = fromGregorian 2019 3 10

-- | Type-check and synthesize a program, according to command-line arguments
main = do
    res <- cmdArgsRun $ mode
    case res of
        Synthesis { file
                  , json
                  , search_type
                  , env_file_path_in
                  , app_max
                  , log_
                  , sol_num
                  , disable_higher_order
                  , use_refine
                  , disable_demand
                  , stop_refine
                  , stop_threshold
                  , get_n_examples
                  , get_n_types
                  , disable_coalescing
                  , coalescing_strategy
                  , incremental
                  , disable_relevancy
                  , disable_copy_trans
                  , disable_blacklist
                  , disable_filter
                  } -> do
            let searchParams =
                    defaultSearchParams
                        { _maxApplicationDepth = app_max
                        , _explorerLogLevel = log_
                        , _solutionCnt = sol_num
                        , _useHO = not disable_higher_order
                        , _stopRefine = stop_refine
                        , _threshold = stop_threshold
                        , _incrementalSolving = incremental
                        , _refineStrategy = use_refine
                        , _disableDemand = disable_demand
                        , _coalesceTypes = not disable_coalescing
                        , _coalesceStrategy = coalescing_strategy
                        , _disableRelevancy = disable_relevancy
                        , _disableCopy = disable_copy_trans
                        , _disableBlack = disable_blacklist
                        , _disableFilter = disable_filter
                        }
            let synquidParams =
                    defaultSynquidParams {Types.Experiments.envPath = env_file_path_in}
            let searchPrograms = case (file, json) of
                                   ("", "") -> error "Must specify a file path or a json string"
                                   ("", json) -> executeSearch synquidParams searchParams json
                                   (f, _) -> readFile f >>= executeSearch synquidParams searchParams
            case search_type of
              SearchPrograms -> searchPrograms
              SearchTypes -> searchTypes synquidParams json get_n_types >> return ()
              SearchResults -> searchResults synquidParams json
              SearchExamples -> searchExamples synquidParams json get_n_examples
        Generate {preset = (Just preset)} -> do
            precomputeGraph (getOptsFromPreset preset)
        Generate Nothing files pkgs mdls d ho pathToEnv hoPath -> do
            let fetchOpts =
                    if (length files > 0)
                        then Local files
                        else defaultHackageOpts {packages = pkgs}
            let generationOpts =
                    defaultGenerationOpts
                        { modules = mdls
                        , instantiationDepth = d
                        , enableHOF = ho
                        , pkgFetchOpts = fetchOpts
                        , Types.Generate.envPath = pathToEnv
                        , Types.Generate.hoPath = hoPath
                        }
            precomputeGraph generationOpts
        Evaluation benchmark fp -> do
            readSuite fp >>= runTypeInferenceEval

{- Command line arguments -}

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore" #-}

data CommandLineArgs
    = Synthesis {
        -- | Input
        file :: String,
        json :: String,
        search_type :: QueryType,
        env_file_path_in :: String,
        -- | Search params
        app_max :: Int,
        -- | Output
        log_ :: Int,
        -- | Graph params
        sol_num :: Int,
        disable_higher_order :: Bool,
        use_refine :: RefineStrategy,
        stop_refine :: Bool,
        stop_threshold :: Int,
        get_n_examples :: Int,
        get_n_types :: Int,
        disable_demand :: Bool,
        disable_coalescing :: Bool,
        incremental :: Bool,
        coalescing_strategy :: CoalesceStrategy,
        disable_relevancy :: Bool,
        disable_copy_trans :: Bool,
        disable_blacklist :: Bool,
        disable_filter :: Bool
      }
      | Generate {
        -- | Input
        preset :: Maybe Preset,
        files :: [String],
        pkg_name :: [String],
        module_name :: [String],
        type_depth :: Int,
        higher_order :: Bool,
        env_file_path_out :: String,
        ho_path :: String
      }
      | Evaluation {
        benchmark :: String,
        file_path :: String
      }
  deriving (Data, Typeable, Show, Eq)

synt = Synthesis {
  file                = ""              &= typFile &= help ("Input query from the specified file path, file should be in json format"),
  json                = ""              &= help ("Input query from a json string"),
  search_type         = SearchPrograms  &= help ("For web demo"),
  env_file_path_in    = defaultEnvPath  &= help ("Environment file path (default:" ++ (show defaultEnvPath) ++ ")"),
  app_max             = 6               &= help ("Maximum depth of an application term (default: 6)") &= groupname "Explorer parameters",
  log_                = 0               &= help ("Logger verboseness level (default: 0)") &= name "l",
  sol_num             = 1               &= help ("Number of solutions need to find (default: 1)") &= name "cnt",
  disable_higher_order= False           &= help ("Disable higher order functions (default: False)"),
  use_refine          = TyGarQ          &= help ("Use abstract refinement or not (default: TyGarQ)"),
  stop_refine         = True            &= help ("Stop refine the abstraction cover after some threshold (default: False)"),
  stop_threshold      = 10              &= help ("Refinement stops when the number of places reaches the threshold, only when stop_refine is True"),
  get_n_examples      = 1               &= help ("Number of more examples (used only for web mode)"),
  get_n_types         = 10              &= help ("Number of inferred types (used only for web mode)"),
  incremental         = False           &= help ("Enable the incremental solving in z3 (default: False)"),
  disable_demand      = False           &= name "d" &= help ("Disable the demand analyzer (default: False)"),
  disable_coalescing  = False           &= name "xc" &= help ("Do not coalesce transitions in the net with the same abstract type"),
  coalescing_strategy = First           &= help ("Choose how type coalescing works. Default: Pick first element of each group set."),
  disable_relevancy   = False           &= help ("Disable the relevancy requirement for argument types (default: False)"),
  disable_copy_trans  = False           &= help ("Disable the copy transitions and allow more than one token in initial state instead (default: False)"),
  disable_blacklist   = False           &= help ("Disable blacklisting functions in the solution (default: False)"),
  disable_filter      = True            &= help ("Disable filter-based test")
  } &= auto &= help "Synthesize goals specified in the input file"

generate = Generate {
  preset               = Nothing         &= help ("Environment preset to use"),
  files                = []              &= help ("Files to use to generate from. Exclusive with packages and modules. Takes precedence"),
  pkg_name             = []              &= help ("Package names to be generated"),
  module_name          = []              &= help ("Module names to be generated in the given packages"),
  type_depth           = 2               &= help ("Depth of the types to be instantiated for polymorphic type constructors"),
  higher_order         = True            &= help ("Include higher order functions (default: True)"),
  env_file_path_out    = defaultEnvPath  &= help ("Environment file path (default:" ++ (show defaultEnvPath) ++ ")"),
  ho_path              = "ho.txt"        &= typFile &= help ("Filename of components to be used as higher order arguments")
} &= help "Generate the type conversion database for synthesis"

evaluation = Evaluation {
  benchmark            = ""              &= help ("Evaluate this single benchmark"),
  file_path            = ""              &= help ("Path to the benchmark file, in the format of YAML")
} &= help "Evaluate Hoogle+ modules"

mode = cmdArgsMode $ modes [synt, generate, evaluation] &=
  help (programName ++ " program synthesizer") &=
  program programName &=
  summary (programName ++ " v" ++ versionName ++ ", " ++ showGregorian releaseDate)



precomputeGraph :: GenerationOpts -> IO ()
precomputeGraph opts = generateEnv opts >>= writeEnv (Types.Generate.envPath opts)


-- | Parse and resolve file, then synthesize the specified goals
executeSearch :: SynquidParams -> SearchParams -> String -> IO ()
executeSearch synquidParams searchParams inStr = catch (do
  let input = decodeInput (LB.pack inStr)
  let tquery = query input
  let exquery = inExamples input
  env' <- readEnv $ Types.Experiments.envPath synquidParams
  env <- readBuiltinData synquidParams env'
  goal <- envToGoal env tquery
  solverChan <- newChan
  checkerChan <- newChan
  forkIO $ synthesize searchParams goal exquery solverChan
  readChan solverChan >>= (handleMessages solverChan))
  (\(e :: SomeException) -> printResult $ encodeWithPrefix $ QueryOutput [] (show e) [])
  where
    logLevel = searchParams ^. explorerLogLevel

    handleMessages ch (MesgClose _) = when (logLevel > 0) (putStrLn "Search complete") >> return ()
    handleMessages ch (MesgP (out, stats, _)) = do
      when (logLevel > 0) $ printf "[writeStats]: %s\n" (show stats)
      -- printSolution program
      printResult $ encodeWithPrefix out
      hFlush stdout
      readChan ch >>= (handleMessages ch)
    handleMessages ch (MesgS debug) = do
      when (logLevel > 1) $ printf "[writeStats]: %s\n" (show debug)
      readChan ch >>= (handleMessages ch)
    handleMessages ch (MesgLog level tag msg) = do
      when (level <= logLevel) (do
        mapM (printf "[%s]: %s\n" tag) (lines msg)
        hFlush stdout)
      readChan ch >>= (handleMessages ch)
