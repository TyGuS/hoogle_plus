{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- generate databases
import Database.Convert
import Database.Download
import Database.Environment
import Database.Generate
import Database.Presets
import Database.Utils
-- encoders
import Datalog.Formulog
import Datalog.Souffle
import Encoder.CBCEnc ()
import Encoder.CBCTypes (CBCState)
import Encoder.PrologEnc ()
import Encoder.PrologTypes (PrologState)
import Encoder.Z3SATEnc ()
import Encoder.Z3SATTypes (Z3SATState)
import Encoder.Z3SMTEnc ()
import Encoder.Z3SMTTypes (Z3SMTState)
-- evaluation
import Evaluation.EvalTypeInf
import Evaluation.ReadBenchmark
-- synthesis with examples
import Examples.ExampleChecker
import HooglePlus.GHCChecker
import HooglePlus.IOFormat
import HooglePlus.Stats
import HooglePlus.Synthesize
import HooglePlus.Utils
import Synquid.Error
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver (resolveDecls, ResolverState (..), initResolverState, resolveSchema)
import Synquid.Type
import Synquid.Utils (showme)
import Types.Environment
import Types.Experiments
import Types.Filtering
import Types.Generate hiding (files)
import Types.IOFormat
import Types.Program
import Types.Solver
import Types.Type

import Control.Concurrent
import Control.Exception
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.State (runState, evalStateT, execStateT, evalState)
import Control.Monad.Logic (observeT)
import Data.Char
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.List.Split
import Data.Map ((!))
import Data.Maybe (mapMaybe, fromJust)
import Data.Time.Calendar
import Distribution.PackDeps
import Language.Haskell.Exts (Decl(TypeSig))
import System.Console.ANSI
import System.Console.CmdArgs hiding (Normal)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Pretty.Simple
import Text.PrettyPrint.ANSI.Leijen (fill, column)
import Text.Printf
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.ANSI.Leijen as PP

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
                  , solver_name
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
                        , _solver = solver_name
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
        Generate {preset = (Just preset)} -> precomputeGraph (getOptsFromPreset preset)
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
        Evaluation benchmark fp -> readSuite fp >>= runTypeInferenceEval

{- Command line arguments -}

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}

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
        disable_filter :: Bool,
        solver_name :: SolverName
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
  disable_filter      = True            &= help ("Disable filter-based test"),
  solver_name         = Z3SMT           &= help ("Constraint solver used in petri net search")
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
precomputeGraph opts = do
    env <- generateEnv opts
    writeEnv (Types.Generate.envPath opts) env
    writeSouffle env
    writeFormulog env

-- | Parse and resolve file, then synthesize the specified goals
executeSearch :: SynquidParams -> SearchParams -> String -> IO ()
executeSearch synquidParams searchParams inStr = catch (do
    let input = decodeInput (LB.pack inStr)
    let tquery = query input
    let exquery = inExamples input
    env' <- readEnv $ Types.Experiments.envPath synquidParams
    env <- readBuiltinData synquidParams env'
    goal <- envToGoal env tquery
    case searchParams ^. solver of
        Z3SMT -> synthesize searchParams goal exquery (emptySolverState :: SolverState Z3SMTState)
        Z3SAT -> synthesize searchParams goal exquery (emptySolverState :: SolverState Z3SATState)
        CBC -> synthesize searchParams goal exquery (emptySolverState :: SolverState CBCState)
        Prolog -> synthesize searchParams goal exquery (emptySolverState :: SolverState PrologState)
        Souffle -> observeT $ runSouffle searchParams (gEnvironment goal) (gSpec goal) exquery 0
        _ -> error "not implemented"
    )
    (\(e :: SomeException) -> printResult $ encodeWithPrefix $ QueryOutput [] (show e) [])
