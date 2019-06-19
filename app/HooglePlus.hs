{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, NamedFieldPuns #-}

module Main (main) where

import Synquid.Logic
import Synquid.Type
import Synquid.Program
import Synquid.Error
import Synquid.Pretty
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Resolver (resolveDecls, ResolverState (..), initResolverState, resolveSchema)
import Synquid.SolverMonad
import Types.Generate hiding (files)
import Types.Experiments
import Types.Environment
import Types.Program
import Types.Solver
import Synquid.HtmlOutput
import Database.Presets
import Database.Environment (writeEnv, generateEnv)
import Database.Convert
import Database.Generate
import Database.Download
import Database.Util
import Synquid.Util (showme)
import HooglePlus.Synthesize
import HooglePlus.Stats
import Types.Encoder

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
import Control.Monad.Except (runExcept)
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Char
import Data.List
import Data.Foldable
import Data.Serialize
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
    Synthesis {file, libs, env_file_path_in, app_max, log_, sol_num,
      higher_order, use_refine, remove_duplicates, disable_demand,
      stop_refine, stop_threshold} -> do
      let searchParams = defaultSearchParams {
        _maxApplicationDepth = app_max,
        _explorerLogLevel = log_,
        _solutionCnt = sol_num,
        _useHO = higher_order,
        _refineStrategy = use_refine,
        _stopRefine = stop_refine,
        _threshold = stop_threshold,
        _shouldRemoveDuplicates = remove_duplicates,
        _disableDemand = disable_demand
        }
      let synquidParams = defaultSynquidParams {
        Main.envPath = env_file_path_in
      }
      executeSearch synquidParams searchParams file

    Generate {preset=(Just preset)} -> do
      let opts = case preset of
                    ICFPTotal -> genOptsTier1
                    ICFPPartial -> genOptsTier2
      precomputeGraph opts

    Generate Nothing files pkgs mdls d ho pathToEnv -> do
      let fetchOpts = if (length files > 0)
                        then Local files
                        else defaultHackageOpts {packages=pkgs}
      let generationOpts = defaultGenerationOpts {
        modules = mdls,
        instantiationDepth = d,
        enableHOF = ho,
        pkgFetchOpts = fetchOpts,
        Types.Generate.envPath = pathToEnv
      }
      precomputeGraph generationOpts

{- Command line arguments -}

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore" #-}

data CommandLineArgs
    = Synthesis {
        -- | Input
        file :: String,
        libs :: [String],
        env_file_path_in :: String,
        -- | Search params
        app_max :: Int,
        -- | Output
        log_ :: Int,
        -- | Graph params
        sol_num :: Int,
        higher_order :: Bool,
        use_refine :: RefineStrategy,
        stop_refine :: Bool,
        stop_threshold :: Int,
        remove_duplicates :: Bool,
        disable_demand :: Bool
      }
      | Generate {
        -- | Input
        preset :: Maybe Preset,
        files :: [String],
        pkg_name :: [String],
        module_name :: [String],
        type_depth :: Int,
        higher_order :: Bool,
        env_file_path_out :: String
      }
  deriving (Data, Typeable, Show, Eq)

synt = Synthesis {
  file                = ""              &= typFile &= argPos 0,
  libs                = []              &= args &= typ "FILES",
  env_file_path_in    = defaultEnvPath  &= help ("Environment file path (default:" ++ (show defaultEnvPath) ++ ")"),
  app_max             = 6               &= help ("Maximum depth of an application term (default: 6)") &= groupname "Explorer parameters",
  log_                = 0               &= help ("Logger verboseness level (default: 0)") &= name "l",
  sol_num             = 1               &= help ("Number of solutions need to find (default: 1)") &= name "cnt",
  higher_order        = False           &= help ("Include higher order functions (default: False)"),
  use_refine          = TyGarQ          &= help ("Use abstract refinement or not (default: TyGarQ)"),
  stop_refine         = False           &= help ("Stop refine the abstraction cover after some threshold (default: False)"),
  stop_threshold      = 10              &= help ("Refinement stops when the number of places reaches the threshold, only when stop_refine is True"),
  remove_duplicates   = False &= help ("Remove duplicates while searching. Under development."),
  disable_demand = False &= name "d" &= help ("Disable the demand analyzer (default: False)")
  } &= auto &= help "Synthesize goals specified in the input file"

generate = Generate {
  preset               = Nothing         &= help ("Environment preset to use"),
  files                = []              &= help ("Files to use to generate from. Exclusive with packages and modules. Takes precedence"),
  pkg_name             = []              &= help ("Package names to be generated"),
  module_name          = []              &= help ("Module names to be generated in the given packages"),
  type_depth           = 2               &= help ("Depth of the types to be instantiated for polymorphic type constructors"),
  higher_order         = True           &= help ("Include higher order functions (default: True)"),
  env_file_path_out    = defaultEnvPath  &= help ("Environment file path (default:" ++ (show defaultEnvPath) ++ ")")
} &= help "Generate the type conversion database for synthesis"

mode = cmdArgsMode $ modes [synt, generate] &=
  help (programName ++ " program synthesizer") &=
  program programName &=
  summary (programName ++ " v" ++ versionName ++ ", " ++ showGregorian releaseDate)

-- | Output format
data OutputFormat = Plain -- ^ Plain text
  | Ansi -- ^ Text with ANSI-terminal special characters
  | Html -- ^ HTML
  deriving (Typeable, Data, Eq, Show)

-- | 'printDoc' @format doc@ : print @doc@ to the console using @format@
printDoc :: OutputFormat -> Doc -> IO()
printDoc Plain doc = putDoc (plain doc) >> putStr "\n"
printDoc Ansi doc = putDoc doc >> putStr "\n"
printDoc Html doc = putStr (showDocHtml (renderPretty 0.4 100 doc))

-- | Parameters of the synthesis
data SynquidParams = SynquidParams {
    envPath :: String -- ^ Path to the environment file
}

defaultSynquidParams = SynquidParams {
    Main.envPath = defaultEnvPath
}

precomputeGraph :: GenerationOpts -> IO ()
precomputeGraph opts = generateEnv opts >>= writeEnv (Types.Generate.envPath opts)


-- | Parse and resolve file, then synthesize the specified goals
executeSearch :: SynquidParams -> SearchParams  -> String -> IO ()
executeSearch synquidParams searchParams query = do
  env <- readEnv
  goal <- envToGoal env query
  messageChan <- newChan
  worker <- forkIO $ synthesize searchParams goal messageChan
  readChan messageChan >>= (handleMessages messageChan)
  -- when (_explorerLogLevel searchParams > 0) (mapM_ (printTime . snd) results)
  return ()
  where
    logLevel = searchParams ^. explorerLogLevel
    readEnv = do
      let envPathIn = Main.envPath synquidParams
      doesExist <- doesFileExist envPathIn
      when (not doesExist) (error ("Please run `stack exec -- " ++ programName ++ " generate -p [PACKAGES]` to generate database first"))
      envRes <- decode <$> B.readFile envPathIn
      case envRes of
        Left err -> error err
        Right env ->
          return env

    handleMessages ch (MesgClose _) = putStrLn "Search complete" >> return ()
    handleMessages ch (MesgP (program, stats)) = do
      when (logLevel > 2) (pPrint stats)
      print program >> readChan ch >>= (handleMessages ch)
    handleMessages ch (MesgS debug) = do
      when (logLevel > 2) (pPrint debug)
      readChan ch >>= (handleMessages ch)
    handleMessages ch (MesgLog level tag msg) = do
      when (level <= logLevel) (do
        mapM (printf "[%s]: %s\n" tag) (lines msg)
        hFlush stdout)
      readChan ch >>= (handleMessages ch)

pdoc = printDoc Plain
