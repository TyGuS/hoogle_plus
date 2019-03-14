{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Main (main) where

import Synquid.Logic
import Synquid.Type
import Synquid.Program
import Synquid.Error
import Synquid.Pretty
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Resolver (resolveDecls, ResolverState (..), initResolverState, resolveSchema)
import Synquid.SolverMonad
import Types.Experiments
import Types.Environment
import Types.Program
import Types.Solver
import Synquid.HtmlOutput
import Database.Environment (writeEnv, generateEnv)
import Database.Convert
import Database.Generate
import Database.Download
import Database.Util
import Synquid.Util (showme)
import HooglePlus.Synthesize
import qualified PetriNet.PNSolver as PNS
import Types.Encoder

import Control.Monad
import Control.Lens ((^.))
import System.Exit
import System.Console.CmdArgs hiding (Normal)
import System.Console.ANSI
import System.FilePath
import Text.Parsec.Pos
import Control.Monad.State (runState, evalStateT, execStateT, evalState)
import Control.Monad.Except (runExcept)
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
    (Synthesis file libs envPath appMax log_ sol_num path_search higher_order encoder refine) -> do
                  let searchParams = defaultSearchParams {
                    _eGuessDepth = appMax,
                    _explorerLogLevel = log_,
                    _solutionCnt = sol_num,
                    _pathSearch = path_search,
                    _useHO = higher_order,
                    _encoderType = encoder,
                    _useRefine = refine
                    }
                  let synquidParams = defaultSynquidParams {
                    envPath = envPath
                  }
                  runOnFile synquidParams searchParams file
    (Generate pkgs mdls d ho envPath) -> do
                  precomputeGraph pkgs mdls d ho envPath
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
        path_search :: PathStrategy,
        higher_order :: Bool,
        encoder :: EncoderType,
        use_refine :: PNS.RefineStrategy
      }
      | Generate {
        -- | Input
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
  path_search         = PetriNet     &= help ("Use path search algorithm to ensure the usage of provided parameters (default: PetriNet)") &= name "path",
  higher_order        = False           &= help ("Include higher order functions (default: False)"),
  encoder             = Normal &= help ("Choose normal or refined arity encoder (default: Normal)"),
  use_refine          = PNS.QueryRefinement    &= help ("Use abstract refinement or not (default: QueryRefinement)")
  } &= auto &= help "Synthesize goals specified in the input file"

generate = Generate {
  pkg_name             = []              &= help ("Package names to be generated"),
  module_name          = []              &= help ("Module names to be generated in the given packages"),
  type_depth           = 2               &= help ("Depth of the types to be instantiated for polymorphic type constructors"),
  higher_order         = False           &= help ("Include higher order functions (default: False)"),
  env_file_path_out    = defaultEnvPath  &= help ("Environment file path (default:" ++ (show defaultEnvPath) ++ ")")
} &= help "Generate the type conversion database for synthesis"

mode = cmdArgsMode $ modes [synt, generate] &=
  help (programName ++ " program synthesizer") &=
  program programName &=
  summary (programName ++ " v" ++ versionName ++ ", " ++ showGregorian releaseDate)

-- | Parameters for template exploration
defaultSearchParams = SearchParams {
  _eGuessDepth = 3,
  _sourcePos = noPos,
  _explorerLogLevel = 0,
  _solutionCnt = 1,
  _pathSearch = PetriNet,
  _useHO = False,
  _encoderType = Normal,
  _useRefine = PNS.QueryRefinement
}

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
    envPath = defaultEnvPath
}

precomputeGraph :: [PkgName] -> [String] -> Int -> Bool -> String -> IO ()
precomputeGraph pkgs mdls depth useHO envPath = do
  env <- generateEnv pkgs mdls depth useHO
  writeEnv env envPath

-- | Parse and resolve file, then synthesize the specified goals
runOnFile :: SynquidParams -> SearchParams  -> String -> IO ()
runOnFile synquidParams searchParams file = do
  goal <- parseGoal file
  goal' <- feedEnv goal
  result <- synthesize searchParams goal'
  return ()
  where
    feedEnv goal = do
      let envPathIn = envPath synquidParams
      doesExist <- doesFileExist envPathIn
      when (not doesExist) (error ("Please run `stack exec -- " ++ programName ++ " generate -p [PACKAGES]` to generate database first"))
      envRes <- decode <$> B.readFile envPathIn
      case envRes of
        Left err -> error err
        Right env -> do
          putStrLn $ "INSTRUMENTED: Components: " ++ show (sum (map length (map Map.elems (Map.elems (_symbols env)))))
          let spec = runExcept $ evalStateT (resolveSchema (gSpec goal)) (initResolverState { _environment = env })
          case spec of
            Right sp -> return $ goal { gEnvironment = env, gSpec = sp }
            Left parseErr -> (pdoc $ pretty parseErr) >> pdoc empty >> exitFailure
    parseGoal sig = do
      let transformedSig = "goal :: " ++ sig ++ "\ngoal = ??"
      parseResult <- return $ flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" transformedSig
      case parseResult of
        Left parseErr -> (pdoc $ pretty $ toErrorMessage parseErr) >> pdoc empty >> exitFailure
        -- Right ast -> print $ vsep $ map pretty ast
        Right (funcDecl:decl:_) -> case decl of
          Pos _ (SynthesisGoal id uprog) ->
            let Pos _ (FuncDecl _ sch) = funcDecl
            in do
              -- let tvs = Set.toList $ typeVarsOf (toMonotype sch)
              -- let spec = foldr ForallT sch tvs
              return $ Goal id emptyEnv sch uprog 3 $ initialPos "goal"
          _ -> error "parse a signature for a none goal declaration"

    pdoc = printDoc Plain
