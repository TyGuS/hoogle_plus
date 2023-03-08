module Main
  ( main
  ) where
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Lens                   ( (.~)
                                                , (^.)
                                                )
import           Control.Monad.State
import qualified Data.ByteString.Lazy.Char8    as LB
import           Data.Function                  ( (&) )
import           Data.Time.Calendar             ( Day
                                                , fromGregorian
                                                , showGregorian
                                                )
import           System.Console.CmdArgs  hiding ( Normal )
import           System.Directory               ( doesFileExist
                                                , removeFile
                                                )
import           System.IO                      ( BufferMode(LineBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import qualified Data.Text as Text

import           Database.Dataset
import           Database.Environment
import           Database.Presets
import qualified Evaluation.Benchmark
import           Evaluation.EvalTypeInf
import           Evaluation.ReadBenchmark
import           Examples.InferenceDriver
import qualified Hectare.TermSearch            as Hectare
import           HooglePlus.IOFormat
import           HooglePlus.Synthesize
import           PetriNet.PNSolver
import           Postfilter.GHCChecker
import           Types.Environment
import           Types.Experiments
import           Types.Filtering
import           Types.Generate          hiding ( files )
import           Types.Program
import           Types.Solver
import Types.Common
import Dataset.Dataset

programName :: String
programName = "hoogleplus"

versionName :: String
versionName = "0.1"

releaseDate :: Day
releaseDate = fromGregorian 2019 3 10

-- | Type-check and synthesize a program, according to command-line arguments
main :: IO ()
main = do
  res <- cmdArgsRun mode
  case res of
    Synthesis jsonStr searchCat engine appMax solNum noHigherOrder useRefine stopRef stopThreshold getNExamples getNTypes disableDema disableCoalescing coalescingStrategy noRelevancy noCopyTrans noBlacklist noFilter logLv outputFormat outputFile
      -> do
        let sparams = defaultSearchParams
              { _maxApplicationDepth = appMax
              , _logLevel            = logLv
              , _solutionCnt         = solNum
              , _useHO               = not noHigherOrder
              , _stopRefine          = stopRef
              , _threshold           = stopThreshold
              , _refineStrategy      = useRefine
              , _disableDemand       = disableDema
              , _coalesceTypes       = not disableCoalescing
              , _coalesceStrategy    = coalescingStrategy
              , _disableRelevancy    = noRelevancy
              , _disableCopy         = noCopyTrans
              , _disableBlack        = noBlacklist
              , _disableFilter       = noFilter
              }
        let searchPrograms = if null jsonStr
              then error "A JSON string must be provided"
              else executeSearch engine sparams jsonStr outputFormat outputFile
        case searchCat of
          SearchPrograms -> searchPrograms
          SearchTypes    -> void (searchTypes jsonStr getNTypes)
          SearchResults  -> searchResults jsonStr
          SearchExamples -> searchExamples includedModules jsonStr getNExamples
    Generate { preset = Just p } -> precomputeGraph (getOptsFromPreset p)
    Generate Nothing genFiles mdls d ho pathToEnv genHoPath -> do
      let fetchOpts = Local genFiles
      let generationOpts = defaultGenerationOpts
            { modules                = mdls
            , instantiationDepth     = d
            , enableHOF              = ho
            , pkgFetchOpts           = fetchOpts
            , Types.Generate.envPath = pathToEnv
            , Types.Generate.hoPath  = genHoPath
            }
      precomputeGraph generationOpts
    Evaluation outFile fp bm isStudy -> do
      benchmarks <- readSuite fp
      let benchmarks' =
            filter (\b -> Evaluation.Benchmark.name b == bm) benchmarks
      runTypeInferenceEval outFile
                           isStudy
                           (if bm == "" then benchmarks else benchmarks')
    Dataset outFile mdls rep maxargs minsize maxsize samplenum -> do
      let mdls' = ["Nil", "Cons", "Data.Bool.True", "Data.Bool.False"] ++ mdls
      let components = case mdls of
                        [] -> hplusComponents
                        _ -> filter (\(f, _) -> any (`Text.isPrefixOf` f) mdls') hplusComponents
      putStrLn $ "using " ++ show (length components) ++ " components"
      writeCsv outFile $ generateQAPairs components (Configuration rep maxargs minsize maxsize samplenum)
      -- let programs = Hectare.doSample 1 3 100
      -- mapM_ (putStrLn . plainShow) programs

{- Command line arguments -}

data CommandLineArgs
    = Synthesis { -- | Input
                  json :: String
                , search_category :: QueryType
                , -- | Search params
                  search_engine :: SearchEngine
                , app_max :: Int
                , sol_num :: Int
                , disable_higher_order :: Bool
                , use_refine :: RefineStrategy
                , stop_refine :: Bool
                , stop_threshold :: Int
                , get_n_examples :: Int
                , get_n_types :: Int
                , disable_demand :: Bool
                , disable_coalescing :: Bool
                , coalescing_strategy :: CoalesceStrategy
                , disable_relevancy :: Bool
                , disable_copy_trans :: Bool
                , disable_blacklist :: Bool
                , disable_filter :: Bool
                , -- | Output
                  log_ :: Int
                , output_format :: OutputFormat
                , output_file :: FilePath
      }
      | Generate {
        -- | Input
        preset :: Maybe Preset,
        files :: [String],
        module_name :: [String],
        type_depth :: Int,
        higher_order :: Bool,
        env_file_path_out :: String,
        ho_path :: String
      }
      | Evaluation {
        out_file :: FilePath,
        benchmark_suite :: String,
        benchmark :: String,
        use_study_data :: Bool
      }
      | Dataset {
        out_file :: FilePath,
        dataset_modules :: [Id],
        dataset_repeat :: Int,
        dataset_maxargs :: Int,
        dataset_minsize :: Int,
        dataset_maxsize :: Int,
        dataset_samplenum :: Int
      }
  deriving (Data, Typeable, Show, Eq)

synt :: CommandLineArgs
synt =
  Synthesis
      { json                 = "" &= help "Input query from a json string"
      , search_category      = SearchPrograms &= help "For web demo"
      , search_engine        = Hectare &= help "Search engine to use"
      , app_max              = 6 &= help "Maximum depth of an application term (default: 6)" &= groupname "Explorer parameters"
      , log_ = 0 &= help "Logger verboseness level (default: 0)" &= name "l"
      , sol_num              = 1 &= help "Number of solutions need to find (default: 1)" &= name "cnt"
      , disable_higher_order = False &= help "Disable higher order functions (default: False)"
      , use_refine           = TyGarQ &= help "Use abstract refinement or not (default: TyGarQ)"
      , stop_refine          = True &= help "Stop refine the abstraction cover after some threshold (default: False)"
      , stop_threshold       = 10 &= help "Refinement stops when the number of places reaches the threshold, only when stop_refine is True"
      , get_n_examples       = 1 &= help "Number of more examples (used only for web mode)"
      , get_n_types          = 10 &= help "Number of inferred types (used only for web mode)"
      , disable_demand       = False &= name "d" &= help "Disable the demand analyzer (default: False)"
      , disable_coalescing   = False &= name "xc" &= help "Do not coalesce transitions in the net with the same abstract type"
      , coalescing_strategy  = First &= help "Choose how type coalescing works. Default: Pick first element of each group set."
      , disable_relevancy    = False &= help "Disable the relevancy requirement for argument types (default: False)"
      , disable_copy_trans   = False &= help "Disable the copy transitions and allow more than one token in initial state instead (default: False)"
      , disable_blacklist    = False &= help "Disable blacklisting functions in the solution (default: False)"
      , disable_filter       = True &= help "Disable filter-based test"
      , output_format        = CommandLine &= help "Output format (default: CommandLine)"
      , output_file          = "results.log" &= help "Output file (default: ./results.log). This flag is only used when output_format is OutputFile"
      } &= help "Synthesize a program" &= auto

generate :: CommandLineArgs
generate =
  Generate
      { preset            = Nothing &= help "Environment preset to use"
      , files             = [] &= help "Files to use to generate from. Exclusive with packages and modules. Takes precedence"
      , module_name       = [] &= help "Module names to be generated in the given packages"
      , type_depth        = 2 &= help "Depth of the types to be instantiated for polymorphic type constructors"
      , higher_order      = True &= help "Include higher order functions (default: True)"
      , env_file_path_out = defaultEnvPath &= help ("Environment file path (default:" ++ show defaultEnvPath ++ ")")
      , ho_path           = "ho.txt" &= typFile &= help "Filename of components to be used as higher order arguments"
      }
    &= help "Generate the type conversion database for synthesis"

evaluation :: CommandLineArgs
evaluation =
  Evaluation
      { out_file        = "inference.tsv" &= help "Path to the output file"
      , benchmark_suite = "benchmark/suites/working.yml" &= help "Path to the benchmark file, in the format of YAML"
      , benchmark       = "" &= help "Evaluate this single benchmark"
      , use_study_data  = False &= help "Evaluate the type inference over data collected in user study"
      }
    &= help "Evaluate Hoogle+ modules"

dataset :: CommandLineArgs
dataset =
  Dataset
    { out_file = "dataset" &= help "Path to the output file"
    , dataset_modules = [] &= help "Modules to be used during dataset creation. Empty means include everything." &= name "dm"
    , dataset_repeat = 3 &= help "Repeat the bottom up generation n times." &= name "dr"
    , dataset_maxargs = 3 &= help "Only consider lambda expressions with no more than n arguments." &= name "dargs"
    , dataset_minsize = 2 &= help "Minimum size of the generated program" &= name "minsz"
    , dataset_maxsize = 3 &= help "Maximum size of the generated program" &= name "maxsz"
    , dataset_samplenum = 300 &= help "Total number of samples in the dataset" &= name "nsample"
    }
  &= help "Generate a dataset using components from specified modules"

mode :: Mode (CmdArgs CommandLineArgs)
mode =
  cmdArgsMode
    $  modes [synt, generate, evaluation, dataset]
    &= help (programName ++ " program synthesizer")
    &= program programName
    &= summary
         (  programName
         ++ " v"
         ++ versionName
         ++ ", "
         ++ showGregorian releaseDate
         )

precomputeGraph :: GenerationOpts -> IO ()
precomputeGraph = generateEnv

-- | Parse and resolve file, then synthesize the specified goals
executeSearch
  :: SearchEngine -> SearchParams -> String -> OutputFormat -> FilePath -> IO ()
executeSearch engine params inStr outputFormat outputFile = catch
  (do
    let input    = decodeInput (LB.pack inStr)
    let tquery   = query input
    let examples = inExamples input
    hSetBuffering stdout LineBuffering

    -- clear the log file before writing to it
    exists <- doesFileExist outputFile
    when (outputFormat == OutputFile && exists) $ removeFile outputFile

    print $ "Synthesizing " ++ show tquery

      -- invoke synthesis
    case engine of
      HooglePlus ->
        envToGoal loadEnv tquery examples >>= \goal -> runHooglePlus goal
      Hectare ->
        envToGoal loadEnvFo tquery examples >>= \goal -> runHectare goal
    return ()
  )
  (\(e :: SomeException) -> do
    printResult $ encodeWithPrefix $ QueryOutput [] (show e) []
    error (show e)
  )

 where
  runHooglePlus :: Goal -> IO ()
  runHooglePlus goal = do
    (programs, st) <- synthesize params goal
    let initState = (st ^. filterState) { flogLevel = params ^. logLevel }
    (cnt, fstate) <- getKPrograms goal (0, initState) programs
    let appDepth = params ^. maxApplicationDepth
    let currLen  = st ^. (searchState . currentLoc)
    when (cnt < params ^. solutionCnt && currLen <= appDepth)
         (getMoreSolutions goal (st & filterState .~ fstate) cnt)

  getMoreSolutions :: Goal -> SolverState -> Int -> IO ()
  getMoreSolutions goal@(Goal env goalTyp _) st n = do
    if n >= params ^. solutionCnt
      then return ()
      else do
        (programs, st'   ) <- runStateT (nextSolution env goalTyp) st
        (cnt     , fstate) <- getKPrograms goal (0, st' ^. filterState) programs
        let appDepth = params ^. maxApplicationDepth
        let currLen  = st' ^. (searchState . currentLoc)
        when (n + cnt < params ^. solutionCnt && currLen <= appDepth)
             (getMoreSolutions goal (st' & filterState .~ fstate) (n + cnt))

  runHectare :: Goal -> IO ()
  runHectare goal = do
    let programs = Hectare.synthesize goal
    -- print programs
    (synthesisCnt, _) <- getKPrograms
      goal
      (0, emptyFilterState { flogLevel = params ^. logLevel })
      programs
    when (synthesisCnt < params ^. solutionCnt)
      $ putStrLn "Hectare cannot find more solutions"

  getKPrograms
    :: Goal -> (Int, FilterState) -> [TProgram] -> IO (Int, FilterState)
  getKPrograms _ (n, fstate) _ | n == (params ^. solutionCnt) =
    return (n, fstate)
  getKPrograms _    (n, fstate) []       = return (n, fstate)
  getKPrograms goal (n, fstate) (p : ps) = do
    (fstate', mbProgram) <- runPostFilter goal n fstate p
    case mbProgram of
      Nothing -> getKPrograms goal (n, fstate') ps
      Just _  -> getKPrograms goal (n + 1, fstate') ps

  runPostFilter
    :: Goal
    -> Int
    -> FilterState
    -> TProgram
    -> IO (FilterState, Maybe TProgram)
  runPostFilter (Goal env goalType examples) cnt fstate p = do
    (checkResult, fstate') <- runStateT
      (checkSolution params env goalType examples p)
      fstate
    case checkResult of
      Nothing  -> return (fstate', Nothing)
      Just exs -> do
        queryOutput <- liftIO $ toOutput env p exs
        case outputFormat of
          JSON        -> liftIO $ printResult $ encodeWithPrefix queryOutput
          CommandLine -> liftIO $ printCmd cnt queryOutput Nothing
          OutputFile  -> liftIO $ printCmd cnt queryOutput (Just outputFile)
        return (fstate', Just p)
