module Dataset.Dataset (
    Configuration(..)
  , generateQAPairs
  , writeCsv
  , postfilter
) where

import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BS
import System.IO
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC hiding ( Id )
import Control.Exception (SomeException, catch)
import Outputable
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random

import qualified Data.Csv as Csv
import qualified Data.Aeson as Aeson
import Control.Monad.Extra
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P

import Dataset.Generation
import Types.Environment
import Types.Pretty
import Types.Program
import Types.TypeChecker
import Types.Type
import Examples.Utils
import Database.Dataset
import Examples.InferenceDriver
import Postfilter.FilterTest
import Postfilter.GHCChecker
import Types.Filtering
import HooglePlus.IOFormat
import Dataset.Configuration
import Dataset.HyperGraph (HyperGraph, SampleConfig(..))
import qualified Dataset.HyperGraph as HG

import Debug.Trace

writeCsv :: FilePath -> DatasetT IO (String, TProgram) -> IO ()
writeCsv outFile go =
  withFile (outFile ++ ".csv") WriteMode $ \hdl ->
    evalStateT (runEffect $ do
      every go
      >-> P.filter ((/= "") . fst)
      >-> P.map (\(t, p) -> t ++ "\t" ++ (plainShow $ unqualifyFunc p))
      >-> P.toHandle hdl) Set.empty

postfilter :: TProgram -> String -> IO Bool
postfilter prog funcSig = do
  let mdls = map Text.unpack includedModules
  let argNames = map Text.unpack (argumentsOf prog)
  andM [ checkStrictness 0 ("let i = 1 in let c = 'a' in " ++ plainShow prog) funcSig mdls
       , evalStateT (checkSolutionNotCrash mdls argNames funcSig prog) emptyFilterState
       ]

type DatasetT m = ListT (StateT (Set TProgram) m)

generateQAPairs :: MonadIO m => [(Text, SchemaSkeleton)] -> Configuration -> DatasetT m (String, TProgram)
generateQAPairs components config = do
  graph <- generate components config
  trace "Generate the hypergraph completed" $ return ()
  -- trace ("The hypergraph is " ++ show graph) $ return ()

  trace "Start sampling programs..." $ return ()
  let argThreshold t = if isNullDatatype t then 0.25
                        else if isFunctionType t then 0.75
                          else 0.9 :: Double
  let sampleConfig = SampleConfig {
    sampleDepth = randomRIO (minSize config, maxSize config),
    sampleEdge = \es -> let es' = Set.toList es in (es' !!) <$> randomRIO (0, length es' - 1),
    sampleArg = \t -> lift (randomRIO (0, 1)) >>= \r -> get >>= \i -> return (r >= argThreshold t && i < maxArgs config)
    }
  samples components graph config sampleConfig (sampleNum config)

samples :: MonadIO m => [(Text, SchemaSkeleton)] -> HyperGraph -> Configuration -> SampleConfig m -> Int -> DatasetT m (String, TProgram)
samples components graph config sampleConfig n
  | n == 0 = return ("", varp "dummy")
  | otherwise = do
    term <- lift . lift $ HG.sample graph sampleConfig
    -- trace ("get program " ++ plainShow term) $ return ()

    let prog = evalState (Types.Program.canonicalize term) Map.empty
    st <- get
    if prog `Set.member` st || numArguments prog == 0 || numArguments prog > maxArgs config || typeSize (typeOf prog) > 10
      then samples components graph config sampleConfig n
      else do
        modify $ Set.insert prog
        let (sig, prog') = localTypeCheck components prog
        if typeSize (typeOf prog') > 15 then
          samples components graph config sampleConfig n
          else
          return (sig, prog') `mplus` samples components graph config sampleConfig (n-1)

localTypeCheck :: [(Text, SchemaSkeleton)] -> TProgram -> (String, TProgram)
localTypeCheck components prog =
  let env = addComponent "0" (Monotype intType)
          $ addComponent "'a'" (Monotype charType)
          $ foldr (uncurry addComponent) loadEnv
          $ filter (\(n, _) -> isNothing $ lookupSymbol n loadEnv) components
  in case runTypeChecker Map.empty env emptyChecker prog of
    Left _ -> ("", prog)
    Right p -> (plainShow $ Types.TypeChecker.canonicalize $ typeOf p, p)

typeCheck :: Configuration -> TProgram -> IO (String, TProgram)
typeCheck config lambda = do
  if numArguments lambda > (maxArgs config)
    then return ("", lambda)
    else do
      (typ, filterRes) <- catch
        (do
          typ <- askGhc includedModules $ exprType TM_Default ("let i = 1 in let c = 'a' in " ++ plainShow lambda)
          -- trace ("type for " ++ (plainShow lambda) ++ " is " ++ showSDocUnsafe (ppr typ)) $ return ()
          let typ' = (toMonotype $ resolveType $ typeToLHsType typ)
          -- trace ("signature for " ++ (plainShow lambda) ++ " is " ++ show typ') $ return ()
          let typeSignature = filter (\c -> c /= '\n' && c /= '\t') $ plainShow typ'
          (typeSignature,) <$> postfilter lambda typeSignature
        )
        (\(e :: SomeException) -> trace ("error:" ++ show e) $ return ("", False))
      return (if filterRes then typ else "", lambda)
