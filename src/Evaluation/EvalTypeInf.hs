{-# LANGUAGE ScopedTypeVariables #-}

module Evaluation.EvalTypeInf (
    runTypeInferenceEval
    ) where

import Test.QuickCheck (Result(..))
import qualified Test.QuickCheck as QC
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import System.Random
import System.IO
import Text.Printf
import Language.Haskell.Interpreter hiding (Id)
import Control.Monad.IO.Class
import Control.Exception

import Database.Util (tyclassPrefix)
import Types.Common
import Types.Type
import Types.IOFormat
import Types.Experiments (defaultSynquidParams)
import Types.Generate (defaultEnvPath)
import Types.Filtering (FunctionSignature(..), defaultInterpreterTimeoutMicro)
import Types.InfConstraint (InfStats(..))
import Types.Environment (Environment)
import HooglePlus.IOFormat (searchTypes, readEnv, parseQueryType)
import HooglePlus.FilterTest (showParams, runInterpreter', parseTypeString)
import HooglePlus.Utils (removeTypeclasses, mkFunctionSigStr, replaceId)
import Synquid.Type (toMonotype, shape, stypeSubstitute, boundVarsOf, argsWithName, lastType, eqType)
import Evaluation.Benchmark

{- set up candidates to be used in type variable instantiation -}
int_ = ScalarT (DatatypeT "Int" [] []) ()
char_ = ScalarT (DatatypeT "Char" [] []) ()
double_ = ScalarT (DatatypeT "Double" [] []) ()
list_ t = ScalarT (DatatypeT "List" [t] []) ()
maybe_ t = ScalarT (DatatypeT "Maybe" [t] []) ()

typeCandidates :: [SType]
typeCandidates = 
    [ int_
    , char_
    , list_ char_
    , double_
    ]

{- definition of results -}
data InferenceResult = InferenceResult {
    benchmark :: !Benchmark,
    genExamples :: ![Example],
    infTypes :: ![String],
    correctIndex :: !String,
    beforeFilterCount :: !Int,
    afterFilterCount :: !Int,
    varCount :: !Int,
    argCount :: !Int
} deriving (Show)

runTest :: Int -> Benchmark -> IO [Example]
runTest numOfExs bm@(Benchmark _ q sol _ _) = do
    env <- readEnv defaultEnvPath 
    let sch = parseQueryType env q
    -- generate @numOfExs@ examples
    mbExamples <- mapM (const $ catch (do
        subst <- randomSubst (boundVarsOf sch) Map.empty
        let t = stypeSubstitute subst (shape $ toMonotype sch)
        let prop = buildProperty t []
        res <- runInterpreter' defaultInterpreterTimeoutMicro (do
            setImports [  "Data.Maybe"
                        , "Data.Either"
                        , "Data.Char"
                        , "Test.QuickCheck"
                        , "System.Random"
                        , "Data.List"
                        , "Data.Function"
                        , "Data.Tuple"
                        , "Data.Bool"
                        , "Data.Int"
                        , "Text.Show"
                        , "Control.Exception"
                        , "Data.Eq"
                        , "Prelude"
                        ]
            interpret prop (as :: IO Example) >>= liftIO)
        case res of
            Left err -> print err >> return Nothing
            Right (Example ins out) -> do
                let correctedCode = map correctFun ins
                if "***Exception" `isInfixOf` out || any ("Infinity" `isInfixOf`) ins || "[]" `isInfixOf` out
                    then return Nothing
                    else return $ Just $ Example correctedCode (correctFun out)
            ) (\(e :: SomeException) -> print e >> return Nothing)
        ) [1..numOfExs]
    let examples = catMaybes mbExamples
    if length examples == numOfExs
        then return examples
        else (++ examples) <$> runTest (numOfExs - length examples) bm
    where
        correctFun code | "..." `isInfixOf` code =
            replaceId "..." (printf "_ -> error \"unhandled\";") code
        correctFun code = code

        wrapMyFun :: SType -> SType
        wrapMyFun (ScalarT (DatatypeT dt args _) _) =  ScalarT (DatatypeT dt (map wrapMyFun args) []) ()
        wrapMyFun (FunctionT x tArg tRes) = ScalarT (DatatypeT "MyFun" [(wrapMyFun tArg), (wrapMyFun tRes)] []) ()
        wrapMyFun t = t

        notTypeclass (ScalarT (DatatypeT dt _ _) _) | tyclassPrefix `isPrefixOf` dt = False
        notTypeclass _ = True

        usedArgs t = filter notTypeclass $ map snd (argsWithName t)

        buildProperty t refutes = let
            args = filter notTypeclass $ map snd (argsWithName t)
            rndSize = "sz <- getStdRandom (randomR (3, 10))"
            namedArgs = map (\(a, i) -> ("arg" ++ show i, wrapMyFun a)) (zip args [0..])
            nonEmpty t = if head (show t) == '[' then "QC.suchThat (QC.resize sz QC.arbitrary) (not . null)" else "QC.resize sz QC.arbitrary"
            genArg arg typ = printf "%s; %s <- QC.generate (%s) :: IO (%s)" rndSize arg (nonEmpty typ) (show typ) :: String
            genArgs = map (uncurry genArg) namedArgs
            unwrpArgs = map (printf "(unwrap %s)" . fst) namedArgs
            showArgs = map (printf "(show %s)" . fst) namedArgs
            callArgs = printf "out <- catch (show <$> ((wrap <$> evaluate (((%s) :: %s) %s)) :: IO (%s))) (\\(e :: SomeException) -> return (\"***Exception\" ++ show e)); return (Example [%s] out)" sol (mkFunctionSigStr (args ++ [lastType t])) (unwords unwrpArgs) (show (wrapMyFun (lastType t))) (intercalate "," showArgs)  :: String
            in printf "do {%s; %s}" (intercalate "; " genArgs) callArgs

randomSubst :: [Id] -> Map Id SType -> IO (Map Id SType)
randomSubst [] sofar = return sofar
randomSubst (v:vars) sofar = do
    -- update the random generator by splitting the seed
    -- newStdGen
    rndIdx <- getStdRandom (randomR (0, length typeCandidates - 1))
    randomSubst vars (Map.insert v (typeCandidates !! rndIdx) sofar)

runInference :: Bool -> Benchmark -> IO [InferenceResult]
runInference isStudy bm = do
    print bm
    exs <- if isStudy then return (Evaluation.Benchmark.examples bm)
                      else runTest 3 bm
    mapM (\xs -> do
        let inStr = unpack (encode (QueryInput "??" xs))
        (ListOutput res _, stats) <- searchTypes defaultSynquidParams inStr 10
        dt <- getMetadata res stats
        return $ dt { genExamples = xs }
        ) (tail $ inits exs)

    where
        getMetadata res (InfStats prefilter postfilter) = do
            env <- readEnv defaultEnvPath 
            let query = Evaluation.Benchmark.query bm
            let q = parseQueryType env query
            checkRes <- getCorrectIndex env q 1 res
            let argCnt = length (argsWithName (toMonotype q))
            let varCnt = length (boundVarsOf q)
            return (InferenceResult bm [] res checkRes prefilter postfilter varCnt argCnt)

runTypeInferenceEval :: FilePath -> Bool -> [Benchmark] -> IO ()
runTypeInferenceEval fp isStudy bms = do
    results <- mapM (runInference isStudy) bms
    writeResultsTsv fp (concat results)

getCorrectIndex :: Environment -> RSchema -> Int -> [String] -> IO String
getCorrectIndex _ _ _ [] = return "NO ANSWER"
getCorrectIndex env q idx (infer:xs) = do
    let t = parseQueryType env infer
    if eqType (toMonotype q) (toMonotype t)
        then return (show idx)
        else getCorrectIndex env q (idx + 1) xs

writeResultsTsv :: FilePath -> [InferenceResult] -> IO ()
writeResultsTsv fp results = 
    withFile fp AppendMode $ \hdl -> do
        let padEnd n xs = xs ++ replicate n []
        mapM_ (\(InferenceResult bm exs typs rank pre post vars args) -> do
            let len = max (length exs) (length typs)
            let align x = padEnd (len - 1) [x]
            let exStrs = padEnd (len - length exs) (map show exs)
            let typStrs = padEnd (len - length typs) typs
            let nameStrs = align (Evaluation.Benchmark.name bm)
            let queryStrs = align (Evaluation.Benchmark.query bm)
            let dataStrs = align [(length exs, rank, pre, post, vars, args)]
            mapM_ (\(name,query,e,t,d) -> do
                let ln = case (name, query, d) of -- there appear only in the first line
                        ("", _, _) -> printf "\t\t%s\t%s\t\t\t\t\t\t" e t
                        (n, q, [(ex, r, pre, post, v, a)]) -> printf "%s\t%s\t%s\t%s\t%d\t%s\t%d\t%d\t%d\t%d" n q e t ex r pre post v a
                hPutStrLn hdl ln
                ) (zip5 nameStrs queryStrs exStrs typStrs dataStrs)
            ) results

