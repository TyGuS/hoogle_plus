module Evaluation.EvalTypeInf
    ( runTypeInferenceEval
    ) where

import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.List                      ( inits
                                                , intercalate
                                                , isInfixOf
                                                , zip5
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Language.Haskell.Interpreter   ( MonadIO(liftIO)
                                                , as
                                                , interpret
                                                , setImports
                                                )
import           System.IO                      ( IOMode(AppendMode)
                                                , hPutStrLn
                                                , withFile
                                                )
import           System.IO.Silently             ( silence )
import           System.Random                  ( Random(randomR)
                                                , getStdRandom
                                                )
import           Test.QuickCheck                ( Result(..) )
import qualified Test.QuickCheck               as QC
import           Text.Printf                    ( printf )

import           Evaluation.Benchmark
import           Examples.InferenceDriver
import           Postfilter.FilterTest
import           HooglePlus.IOFormat
import           HooglePlus.Utils
import           Types.Common
import           Types.Environment              ( Environment )
import           Types.Filtering
import           Types.Generate                 ( defaultEnvPath )
import           Types.Program
import           Types.Type
import           Types.TypeChecker
import           Utility.Utils

typeCandidates :: [TypeSkeleton]
typeCandidates = [intType, charType, listType charType, doubleType]

{- definition of results -}
data InferenceResult = InferenceResult
    { benchmark         :: !Benchmark
    , genExamples       :: ![Example]
    , infTypes          :: ![String]
    , correctIndex      :: !String
    , beforeFilterCount :: !Int
    , afterFilterCount  :: !Int
    , varCount          :: !Int
    , argCount          :: !Int
    }
    deriving Show

runTest :: Int -> Benchmark -> IO [Example]
runTest numOfExs bm@(Benchmark _ q sol _ _) = do
    let typ = parseQueryType q
    -- generate @numOfExs@ examples
    mbExamples <- mapM
        (const $ catch
            (do
                subst <- randomSubst (Set.toList $ typeVarsOf typ) Map.empty
                let t    = typeSubstitute subst typ
                let prop = buildProperty t []
                res <- runInterpreter'
                    defaultInterpreterTimeoutMicro
                    (do
                        setImports
                            [ "Data.Maybe"
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
                        interpret prop (as :: IO Example) >>= liftIO
                    )
                case res of
                    Left  err               -> print err >> return Nothing
                    Right (Example ins out) -> do
                        let correctedCode = map correctFun ins
                        if "***Exception"
                            `isInfixOf` out
                            ||          any ("Infinity" `isInfixOf`) ins
                        then
                            return Nothing
                        else
                            return $ Just $ Example correctedCode
                                                    (correctFun out)
            )
            (\(e :: SomeException) -> print e >> return Nothing)
        )
        [1 .. numOfExs]
    let examples = catMaybes mbExamples
    if length examples == numOfExs
        then return examples
        else (++ examples) <$> runTest (numOfExs - length examples) bm
  where
    correctFun code | "..." `isInfixOf` code =
        replaceId "..." (printf "_ -> error \"unhandled\";") code
    correctFun code = code

    wrapMyFun :: TypeSkeleton -> TypeSkeleton
    wrapMyFun (DatatypeT dt args) = DatatypeT dt (map wrapMyFun args)
    wrapMyFun (FunctionT x tArg tRes) =
        DatatypeT "MyFun" [wrapMyFun tArg, wrapMyFun tRes]
    wrapMyFun t = t

    notTypeclass (DatatypeT dt _) | tyclassPrefix `Text.isPrefixOf` dt = False
    notTypeclass _ = True

    usedArgs t = filter notTypeclass $ map snd (argsWithName t)

    buildProperty t refutes =
        let
            args    = filter notTypeclass $ map snd (argsWithName t)
            rndSize = "sz <- getStdRandom (randomR (3, 10))" :: String
            namedArgs =
                zipWith (\a i -> ("arg" ++ show i, wrapMyFun a)) args [0 ..]
            nonEmpty t = if head (show t) == '['
                then "QC.suchThat (QC.resize sz QC.arbitrary) (not . null)"
                else "QC.resize sz QC.arbitrary" :: String
            genArg arg typ =
                printf "%s; %s <- QC.generate (%s) :: IO (%s)"
                       rndSize
                       arg
                       (nonEmpty typ)
                       (show typ) :: String
            genArgs   = map (uncurry genArg) namedArgs
            unwrpArgs = map (printf "(unwrap %s)" . fst) namedArgs
            showArgs  = map (printf "(show %s)" . fst) namedArgs
            callArgs =
                printf
                    "out <- catch (show <$> ((wrap <$> evaluate (((%s) :: %s) %s)) :: IO (%s))) (\\(e :: SomeException) -> return (\"***Exception\" ++ show e)); return (Example [%s] out)"
                    sol
                    (mkFunctionSigStr (args ++ [lastType t]))
                    (unwords unwrpArgs)
                    (show (wrapMyFun (lastType t)))
                    (intercalate "," showArgs) :: String
        in
            printf "do {%s; %s}" (intercalate "; " genArgs) callArgs

randomSubst :: [Id] -> Map Id TypeSkeleton -> IO (Map Id TypeSkeleton)
randomSubst []         sofar = return sofar
randomSubst (v : vars) sofar = do
    -- update the random generator by splitting the seed
    -- newStdGen
    rndIdx <- getStdRandom (randomR (0, length typeCandidates - 1))
    randomSubst vars (Map.insert v (typeCandidates !! rndIdx) sofar)

runInference :: Bool -> Benchmark -> IO [InferenceResult]
runInference isStudy bm = do
    print bm
    exs <- if isStudy
        then return (Evaluation.Benchmark.examples bm)
        else runTest 3 bm
    let combinations = if isStudy then [exs] else tail (inits exs)
    mapM
        (\xs -> silence $ do
            let
                names = map (appendIndex "arg")
                            [0 .. (length (inputs (head xs)) - 1)]
            let inStr = unpack (encode (QueryInput "??" xs names))
            (ListOutput res _, stats) <- searchTypes inStr 10
            dt                        <- getMetadata res stats
            return $ dt { genExamples = xs }
        )
        combinations

  where
    getMetadata res (InfStats prefilter postfilter) = do
        let query = Evaluation.Benchmark.query bm
        let q     = parseQueryType query
        checkRes <- getCorrectIndex q 1 res
        let argCnt = length (argsWithName q)
        let varCnt = length (typeVarsOf q)
        return
            (InferenceResult bm
                             []
                             res
                             checkRes
                             prefilter
                             postfilter
                             varCnt
                             argCnt
            )

runTypeInferenceEval :: FilePath -> Bool -> [Benchmark] -> IO ()
runTypeInferenceEval fp isStudy bms = do
    results <- mapM (runInference isStudy) bms
    writeResultsTsv fp (concat results)

getCorrectIndex :: TypeSkeleton -> Int -> [String] -> IO String
getCorrectIndex _ _   []           = return "NO ANSWER"
getCorrectIndex q idx (infer : xs) = do
    let t = parseQueryType infer
    if typeCmp q t then return (show idx) else getCorrectIndex q (idx + 1) xs

writeResultsTsv :: FilePath -> [InferenceResult] -> IO ()
writeResultsTsv fp results = withFile fp AppendMode $ \hdl -> do
    let padEnd n xs = xs ++ replicate n []
    mapM_
        (\(InferenceResult bm exs typs rank pre post vars args) -> do
            let len = max (length exs) (length typs)
            let align x = padEnd (len - 1) [x]
            let exStrs    = padEnd (len - length exs) (map show exs)
            let typStrs   = padEnd (len - length typs) typs
            let nameStrs  = align (Evaluation.Benchmark.name bm)
            let queryStrs = align (Evaluation.Benchmark.query bm)
            let
                dataStrs = padEnd
                    (len - 1)
                    [[(length exs, rank, pre, post, vars, args)]]
            mapM_
                (\(name, query, e, t, d) -> do
                    let
                        ln = case (name, query, d) of -- there appear only in the first line
                            ("", _, _) -> printf "\t\t%s\t%s\t\t\t\t\t\t" e t
                            (n, q, [(ex, r, pre, post, v, a)]) -> printf
                                "%s\t%s\t%s\t%s\t%d\t%s\t%d\t%d\t%d\t%d"
                                n
                                q
                                e
                                t
                                ex
                                r
                                pre
                                post
                                v
                                a
                            _ -> error "writeResultsTsv: invalid result format"
                    hPutStrLn hdl ln
                )
                (zip5 nameStrs queryStrs exStrs typStrs dataStrs)
        )
        results

