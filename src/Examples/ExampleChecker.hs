{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.ExampleChecker(
    execExample,
    checkExamples,
    checkExampleOutput,
    augmentTestSet
    )where

import Types.Program
import Types.Type
import Types.Environment
import Types.Experiments
import Types.IOFormat
import Types.TypeChecker
import Types.Common
import Types.Filtering (defaultTimeoutMicro, defaultDepth, defaultInterpreterTimeoutMicro, frameworkModules)
import Synquid.Type
import Synquid.Pretty
import Synquid.Program
import HooglePlus.TypeChecker
import HooglePlus.Utils
import PetriNet.Utils
import Synquid.Utils (permuteBy)
import Database.Utils
import Examples.Utils
import Examples.InferenceDriver

import Control.Exception
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Chan
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either
import Data.Maybe
import Data.List
import Text.Printf
import Debug.Trace

checkExample :: Environment -> SchemaSkeleton -> Example -> Chan Message -> IO (Either ErrorMessage SchemaSkeleton)
checkExample env typ ex checkerChan = do
    let mdls = Set.toList $ env ^. included_modules
    eitherTyp <- parseExample mdls mkFun
    case eitherTyp of
        Left e -> return $ Left e
        Right exTyp -> do
            let err = printf "%s does not have type %s" (show ex) (show typ) :: String
            let tcErr = printf "%s does not satisfy type class constraint in %s" (show ex) (show typ) :: String
            (res, substedTyp) <- checkTypes env checkerChan exTyp typ
            let (tyclasses, strippedTyp) = unprefixTc substedTyp
            let tyclassesPrenex = intercalate ", " $ map show tyclasses
            let breakTypes = map show $ breakdown strippedTyp
            let mkTyclass = printf "%s :: (%s) => (%s)" mkFun tyclassesPrenex (intercalate ", " breakTypes)
            eitherTyclass <- if null tyclasses then return (Right (Monotype AnyT)) else parseExample mdls mkTyclass
            if res then if isLeft eitherTyclass then return $ Right exTyp
                                                else return $ Left tcErr
                else return $ Left err
    where
        mkFun = printf "(%s)" (intercalate ", " $ inputs ex ++ [output ex])

        unprefixTc (FunctionT x tArg tRes) = (argTcs ++ resTcs, FunctionT x tArg' tRes')
            where
                (argTcs, tArg') = unprefixTc tArg
                (resTcs, tRes') = unprefixTc tRes
        unprefixTc (TyAppT (DatatypeT name) tArg) | tyclassPrefix `isPrefixOf` name =
            let (tcs, t) = unprefixTc tArg
                currTc = DatatypeT (drop (length tyclassPrefix) name)
             in (currTc : tcs, t) 
        unprefixTc t = ([], t)

checkExamples :: Environment -> SchemaSkeleton -> [Example] -> Chan Message -> IO (Either [ErrorMessage] [SchemaSkeleton])
checkExamples env typ exs checkerChan = do
    outExs <- mapM (\ex -> checkExample env typ ex checkerChan) exs
    let (errs, validResults) = partitionEithers outExs
    if null errs then return $ Right validResults
                 else return $ Left errs

execExample :: [String] -> Environment -> TypeQuery -> String -> Example -> IO (Either ErrorMessage String)
execExample mdls env typ prog ex = do
    let args = Map.keys $ env ^. arguments
    let nontcArgs = filter (not . (tyclassArgBase `isPrefixOf`)) args
    let prependArg = unwords nontcArgs
    let progBody = if Map.null (env ^. arguments) -- if this is a request from front end
        then printf "let f = (%s) :: %s in" prog typ
        else printf "let f = (\\%s -> %s) :: %s in" prependArg prog typ
    let parensedInputs = map wrapParens $ inputs ex
    let progCall = printf "(f %s)" (unwords parensedInputs)
    runStmt mdls $ unwords [progBody, progCall]

augmentTestSet :: Environment -> SchemaSkeleton -> IO [Example]
augmentTestSet env goal = do
    let candidates = env ^. queryCandidates
    let permutedCands = concatMap permuteMap (Map.toList candidates)
    let permutedMap = Map.fromList permutedCands
    matchCands <- filterM (\s -> generalThan goal s) (Map.keys permutedMap)
    let usefulExs = concatMap (\s -> permutedMap Map.! s) matchCands
    return $ nubBy (\x y -> inputs x == inputs y) usefulExs
    where
        permuteExamples ords (Example ins out) = Example (permuteBy ords ins) out
        permuteMap (q, exs) = let argLen = length (argsWithName $ toMonotype q)
                                  orderPermutes = permutations [1..argLen]
                                  -- TODO: maybe we need to monomorphize the
                                  -- types here
                                  typesList = map (flip permuteArgs q) orderPermutes
                                  examplesList = map (\o -> map (permuteExamples o) exs) orderPermutes
                               in zip typesList examplesList

        generalThan s1 s2 = do
            msgChan <- newChan
            let initChecker = emptyChecker { _checkerChan = msgChan }
            let bound = env ^. boundTypeVars
            state <- execStateT (do
                s1' <- freshType bound s1
                s2' <- freshType bound s2
                let vars = typeVarsOf s2'
                let env' = foldr addTypeVar env vars
                state $ runState $ solveTypeConstraint env' s1' s2') initChecker
            return $ state ^. isChecked

checkExampleOutput :: [String] -> Environment -> TypeQuery -> String -> [Example] -> IO (Maybe [Example])
checkExampleOutput mdls env typ prog exs = do
    let progWithoutTc = removeTypeclasses prog
    currOutputs <- mapM (execExample mdls env typ progWithoutTc) exs
    cmpResults <- zipWithM compareResults currOutputs exs
    let justResults = catMaybes cmpResults
    if length justResults == length exs then return $ Just justResults
                                        else return Nothing
    where
        compareResults currOutput ex
          | output ex == "??" = return $ Just (ex { output = either id id currOutput })
          | otherwise = case currOutput of
                          Left e -> return Nothing
                          Right o -> do
                              expectedOutput <- runStmt mdls (printf "(%s)" $ output ex)
                              case expectedOutput of
                                  Left err -> return Nothing
                                  Right out | o == out -> return (Just ex)
                                            | otherwise -> return Nothing

