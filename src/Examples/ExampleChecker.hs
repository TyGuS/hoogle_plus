{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE TupleSections #-}

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
import Types.Filtering (defaultTimeoutMicro, defaultInterpreterTimeoutMicro, frameworkModules)
import Synquid.Type
import Synquid.Pretty
import Synquid.Program
import Synquid.Logic
import HooglePlus.TypeChecker
import HooglePlus.Utils
import PetriNet.Util
import Synquid.Util (permuteBy)
import Database.Convert (addTrue)
import Database.Util
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

checkExample :: Environment -> RSchema -> Example -> Chan Message -> IO (Either ErrorMessage RSchema)
checkExample env typ ex checkerChan = do
    let mdls = Set.toList $ env ^. included_modules
    eitherTyp <- parseExample mdls mkFun
    case eitherTyp of
      Right exTyp -> do
        let err = printf "%s does not have type %s" (show ex) (show typ) :: String
        let tcErr = printf "%s does not satisfy type class constraint in %s" (show ex) (show typ) :: String
        let initChecker = emptyChecker { _checkerChan = checkerChan }
        -- refresh the type variables names in the two
        ((exTyp', typ'), checker') <- runStateT (do
            t1 <- freshType (env ^. boundTypeVars) exTyp
            t2 <- freshType (env ^. boundTypeVars) typ
            return (t1, t2)
            ) initChecker
        (checked, tass) <- checkTypes env checker' [] exTyp' typ'
        let substedTyp = traceShow tass $ stypeSubstitute tass (shape typ')
        let (tyclasses, strippedTyp) = traceShow substedTyp $ unprefixTc substedTyp
        let tyclassesPrenex = intercalate ", " $ map show tyclasses
        let breakTypes = map show $ breakdown strippedTyp
        let mkTyclass = printf "%s :: (%s) => (%s)" mkFun tyclassesPrenex (intercalate ", " breakTypes)
        eitherTyclass <- parseExample mdls mkTyclass
        if checked 
            then if null tyclasses || isRight eitherTyclass 
                then return $ Right exTyp
                else return $ Left tcErr
            else return $ Left err
      Left e -> return $ Left e
    where
        mkFun = printf "(%s)" (intercalate ", " $ inputs ex ++ [output ex])

        unprefixTc (FunctionT x tArg tRes) =
            case tArg of
              ScalarT (DatatypeT name args rs) r | tyclassPrefix `isPrefixOf` name ->
                  let (tcs, t) = unprefixTc tRes
                      currTc = ScalarT (DatatypeT (drop (length tyclassPrefix) name) args rs) r
                   in (currTc:tcs, t)
              _ -> ([], FunctionT x tArg tRes)
        unprefixTc t = ([], t)

checkExamples :: Environment -> RSchema -> [Example] -> Chan Message -> IO (Either [ErrorMessage] [RSchema] )
checkExamples env typ exs checkerChan = do
    outExs <- mapM (\ex -> checkExample env typ ex checkerChan) exs
    let (errs, validResults) = partitionEithers outExs
    if null errs then return $ Right validResults
                 else return $ Left errs

execExample :: [String] -> Environment -> TypeQuery -> String -> Example -> IO (Either ErrorMessage String)
execExample mdls env typ prog ex = do
    let args = map fst (env ^. arguments)
    let nontcArgs = filter (not . (tyclassArgBase `isPrefixOf`)) args
    let prependArg = unwords nontcArgs
    let progBody = if null (env ^. arguments) -- if this is a request from front end
        then printf "let internal__f = (%s) :: %s in" prog typ
        else printf "let internal__f = (\\%s -> %s) :: %s in" prependArg prog typ
    let parensedInputs = map wrapParens $ inputs ex
    let progCall = printf "internal__f %s" (unwords parensedInputs)
    result <- handleApproxShowFailure mdls progBody progCall
    let prettyShow a = if "_|_" `isInfixOf` a then "bottom" else a
    return (result >>= Right . prettyShow)

augmentTestSet :: Environment -> RSchema -> IO [Example]
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
                solveTypeConstraint env' (shape s1') (shape s2')) initChecker
            return $ state ^. isChecked

checkExampleOutput :: [String] -> Environment -> TypeQuery -> String -> [Example] -> IO (Maybe [Example])
checkExampleOutput mdls env typ prog exs = do
    let progWithoutTc = removeTypeclasses prog
    currOutputs <- mapM (execExample mdls env typ progWithoutTc) exs
    cmpResults <- mapM (uncurry compareResults) (zip currOutputs exs)
    let justResults = catMaybes cmpResults
    if length justResults == length exs then return $ Just justResults 
                                        else return Nothing
    where
        compareResults currOutput ex
          | output ex == "??" = return $ Just (ex { output = either id id currOutput })
          | otherwise = case currOutput of
                          Left e -> print e >> return Nothing
                          Right o -> do
                              expectedOutput <- handleApproxShowFailure mdls "" (output ex)
                              case expectedOutput of
                                  Left err -> print err >> return Nothing
                                  Right out | o == out -> return (Just ex)
                                            | otherwise -> return Nothing

handleApproxShowFailure :: [String] -> String -> String -> IO (Either ErrorMessage String)
handleApproxShowFailure mdls progBody progCall = do
    let withApproxCall = printf "Test.ChasingBottoms.approxShow 100 (%s)" progCall
    result <- runStmt mdls $ unwords [progBody, withApproxCall]
    case result of
        Left err -> runStmt mdls $ unwords [progBody, progCall]
        Right r -> return result