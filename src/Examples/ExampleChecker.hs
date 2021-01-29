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
import Types.Filtering (defaultTimeoutMicro, defaultDepth, defaultInterpreterTimeoutMicro, frameworkModules)
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

checkExample :: Environment -> RSchema -> Example -> Chan Message -> IO (Either RSchema ErrorMessage)
checkExample env typ ex checkerChan = do
    let mdls = Set.toList $ env ^. included_modules
    eitherTyp <- parseExample mdls mkFun
    case eitherTyp of
      Left exTyp -> do
        let err = printf "%s does not have type %s" (show ex) (show typ) :: String
        let tcErr = printf "%s does not satisfy type class constraint in %s" (show ex) (show typ) :: String
        (res, substedTyp) <- checkTypes env checkerChan exTyp typ
        let (tyclasses, strippedTyp) = unprefixTc substedTyp
        let tyclassesPrenex = intercalate ", " $ map show tyclasses
        let breakTypes = map show $ breakdown strippedTyp
        let mkTyclass = printf "%s :: (%s) => (%s)" mkFun tyclassesPrenex (intercalate ", " breakTypes)
        eitherTyclass <- if null tyclasses then return (Left (Monotype AnyT)) else parseExample mdls mkTyclass
        if res then if isLeft eitherTyclass then return $ Left exTyp
                                            else return $ Right tcErr
               else return $ Right err
      Right e -> return $ Right e
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

checkExamples :: Environment -> RSchema -> [Example] -> Chan Message -> IO (Either [RSchema] [ErrorMessage])
checkExamples env typ exs checkerChan = do
    outExs <- mapM (\ex -> checkExample env typ ex checkerChan) exs
    let (validResults, errs) = partitionEithers outExs
    if null errs then return $ Left validResults
                 else return $ Right errs

execExample :: [String] -> Environment -> TypeQuery -> String -> Example -> IO (Either ErrorMessage String)
execExample mdls env typ prog ex = do
    let args = Map.keys $ env ^. arguments
    let nontcArgs = filter (not . (tyclassArgBase `isPrefixOf`)) args
    let prependArg = unwords nontcArgs
    let progBody = if Map.null (env ^. arguments) -- if this is a request from front end
        then printf "let f = (%s) :: %s in" prog typ
        else printf "let f = (\\%s -> %s) :: %s in" prependArg prog typ
    let parensedInputs = map wrapParens $ inputs ex
    -- let progCall = printf "Test.ChasingBottoms.approxShow 100 (f %s)" (unwords parensedInputs)
    let progCall = printf "(f %s)" (unwords parensedInputs)
    runStmt mdls $ unwords [progBody, progCall]

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
                          Left e -> return Nothing
                          Right o -> do
                              expectedOutput <- runStmt mdls (printf "(%s)" $ output ex)
                              --   expectedOutput <- runStmt mdls (printf "Test.ChasingBottoms.approxShow 100 (%s)" $ output ex)
                              case expectedOutput of
                                  Left err -> return Nothing
                                  Right out | o == out -> return (Just ex)
                                            | otherwise -> return Nothing

