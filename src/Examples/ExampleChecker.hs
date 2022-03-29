module Examples.ExampleChecker
  ( execExample
  , checkExamples
  , checkExampleOutput
  ) where

import           Control.Monad                  ( zipWithM )
import           Control.Monad.State            ( evalStateT )
import           Data.Either                    ( isRight
                                                , partitionEithers
                                                )
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Text.Printf                    ( printf )

import           Examples.InferenceDriver
import           Examples.Utils
import           HooglePlus.Utils
import           Types.Common
import           Types.Environment
import           Types.Filtering                ( defaultDepth
                                                , defaultInterpreterTimeoutMicro
                                                , defaultTimeoutMicro
                                                , frameworkModules
                                                )
import           Types.Fresh
import           Types.Type

checkExample
  :: [Id]
  -> Environment
  -> SchemaSkeleton
  -> Example
  -> IO (Either ErrorMessage SchemaSkeleton)
checkExample mdls env typ ex = do
  eitherTyp <- parseExample mdls mkFun
  case eitherTyp of
    Right exTyp -> do
      let err =
            printf "%s does not have type %s" (show ex) (show typ) :: String
      let tcErr =
            printf "%s does not satisfy type class constraint in %s"
                   (show ex)
                   (show typ) :: String
      -- refresh the type variables names in the two
      let bvs = getBoundTypeVars env
      (freshExTyp, freshTyp) <- evalStateT
        (do
          t1 <- toMonotype <$> freshSchema bvs exTyp
          t2 <- toMonotype <$> freshSchema bvs typ
          return (t1, t2)
        )
        (Map.empty :: Map Id Int)
      let mbTass = checkTypes env [] freshExTyp freshTyp
      case mbTass of
        Nothing   -> return $ Left err
        Just tass -> do
          let substedTyp               = typeSubstitute tass freshTyp
          let (tyclasses, strippedTyp) = unprefixTc substedTyp
          let tyclassesPrenex          = intercalate ", " $ map show tyclasses
          let breakTypes               = map show $ breakdown strippedTyp
          let mkTyclass = printf "%s :: (%s) => (%s)"
                                 mkFun
                                 tyclassesPrenex
                                 (intercalate ", " breakTypes)
          eitherTyclass <- parseExample mdls mkTyclass
          if null tyclasses || isRight eitherTyclass
            then return $ Right exTyp
            else return $ Left tcErr
    Left e -> return $ Left e
 where
  mkFun = printf "(%s)" (intercalate ", " $ inputs ex ++ [output ex])

  unprefixTc (FunctionT x tArg tRes) = case tArg of
    DatatypeT name args | tyclassPrefix `Text.isPrefixOf` name ->
      let (tcs, t) = unprefixTc tRes
          currTc   = DatatypeT (Text.drop (Text.length tyclassPrefix) name) args
      in  (currTc : tcs, t)
    _ -> ([], FunctionT x tArg tRes)
  unprefixTc t = ([], t)

checkExamples
  :: [Id]
  -> Environment
  -> SchemaSkeleton
  -> [Example]
  -> IO (Either [ErrorMessage] [SchemaSkeleton])
checkExamples mdls env typ exs = do
  outExs <- mapM (checkExample mdls env typ) exs
  let (errs, validResults) = partitionEithers outExs
  if null errs then return $ Right validResults else return $ Left errs

execExample
  :: [Id]
  -> Environment
  -> TypeQuery
  -> String
  -> Example
  -> IO (Either ErrorMessage String)
execExample mdls env typ prog ex = do
  let args       = map fst (getArguments env)
  let nontcArgs = filter (not . (tyclassArgBase `Text.isPrefixOf`)) args
  let prependArg = Text.unwords nontcArgs
  let progBody = if null args -- if this is a request from front end
        then printf "let internal__f = (%s) :: %s in" prog typ
        else printf "let internal__f = (\\%s -> %s) :: %s in"
                    prependArg
                    prog
                    typ
  let parensedInputs = map wrapParens $ inputs ex
  let progCall       = printf "internal__f %s" (unwords parensedInputs)
  result <- handleApproxShowFailure mdls progBody progCall
  let prettyShow a = if "_|_" `isInfixOf` a then "bottom" else a
  return (result >>= Right . prettyShow)

checkExampleOutput
  :: [Id]
  -> Environment
  -> TypeQuery
  -> String
  -> [Example]
  -> IO (Maybe [Example])
checkExampleOutput mdls env typ prog exs = do
  let progWithoutTc = removeTypeclasses prog
  currOutputs <- mapM (execExample mdls env typ progWithoutTc) exs
  cmpResults  <- zipWithM compareResults currOutputs exs
  let justResults = catMaybes cmpResults
  if length justResults == length exs
    then return $ Just justResults
    else return Nothing
 where
  compareResults currOutput ex
    | output ex == "??" = return
    $ Just (ex { output = either id id currOutput })
    | otherwise = case currOutput of
      Left  e -> print e >> return Nothing
      Right o -> do
        expectedOutput <- handleApproxShowFailure mdls "" (output ex)
        case expectedOutput of
          Left err -> print err >> return Nothing
          Right out | o == out  -> return (Just ex)
                    | otherwise -> return Nothing

handleApproxShowFailure
  :: [Id] -> String -> String -> IO (Either ErrorMessage String)
handleApproxShowFailure mdls progBody progCall = do
  let withApproxCall =
        printf "Test.ChasingBottoms.approxShow 100 (%s)" progCall
  result <- runStmt mdls $ unwords [progBody, withApproxCall]
  case result of
    Left  err -> runStmt mdls $ unwords [progBody, progCall]
    Right r   -> return result
