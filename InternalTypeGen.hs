{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, LambdaCase, FlexibleContexts #-}
module InternalTypeGen where

import Data.List (isInfixOf, nub, reverse)
import Control.Monad
import Control.Monad.State
import Data.Data

import Text.Printf
import System.IO.Silently
import Test.LeanCheck.Function.Show.FourCases

import qualified Test.ChasingBottoms as CB

defaultShowFunctionDepth = 4 :: Int
defaultMaxOutputLength = 10 :: CB.Nat

instance Eq a => Eq (CB.Result a) where
  (CB.Value a) == (CB.Value b) = a == b
  CB.NonTermination == CB.NonTermination = True
  (CB.Exception _) == (CB.Exception _) = True
  _ == _ = False

isFailedResult :: CB.Result String -> Bool
isFailedResult result = case result of
  CB.NonTermination -> True
  CB.Exception _ -> True
  CB.Value a | "_|_" `isInfixOf` a -> True
  CB.Value a | "Exception" `isInfixOf` a -> True
  _ -> False

printIOResult :: [String] -> [CB.Result String] -> IO ()
printIOResult args rets = (putStrLn . show) result
  where result = (args, map showCBResult rets) :: ([String], [String])

showCBResult :: CB.Result String -> String
showCBResult = \case
                  CB.Value a | "_|_" `isInfixOf` a -> "bottom"
                  CB.Value a -> a
                  CB.NonTermination -> "diverge"
                  CB.Exception ex -> show ex

anyDuplicate :: Eq a => [a] -> Bool
anyDuplicate xs = length (nub xs) /= length xs

-- * instance defined in `Types.IOFormat`
data Example = Example {
    inputs :: [String],
    output :: String
} deriving(Eq, Show)
type ExampleGeneration m = StateT [Example] m

evaluateIO :: Data a => Int -> [String] -> [a] -> ExampleGeneration IO ([CB.Result String])
evaluateIO timeInMicro inputs vals = do
    results <- liftIO $ silence $ mapM (CB.timeOutMicro timeInMicro . eval) vals
    
    let resultsStr = map showCBResult results
    modify ((++) (map (Example inputs) resultsStr))
    return results
  where
    evalStr val = CB.approxShow defaultMaxOutputLength val
    io str = ((putStrLn str) >> return str)

    eval val = io (evalStr val)
 
waitState :: Int -> [String] -> [String] -> CB.Result String -> ExampleGeneration IO Bool
waitState numIOs args previous ret = case (not $ isFailedResult ret) of
  False -> pure False
  _ -> do
    ioState <- get

    let retStr = showCBResult ret
    when (isNotInState retStr ioState) (modify ((:) (Example args retStr)))

    state <- get
    return ((length state) == numIOs)
  where isNotInState retStr state = not $ ((retStr `elem` (map output state)) || (retStr `elem` previous))
