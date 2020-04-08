module InternalTypeGen where

import Data.List (isInfixOf, nub, reverse)
import Control.Monad
import Control.Monad.State

import Text.Printf

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

newtype Inner a = Inner a deriving (Eq)
instance SS.Serial m a => SS.Serial m (Inner a) where series = SS.newtypeCons Inner
instance (SF.ShowFunction a) => Show (Inner a) where
  show (Inner fcn) = SF.showFunctionLine defaultShowFunctionDepth fcn

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

type IOExample = String
type ExampleGeneration m = StateT [IOExample] m

waitState :: Int -> [String] -> CB.Result String -> ExampleGeneration IO Bool
waitState numIOs args ret = case (not $ isFailedResult ret) of
  False -> pure False
  _ -> do
    ioStates <- get
    modify ((++) [printf "%s ==> %s" (unwords args) (showCBResult ret)])

    state <- get
    return ((length state) == numIOs)
