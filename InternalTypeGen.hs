{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, LambdaCase, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module InternalTypeGen where

import Data.List (isInfixOf, nub, reverse, intersect)
import Control.Monad
import Control.Monad.State
import Data.Data

import Text.Printf
import System.IO.Silently
import Control.Lens

import qualified Test.LeanCheck.Function.ShowFunction as SF
import qualified Test.ChasingBottoms as CB
import qualified Test.SmallCheck.Series as SS

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

newtype Inner a = Inner a deriving (Eq)
instance SS.Serial m a => SS.Serial m (Inner a) where series = SS.newtypeCons Inner
instance {-# OVERLAPPABLE #-} (SF.ShowFunction a) => Show (Inner a) where
  show (Inner fcn) = SF.showFunctionLine defaultShowFunctionDepth fcn
instance {-# OVERLAPPING #-} (SF.ShowFunction a) => Show (Inner [a]) where
  show (Inner xs) = show $ map Inner xs
instance {-# OVERLAPPING #-} (SF.ShowFunction a) => Show (Inner (a, a)) where
  show (Inner p) = (show . over each Inner) p
instance SF.ShowFunction a => SF.ShowFunction (Inner a) where
  bindtiers (Inner x) = SF.bindtiers x

class Unwrappable a b where
  unwrap :: a -> b
instance Unwrappable (Inner a) a where unwrap (Inner x) = x
instance Unwrappable ([Inner a]) [a] where unwrap = map unwrap
instance Unwrappable (Inner a, Inner b) (a, b) where unwrap (Inner x, Inner y) = (x, y)

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
 
waitState :: Int -> [String] -> [String] -> [[String]] -> CB.Result String -> ExampleGeneration IO Bool
waitState numIOs args previousRets previousArgs ret = case (not $ isFailedResult ret) of
  False -> pure False
  _ -> do
    ioState <- get

    let retStr = showCBResult ret
    when (retIsNotInState retStr ioState && paramsIsNotInState args ioState) (modify ((:) (Example args retStr)))

    state <- get
    return ((length state) == numIOs)
  where 
    retIsNotInState retStr state = not $ ((retStr `elem` (map output state)) || (retStr `elem` previousRets))
    paramsIsNotInState params state = not $ ((anyCommonArgs params (map inputs state)) || (params `elem` previousArgs))


anyCommonArgs :: [String] -> [[String]] -> Bool
anyCommonArgs args inputs = any id $ map (compare args) inputs
  where
    compare :: [String] -> [String] -> Bool
    compare xs = not . null . intersect xs