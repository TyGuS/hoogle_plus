{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, LambdaCase, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module InternalTypeGen where

import Data.List (isInfixOf, elemIndex, nub, reverse, intersect)
import Control.Monad
import Control.Monad.State
import Control.Monad.Logic
import Data.Data
import Data.Maybe

import Text.Printf
import System.IO.Silently
import Control.Lens

import qualified Test.LeanCheck.Function.ShowFunction as SF
import qualified Test.ChasingBottoms as CB
import qualified Test.SmallCheck.Series as SS

defaultShowFunctionDepth = 4 :: Int
defaultMaxOutputLength = 10 :: CB.Nat
defaultSeriesLimit = 4 :: Int

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
instance {-# OVERLAPPABLE #-} SS.Serial m a => SS.Serial m (Inner a) where
  series = SS.newtypeCons Inner
instance {-# OVERLAPPING #-} Monad m => SS.Serial m (Inner Int) where
  series = SS.limit defaultSeriesLimit $ SS.newtypeCons Inner
instance {-# OVERLAPPABLE #-} SS.CoSerial m a => SS.CoSerial m (Inner a) where
  coseries rs = SS.newtypeAlts rs >>- \f ->
                return $ \(Inner x) -> f x
instance Monad m => SS.CoSerial m (Inner Int) where
  coseries rs = do
    -- all possible values for args, value is limited in Serial m (Inner Int)
    args <- unwind SS.series
    -- assign each arg value a new return series
    let rsPairs = zip args (repeat rs)
    -- pick one value from each series
    rets <- foldM stackRs [] rsPairs
    -- an extra series to handle input values outside [-depth, depth]
    rets' <- rs SS.>>- (return . (:rets))
    return $ \x -> case elemIndex x args of
                     Just i -> rets' !! i
                     Nothing -> last rets'
    where
    stackRs sofar (i, rets) = rets SS.>>- (\r -> return (r:sofar))
    unwind a =
      msplit a >>=
      maybe (return []) (\(x,a') -> (x:) `liftM` unwind a')
instance {-# OVERLAPPABLE #-} (SF.ShowFunction a) => Show (Inner a) where
  show x = SF.showFunctionLine defaultShowFunctionDepth x
instance {-# OVERLAPPING #-} (SF.ShowFunction a) => Show (Inner [a]) where
  show (Inner xs) = show $ map Inner xs
instance {-# OVERLAPPING #-} (SF.ShowFunction a) => Show (Inner (a, a)) where
  show (Inner p) = (show . over each Inner) p
instance {-# OVERLAPPING #-} (SF.ShowFunction (a -> b)) => Show (Inner (Inner a -> Inner b)) where
  show fcn = SF.showFunctionLine defaultShowFunctionDepth (unwrap fcn :: a -> b)
instance SF.ShowFunction a => SF.ShowFunction (Inner a) where
  bindtiers (Inner x) = SF.bindtiers x

class Unwrappable a b where
  unwrap :: a -> b
instance Unwrappable a a where unwrap x = x
instance Unwrappable (Inner a) a where unwrap (Inner x) = unwrap x
instance Unwrappable [Inner a] [a] where unwrap = map unwrap
instance Unwrappable (Inner a, Inner b) (a, b) where unwrap (Inner x, Inner y) = (x, y)
instance (Unwrappable (Inner b) b) => Unwrappable (Inner a -> Inner b) (a -> b) where
  unwrap f = \x -> unwrap $ f (Inner x)
instance (Unwrappable (Inner b) b) => Unwrappable (Inner (Inner a -> Inner b)) (a -> b) where
  unwrap (Inner f) = unwrap f

showCBResult :: CB.Result String -> String
showCBResult = \case
                  CB.Value a | "_|_" `isInfixOf` a -> "bottom"
                  CB.Value a -> a
                  CB.NonTermination -> "diverge"
                  CB.Exception ex -> show ex

anyDuplicate :: Eq a => [a] -> Bool
-- anyDuplicate xs = length (nub xs) /= length xs
anyDuplicate [] = False
anyDuplicate (x:xs) = x `elem` xs 

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
