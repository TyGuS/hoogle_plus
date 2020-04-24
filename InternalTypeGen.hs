{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, LambdaCase, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
module InternalTypeGen where

import Data.List (isInfixOf, elemIndex, nub, drop, reverse, intersect)
import Control.Monad
import Control.Monad.State
import Control.Monad.Logic
import Data.Data

import Text.Printf
import System.IO.Silently
import Control.Lens
import Debug.Trace

import qualified Test.LeanCheck.Function.ShowFunction as SF
import qualified Test.LeanCheck.Core as SF
import qualified Test.ChasingBottoms as CB
import qualified Test.SmallCheck.Series as SS

defaultShowFunctionDepth = 4 :: Int
defaultMaxOutputLength = 10 :: CB.Nat
defaultSeriesLimit = 5 :: Int

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

data MyInt = MOne | Zero | One | Two | Other Int deriving (Eq)

instance Show MyInt where
  show = show . toInt

toInt :: MyInt -> Int
toInt MOne = -1
toInt Zero = 0
toInt One = 1
toInt Two = 2
toInt (Other n) = n

toMyInt :: Int -> MyInt
toMyInt (-1) = MOne
toMyInt 0 = Zero
toMyInt 1 = One
toMyInt 2 = Two
toMyInt n = Other n

instance SF.Listable MyInt where
  tiers = SF.cons0 MOne SF.\/
          SF.cons0 Zero SF.\/
          SF.cons0 One SF.\/
          SF.cons0 Two SF.\/
          SF.cons1 Other

instance Monad m => SS.Serial m MyInt where
  series = SS.cons0 MOne SS.\/
           SS.cons0 Zero SS.\/
           SS.cons0 One SS.\/
           SS.cons0 Two

instance Monad m => SS.CoSerial m MyInt where
  coseries r = let rs = SS.limit defaultSeriesLimit r
                in SS.alts0 rs >>- \z1 ->
                   SS.alts0 rs >>- \z2 ->
                   SS.alts0 rs >>- \z3 ->
                   SS.alts0 rs >>- \z4 ->
                   return $ \x ->
                       case x of
                           MOne -> z1
                           Zero -> z2
                           One -> z3
                           Two -> z4

newtype MyFun a b = MyFun (a -> b)

instance {-# OVERLAPPABLE #-} (SS.Serial m b, SS.CoSerial m a) => SS.Serial m (MyFun a b) where
  series = SS.coseries SS.series >>-
            \f -> return (MyFun f)

instance {-# OVERLAPPING #-} Monad m => SS.Serial m (MyFun Int Int) where
  series = (SS.generate $ \_ -> map MyFun [\x -> x + 1
                                          ,\x -> x * x
                                          ,\x -> x * 3]) SS.\/
            SS.newtypeCons MyFun
instance {-# OVERLAPPING #-} Monad m => SS.Serial m (MyFun MyInt Int) where
  series = (SS.generate $ \_ -> map MyFun [\x -> toInt x + 1
                                          ,\x -> toInt x * toInt x
                                          ,\x -> toInt x * 3]) SS.\/
            (SS.coseries SS.series >>- 
                \f -> return (MyFun f))
instance {-# OVERLAPPING #-} Monad m => SS.Serial m (MyFun [Int] [Int]) where
  series = (SS.generate $ \_ -> map MyFun [\x -> x ++ x]) SS.\/
            SS.newtypeCons MyFun
instance (SS.CoSerial m a, SS.Serial m a, SS.Serial m b, SS.CoSerial m b) => SS.CoSerial m (MyFun a b) where
  coseries rs = SS.newtypeAlts rs >>- \f ->
                return $ \(MyFun x) -> f x

instance {-# OVERLAPPABLE #-} (Show a, SF.Listable a, SF.ShowFunction b) => Show (MyFun a b) where
  show (MyFun f) = "(" ++ SF.showFunctionLine defaultShowFunctionDepth f ++ ")"
instance {-# OVERLAPPING #-} (SF.ShowFunction b) => Show (MyFun MyInt b) where
  show (MyFun f) = "(" ++ SF.showFunctionLine defaultShowFunctionDepth (\x -> f (toMyInt x)) ++ ")"

instance SF.ShowFunction MyInt where
  bindtiers = SF.bindtiers . toInt
instance (Show a, SF.Listable a, SF.ShowFunction b) => SF.ShowFunction (MyFun a b) where
  bindtiers (MyFun f) = SF.bindtiers f

class Unwrappable a b where
  unwrap :: a -> b
  wrap :: b -> a

instance {-# OVERLAPPABLE #-} (a ~ b) => Unwrappable a b where
  unwrap = id
  wrap = id
instance Unwrappable MyInt Int where
  unwrap = toInt
  wrap = toMyInt
instance (Unwrappable a c, Unwrappable b d) => Unwrappable (MyFun a b) (c -> d) where
  unwrap (MyFun f) = \x -> unwrap (f (wrap x))
  wrap f = MyFun $ \x -> wrap (f (unwrap x))
instance (Unwrappable a b) => Unwrappable [a] [b] where
  unwrap = map unwrap
  wrap = map wrap
instance {-# OVERLAPPING #-} (Unwrappable a b) => Unwrappable (Maybe a) (Maybe b) where
  unwrap = fmap unwrap
  wrap = fmap wrap
instance (Unwrappable a c, Unwrappable b d) => Unwrappable (a, b) (c, d) where
  unwrap (x, y) = (unwrap x, unwrap y)
  wrap (x, y) = (wrap x, wrap y)


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
    when (retIsNotInState retStr ioState && paramsIsNotInState args ioState)
        (modify ((:) (Example args retStr)))

    state <- get
    return ((length state) == numIOs)
  where 
    retIsNotInState retStr state = not $ ((retStr `elem` (map output state)) || (retStr `elem` previousRets))
    paramsIsNotInState params state = not (anyCommonArgs params (map inputs state ++ previousArgs))

-- modify 2020/04/22 by Zheng
-- only compare arguments in the same position, intersect is too strict
anyCommonArgs :: [String] -> [[String]] -> Bool
anyCommonArgs args inputs = or $ map (compare args) inputs
  where
    compare :: [String] -> [String] -> Bool
    compare xs = any (uncurry (==)) . zip xs
