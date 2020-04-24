{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, LambdaCase, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module InternalTypeGen where

import Data.List (isInfixOf, elemIndex, nub, drop, reverse, intersect)
import Control.Monad
import Control.Monad.State
import Control.Monad.Logic
import Data.Data

import Text.Printf
import System.IO.Silently
import Control.Lens

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

newtype Inner a = Inner a deriving (Eq)

instance {-# OVERLAPPABLE #-} SS.Serial m a => SS.Serial m (Inner a) where
  series = SS.newtypeCons Inner
instance {-# OVERLAPPING #-} Monad m => SS.Serial m (Inner (Int -> Int)) where
  series = (SS.generate $ \_ -> map Inner [\x -> x + 1
                                          ,\x -> x * x
                                          ,\x -> x * 3]) SS.\/
            SS.newtypeCons Inner
instance {-# OVERLAPPING #-} Monad m => SS.Serial m (Inner (MyInt -> Int)) where
  series = (SS.generate $ \_ -> map Inner [\x -> toInt x + 1
                                          ,\x -> toInt x * toInt x
                                          ,\x -> toInt x * 3]) SS.\/
            SS.newtypeCons Inner
instance {-# OVERLAPPING #-} Monad m => SS.Serial m (Inner (Inner Int -> Inner Int)) where
  series = (SS.generate $ \_ -> map Inner [\(Inner x) -> Inner (x + 1)
                                          ,\(Inner x) -> Inner (x * x)
                                          ,\(Inner x) -> Inner (x * 3)]) SS.\/
            SS.newtypeCons Inner
instance {-# OVERLAPPING #-} Monad m => SS.Serial m (Inner (Inner MyInt -> Inner Int)) where
  series = (SS.generate $ \_ -> map Inner [\(Inner x) -> Inner (toInt x + 1)
                                          ,\(Inner x) -> Inner (toInt x * toInt x)
                                          ,\(Inner x) -> Inner (toInt x * 3)]) SS.\/
            SS.newtypeCons Inner
instance {-# OVERLAPPING #-} Monad m => SS.Serial m (Inner ([Int] -> [Int])) where
  series = (SS.generate $ \_ -> map Inner [\x -> x ++ x]) SS.\/
            SS.newtypeCons Inner
instance {-# OVERLAPPING #-} Monad m => SS.Serial m (Inner (Inner [Int] -> Inner [Int])) where
  series = (SS.generate $ \_ -> map Inner [\(Inner x) -> Inner (x ++ x)]) SS.\/
            SS.newtypeCons Inner
instance {-# OVERLAPPABLE #-} SS.CoSerial m a => SS.CoSerial m (Inner a) where
  coseries rs = SS.newtypeAlts rs >>- \f ->
                return $ \(Inner x) -> f x

instance {-# OVERLAPPABLE #-} (Show a) => Show (Inner a) where
  show (Inner x) = show x
instance {-# OVERLAPPING #-} Show (Inner MyInt) where
  show (Inner x) = show $ toInt x
instance {-# OVERLAPPING #-} Show (Inner Int) where
  show (Inner x) = if x < 0 then "(" ++ show x ++ ")" else show x
instance {-# OVERLAPPING #-} (Show a) => Show (Inner [a]) where
  show (Inner xs) = show (map (wrap :: a -> Inner a) xs)
instance {-# OVERLAPPING #-} (Show a) => Show (Inner (Maybe a)) where
  show (Inner mb) = show $ fmap (wrap :: a -> Inner a) mb
instance {-# OVERLAPPING #-} (Show a, Show b) => Show (Inner (a, b)) where
  show (Inner (f, s)) = show $ ((wrap :: a -> Inner a) f, (wrap :: b -> Inner b) s)
instance {-# OVERLAPPING #-} (Show b) => Show (Inner (MyInt, b)) where
  show (Inner (f, s)) = show $ (toInt f, (wrap :: b -> Inner b) s)
instance {-# OVERLAPPING #-} (Show a) => Show (Inner (a, MyInt)) where
  show (Inner (f, s)) = show $ ((wrap :: a -> Inner a) f, toInt s)
instance {-# OVERLAPPING #-} (Unwrappable a Int, Unwrappable b Int) => Show (Inner (a -> b)) where
  show (Inner f) = SF.showFunctionLine defaultShowFunctionDepth (unwrap f :: Int -> Int)
instance {-# OVERLAPPING #-} (Unwrappable a Int, Unwrappable b Int, Unwrappable c Int) => Show (Inner (a -> Inner (b -> c))) where
  show (Inner f) = SF.showFunctionLine defaultShowFunctionDepth (unwrap f :: Int -> Int -> Int)
instance {-# OVERLAPPING #-} (Unwrappable a Int, Unwrappable b Int, Unwrappable c Int, Unwrappable d Int) => Show (Inner (a -> Inner (b -> Inner (c -> d)))) where
  show (Inner f) = SF.showFunctionLine defaultShowFunctionDepth (unwrap f :: Int -> Int -> Int -> Int)
instance (SF.ShowFunction a) => SF.ShowFunction (Inner a) where
  bindtiers (Inner x) = SF.bindtiers x
instance SF.ShowFunction MyInt where
  bindtiers = SF.bindtiers . toInt

class Unwrappable a b where
  wrap :: b -> a
  unwrap :: a -> b
instance Unwrappable MyInt Int where
  wrap = toMyInt
  unwrap = toInt
instance {-# OVERLAPPABLE #-} Unwrappable a a where
  wrap x = x
  unwrap x = x
instance Unwrappable a b => Unwrappable (Inner a) b where
  wrap = Inner . wrap
  unwrap (Inner x) = unwrap x
instance {-# OVERLAPPABLE #-} Unwrappable a b => Unwrappable [a] [b] where
  wrap = map wrap
  unwrap = map unwrap
instance {-# OVERLAPPING #-} Unwrappable [a] [a] where
  wrap x = x
  unwrap x = x
instance {-# OVERLAPPABLE #-} Unwrappable a b => Unwrappable (Maybe a) (Maybe b) where
  wrap = fmap wrap
  unwrap = fmap unwrap
instance {-# OVERLAPPING #-} Unwrappable (Maybe a) (Maybe a) where
  wrap x = x
  unwrap x = x
instance {-# OVERLAPPABLE #-} (Unwrappable a c, Unwrappable b d) => Unwrappable (a, b) (c, d) where
  wrap (x, y) = (wrap x, wrap y)
  unwrap (x, y) = (unwrap x, unwrap y)
instance {-# OVERLAPPABLE #-} (Unwrappable a c, Unwrappable b d, Unwrappable e f) => Unwrappable (a, b, e) (c, d, f) where
  wrap (x, y, z) = (wrap x, wrap y, wrap z)
  unwrap (x, y, z) = (unwrap x, unwrap y, unwrap z)
instance {-# OVERLAPPABLE #-} (Unwrappable a c, Unwrappable b d, Unwrappable e f, Unwrappable g h) => Unwrappable (a, b, e, g) (c, d, f, h) where
  wrap (x, y, z, s) = (wrap x, wrap y, wrap z, wrap s)
  unwrap (x, y, z, s) = (unwrap x, unwrap y, unwrap z, unwrap s)
instance {-# OVERLAPPING #-} Unwrappable (a, a) (a, a) where
  wrap x = x
  unwrap x = x
instance {-# OVERLAPPABLE #-} (Unwrappable a c, Unwrappable b d) => Unwrappable (a -> b) (c -> d) where
  wrap f = \x -> wrap $ f (unwrap x)
  unwrap f = \x -> unwrap $ f (wrap x)
instance {-# OVERLAPPING #-} Unwrappable (a -> a) (a -> a) where
  wrap x = x
  unwrap x = x

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
