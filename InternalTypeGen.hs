{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, LambdaCase, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
module InternalTypeGen where

import Data.List (isInfixOf, elemIndex, nub, drop, reverse, intersect)
import Data.Containers.ListUtils (nubOrd)

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
import qualified Test.QuickCheck as QC

defaultShowFunctionDepth = 4 :: Int
defaultMaxOutputLength = 10 :: CB.Nat
defaultSeriesLimit = 5 :: Int

instance Eq a => Eq (CB.Result a) where
  (CB.Value a) == (CB.Value b) = a == b
  CB.NonTermination == CB.NonTermination = True
  (CB.Exception _) == (CB.Exception _) = True
  _ == _ = False

instance Ord a => Ord (CB.Result a) where
  (CB.Value a) `compare` (CB.Value b) = a `compare` b
  (CB.Value _) `compare` _ = GT
  (CB.Exception _) `compare` (CB.Exception _) = EQ
  (CB.Exception _) `compare` (CB.Value _) = LT
  (CB.Exception _) `compare` _ = GT
  (CB.NonTermination) `compare` (CB.NonTermination) = EQ
  (CB.NonTermination) `compare` _ = LT

isFailedResult :: CB.Result String -> Bool
isFailedResult result = case result of
  CB.NonTermination -> True
  CB.Exception _ -> True
  CB.Value a | "_|_" `isInfixOf` a -> True
  CB.Value a | "Exception" `isInfixOf` a -> True
  _ -> False

showCBResult :: CB.Result String -> String
showCBResult = \case
                  CB.Value a | "_|_" `isInfixOf` a -> "bottom"
                  CB.Value a -> a
                  CB.NonTermination -> "diverge"
                  CB.Exception ex -> show ex

anyDuplicate :: Ord a => [a] -> Bool
anyDuplicate [x, y] = x == y
anyDuplicate xs = length (nubOrd xs) /= length xs

evaluateValue :: Data a => Int -> [a] -> IO [CB.Result String]
evaluateValue timeInMicro values = mapM (silence . eval) values
  where
    eval value =
      let str = CB.approxShow defaultMaxOutputLength value in
        CB.timeOutMicro timeInMicro (putStrLn str >> return str)

labelProperty :: QC.Testable prop => [String] -> [CB.Result String] -> prop -> QC.Property
labelProperty inputs outputs prop = QC.label (show examples) prop
  where examples = map (Example inputs . showCBResult) outputs

-- * instance defined in `Types.IOFormat`
data Example = Example {
    inputs :: [String],
    output :: String
} deriving(Eq, Show, Read)

-- * Custom Datatype for Range Restriction
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

instance (QC.CoArbitrary a, QC.Arbitrary b) => QC.Arbitrary (MyFun a b) where
  arbitrary = liftM MyFun QC.arbitrary

instance (QC.Arbitrary a, QC.CoArbitrary b) => QC.CoArbitrary (MyFun a b) where
  coarbitrary (MyFun f) = QC.coarbitrary f

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
