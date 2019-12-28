{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module InternalTypeGen where

import Control.Applicative
import Control.Monad
import Data.List (isInfixOf)
import Data.Map (Map)
import Data.Universe

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe (promote)

import qualified Test.ChasingBottoms as CB

isEqualResult lhs rhs = case (lhs, rhs) of
  (CB.Value a, CB.Value b) -> a == b
  (CB.NonTermination, CB.NonTermination) -> True
  (CB.Exception _, CB.Exception _) -> True
  _ -> False

isFailedResult result = case result of
  CB.NonTermination -> True
  CB.Exception _ -> True
  CB.Value a | "_|_" `isInfixOf` a -> True
  CB.Value a | "Exception" `isInfixOf` a -> True
  _ -> False

newtype Internal a = Val a deriving Eq
instance Functor Internal where fmap f (Val v) = Val (f v)
instance Applicative Internal where
  pure = Val
  Val f <*> Val x = Val (f x)
instance Monad Internal where
  (Val v) >>= f = f v
  return = Val
instance Show a => Show (Internal a) where
  show (Val value) = show value

instance Arbitrary a => Arbitrary (Internal a) where
  arbitrary = pure <$> arbitrary
instance CoArbitrary a => CoArbitrary (Internal a) where
  coarbitrary (Val n) = coarbitrary n

instance {-# OVERLAPPING #-} Arbitrary (Internal Int) where
  arbitrary = pure <$> choose (0, 4)
instance {-# OVERLAPPING #-} Arbitrary (Internal Char) where
  arbitrary = pure <$> choose ('a', 'd')
instance {-# OVERLAPPING #-} Arbitrary (Internal String) where
  arbitrary = pure <$> listOf (choose ('a', 'd'))

-- finite values

instance Universe (Internal Int) where universe = map pure [0..4]
instance Universe (Internal Char) where universe = map pure ['a'..'d']

instance Universe (Internal a) => Universe (Internal [a]) where
  universe = map sequence (powerset universe)
    where powerset = filterM (const [True, False])

instance Universe (Internal a) => Finite (Internal a) where
  universeF = universe
