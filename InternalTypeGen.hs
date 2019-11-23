{-# LANGUAGE FlexibleInstances #-}
module InternalTypeGen where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Test.ChasingBottoms as CB
import Data.Map (Map)
import Data.List (isInfixOf)
import Control.Applicative

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

newtype Internal a = Val a

instance Show a => Show (Internal a) where
  show (Val value) = show value

instance Arbitrary a => Arbitrary (Internal a) where
  arbitrary = Val <$> arbitrary

instance {-# OVERLAPPING #-} Arbitrary (Internal Int) where
  arbitrary = Val <$> choose (5, 10)

instance {-# OVERLAPPING #-} Arbitrary (Internal Char) where
  arbitrary = Val <$> choose ('A', 'D')

instance {-# OVERLAPPING #-} Arbitrary (Internal String) where
  arbitrary = Val <$> vectorOf 5 (choose ('A', 'D'))