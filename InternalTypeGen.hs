{-# LANGUAGE FlexibleInstances #-}
module InternalTypeGen where

import Test.QuickCheck hiding (quickCheck)
import Test.QuickCheck.Gen
import Data.Map (Map)
import Control.Applicative

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