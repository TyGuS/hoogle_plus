module InternalIntGen where

import Test.QuickCheck hiding (quickCheck)
import Control.Applicative

newtype InternalInt = Val Int

instance Show InternalInt where
  show (Val value) = show value

instance Arbitrary InternalInt where
  arbitrary = Val <$> choose (5, 10)