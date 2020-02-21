{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module InternalTypeGen where

import Data.List (isInfixOf)

import qualified Test.LeanCheck.Function.ShowFunction as SF
import qualified Test.ChasingBottoms as CB
import qualified Test.SmallCheck.Series as SS

instance Eq a => Eq (CB.Result a) where
  (CB.Value a) == (CB.Value b) = a == b
  CB.NonTermination == CB.NonTermination = True
  (CB.Exception _) == (CB.Exception _) = True
  _ == _ = False

isFailedResult result = case result of
  CB.NonTermination -> True
  CB.Exception _ -> True
  CB.Value a | "_|_" `isInfixOf` a -> True
  CB.Value a | "Exception" `isInfixOf` a -> True
  _ -> False

newtype Inner a = Inner a deriving (Eq)
instance SS.Serial m a => SS.Serial m (Inner a) where series = SS.newtypeCons Inner
instance (SF.ShowFunction a) => Show (Inner a) where
  show (Inner fcn) = SF.showFunctionLine 4 fcn
