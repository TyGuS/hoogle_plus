module InternalTypeGen where

import Data.List (isInfixOf)

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
