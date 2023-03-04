{-# LANGUAGE OverloadedStrings #-}

module ECTASpec ( spec ) where

import Control.Exception (evaluate)
import qualified Data.HashSet as HashSet
import Data.IORef ( newIORef, readIORef, modifyIORef )
import qualified Data.Text as Text
import           Data.Set ( Set )
import qualified Data.Set as Set

import System.IO.Unsafe ( unsafePerformIO )

import Test.Hspec
import Test.QuickCheck

import Data.ECTA
import Data.ECTA.Internal.ECTA.Operations
import Data.ECTA.Internal.ECTA.Type
import Data.ECTA.Internal.Paths
import Data.ECTA.Term
import TermSearch

import Test.Generators.ECTA ()

-----------------------------------------------------------------


constTerms :: [Symbol] -> Node
constTerms ss = Node (map (\s -> Edge s []) ss)

ex1 :: Node
ex1 = Node [mkEdge "f" [constTerms ["1", "2"], Node [Edge "g" [constTerms ["1", "2"]]]] (mkEqConstraints [[path [0], path [1,0]]])]

ex2 :: Node
ex2 = Node [mkEdge "f" [constTerms ["1", "2", "3"], Node [Edge "g" [constTerms ["1", "2", "4"]]]] (mkEqConstraints [[path [0], path [1,0]]])]

ex3 :: Node
ex3 = Node [Edge "f" [Node [Edge "g" [constTerms ["1", "2"]]]], Edge "h" [Node [Edge "i" [constTerms ["3", "4"]]]]]

ex3_root_doubled :: Node
ex3_root_doubled = Node [Edge "ff" [Node [Edge "g" [constTerms ["1", "2"]]]], Edge "hh" [Node [Edge "i" [constTerms ["3", "4"]]]]]

ex3_doubled :: Node
ex3_doubled = Node [Edge "f" [Node [Edge "g" [constTerms ["11", "22"]]]], Edge "h" [Node [Edge "i" [constTerms ["33", "44"]]]]]

doubleNodeSymbols :: Node -> Node
doubleNodeSymbols (Node es) = Node $ map doubleEdgeSymbol es
  where
    doubleEdgeSymbol :: Edge -> Edge
    doubleEdgeSymbol (Edge (Symbol s) ns) = Edge (Symbol (Text.append s s)) ns
doubleNodeSymbols n         = error $ "doubleNodeSymbols: unexpected " <> show n

testBigNode :: Node
testBigNode = uptoDepth4

_testUnreducedConstraint :: Edge
_testUnreducedConstraint = mkEdge "foo" [Node [Edge "A" [], Edge "B" []], Node [Edge "B" [], Edge "C" []]] (mkEqConstraints [[path [0], path [1]]])

bug062721NonIdempotentEqConstraintReduction :: (EqConstraints, [Node])
bug062721NonIdempotentEqConstraintReduction =
  ( EqConstraints {getEclasses = [PathEClass [Path [0],Path [2,0,2]],PathEClass [Path [1],Path [2,0,0]],PathEClass [Path [2,0,1],Path [3,0]]]}
  , [(Node [(Edge "baseType" [])]),(Node [(Edge "(->)" [])]),(Node [(mkEdge "app" [(Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))]),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])]),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "(->)" [])]),(Node [(Edge "g" [(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "baseType" [])]),(Node [(Edge "baseType" [])])])])]),(Edge "x" [(Node [(Edge "baseType" [])])]),(Edge "n" [(Node [(Edge "Int" [])])]),(Edge "$" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,1]],PathEClass [Path [1,2],Path [2,2]]]})])]),(Edge "replicate" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "Int" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [2,1],Path [2,2,0]]]})])]),(Edge "foldr" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,2,1,0]],PathEClass [Path [1,2,1],Path [1,2,2],Path [2,1],Path [2,2,2]]]})])])]),(Node [(Edge "g" [(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "baseType" [])]),(Node [(Edge "baseType" [])])])])]),(Edge "x" [(Node [(Edge "baseType" [])])]),(Edge "n" [(Node [(Edge "Int" [])])]),(Edge "$" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,1]],PathEClass [Path [1,2],Path [2,2]]]})])]),(Edge "replicate" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "Int" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [2,1],Path [2,2,0]]]})])]),(Edge "foldr" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,2,1,0]],PathEClass [Path [1,2,1],Path [1,2,2],Path [2,1],Path [2,2,2]]]})])])])] EqConstraints {getEclasses = [PathEClass [Path [0],Path [2,0,2]],PathEClass [Path [1],Path [2,0,0]],PathEClass [Path [2,0,1],Path [3,0]]]})]),(Node [(mkEdge "app" [(Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))]),(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))]),(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))]),(Edge "Maybe" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(Node [(Edge "(->)" [])]),(Node [(mkEdge "app" [(Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))]),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])]),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "(->)" [])]),(Node [(Edge "g" [(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "baseType" [])]),(Node [(Edge "baseType" [])])])])]),(Edge "x" [(Node [(Edge "baseType" [])])]),(Edge "n" [(Node [(Edge "Int" [])])]),(Edge "$" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,1]],PathEClass [Path [1,2],Path [2,2]]]})])]),(Edge "replicate" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "Int" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [2,1],Path [2,2,0]]]})])]),(Edge "foldr" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,2,1,0]],PathEClass [Path [1,2,1],Path [1,2,2],Path [2,1],Path [2,2,2]]]})])])]),(Node [(Edge "g" [(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "baseType" [])]),(Node [(Edge "baseType" [])])])])]),(Edge "x" [(Node [(Edge "baseType" [])])]),(Edge "n" [(Node [(Edge "Int" [])])]),(Edge "$" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,1]],PathEClass [Path [1,2],Path [2,2]]]})])]),(Edge "replicate" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "Int" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [2,1],Path [2,2,0]]]})])]),(Edge "foldr" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,2,1,0]],PathEClass [Path [1,2,1],Path [1,2,2],Path [2,1],Path [2,2,2]]]})])])])] EqConstraints {getEclasses = [PathEClass [Path [0],Path [2,0,2]],PathEClass [Path [1],Path [2,0,0]],PathEClass [Path [2,0,1],Path [3,0]]]})]),(Node [(Edge "g" [(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "baseType" [])]),(Node [(Edge "baseType" [])])])])]),(Edge "x" [(Node [(Edge "baseType" [])])]),(Edge "n" [(Node [(Edge "Int" [])])]),(Edge "$" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,1]],PathEClass [Path [1,2],Path [2,2]]]})])]),(Edge "replicate" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "Int" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [2,1],Path [2,2,0]]]})])]),(Edge "foldr" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,2,1,0]],PathEClass [Path [1,2,1],Path [1,2,2],Path [2,1],Path [2,2,2]]]})])])])] EqConstraints {getEclasses = [PathEClass [Path [0],Path [2,0,2]],PathEClass [Path [1],Path [2,0,0]],PathEClass [Path [2,0,1],Path [3,0]]]}),(mkEdge "app" [(Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))]),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])]),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "(->)" [])]),(Node [(Edge "g" [(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "baseType" [])]),(Node [(Edge "baseType" [])])])])]),(Edge "x" [(Node [(Edge "baseType" [])])]),(Edge "n" [(Node [(Edge "Int" [])])]),(Edge "$" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,1]],PathEClass [Path [1,2],Path [2,2]]]})])]),(Edge "replicate" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "Int" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [2,1],Path [2,2,0]]]})])]),(Edge "foldr" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,2,1,0]],PathEClass [Path [1,2,1],Path [1,2,2],Path [2,1],Path [2,2,2]]]})])])]),(Node [(mkEdge "app" [(Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))]),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])]),(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "(->)" [])]),(Node [(Edge "g" [(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "baseType" [])]),(Node [(Edge "baseType" [])])])])]),(Edge "x" [(Node [(Edge "baseType" [])])]),(Edge "n" [(Node [(Edge "Int" [])])]),(Edge "$" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,1]],PathEClass [Path [1,2],Path [2,2]]]})])]),(Edge "replicate" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "Int" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [2,1],Path [2,2,0]]]})])]),(Edge "foldr" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,2,1,0]],PathEClass [Path [1,2,1],Path [1,2,2],Path [2,1],Path [2,2,2]]]})])])]),(Node [(Edge "g" [(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "baseType" [])]),(Node [(Edge "baseType" [])])])])]),(Edge "x" [(Node [(Edge "baseType" [])])]),(Edge "n" [(Node [(Edge "Int" [])])]),(Edge "$" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,1]],PathEClass [Path [1,2],Path [2,2]]]})])]),(Edge "replicate" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "Int" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [2,1],Path [2,2,0]]]})])]),(Edge "foldr" [(Node [(mkEdge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])]),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])])),(Node [(Edge "->" [(Node [(Edge "(->)" [])]),(Node [(Edge "List" [(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])]),(createMu $ \x -> (Node [(Edge "baseType" []),(Edge "->" [(Node [(Edge "(->)" [])]),x,x]),(Edge "Maybe" [x]),(Edge "List" [x])]))])])])])] EqConstraints {getEclasses = [PathEClass [Path [1,1],Path [2,2,1,0]],PathEClass [Path [1,2,1],Path [1,2,2],Path [2,1],Path [2,2,2]]]})])])])] EqConstraints {getEclasses = [PathEClass [Path [0],Path [2,0,2]],PathEClass [Path [1],Path [2,0,0]],PathEClass [Path [2,0,1],Path [3,0]]]})])] EqConstraints {getEclasses = [PathEClass [Path [0],Path [2,0,2]],PathEClass [Path [1],Path [2,0,0]],PathEClass [Path [2,0,1],Path [3,0]]]})])]
  )

_bug062721NonIdempotentEqConstraintReductionGen :: Gen [Node]
_bug062721NonIdempotentEqConstraintReductionGen = return $ snd bug062721NonIdempotentEqConstraintReduction

infiniteFNode :: Node
infiniteFNode = createMu (\x ->(Node [Edge "f" [x]]))

_infiniteFGNode :: Node
_infiniteFGNode = createMu (\x ->(Node [Edge "f" [x], Edge "g" [x]]))

--------------------------------------------------------------
----------------------------- Main ---------------------------
--------------------------------------------------------------


spec :: Spec
spec = do
  describe "Pathable" $ do
    it "Node.getPath root" $
      getPath (path []) testBigNode `shouldBe` testBigNode

    it "Node.getPath one-level" $
      getPath (path [0]) ex1 `shouldBe` (constTerms ["1", "2"])

    it "Node.getPath merges multiple branches" $
      getPath (path [0,0]) ex3 `shouldBe` (constTerms ["1", "2", "3", "4"])

    it "Node.modifyAtPath modifies at root" $
      modifyAtPath doubleNodeSymbols (path []) ex3 `shouldBe` ex3_root_doubled

    it "Node.modifyAtPath modifies at path" $
      modifyAtPath doubleNodeSymbols (path [0,0]) ex3 `shouldBe` ex3_doubled

  describe "hash-consing" $ do
    it "similar mu-nodes created independently are equal / have equal ids" $
      createMu (\x -> Node [Edge "f" [x]]) `shouldBe` createMu (\x -> Node [Edge "f" [x]])

  describe "ECTA-nodes" $ do
    it "equality constraints constrain" $
        naiveDenotation ex1 `shouldSatisfy` ((== 2) . length)

    it "reduces paths constrained by equality constraints" $
        reducePartially ex2 `shouldBe` reducePartially ex1

  describe "intersection" $ do
    it "intersection commutes with naiveDenotation" $
      property $ mapSize (min 3) $ \n1 n2 -> HashSet.fromList (naiveDenotation $ intersect n1 n2)
                                               `shouldBe` HashSet.intersection (HashSet.fromList $ naiveDenotation n1)
                                                                               (HashSet.fromList $ naiveDenotation n2)

    it "intersect is associative" $
      property $ \n1 n2 n3 -> ((n1 `intersect` n2) `intersect` n3) == (n1 `intersect` (n2 `intersect` n3))

    it "intersect is commutative" $
      property $ \n1 n2 -> intersect n1 n2 == intersect n2 n1

    it "intersect distributes over union" $
      property $ \n1 n2 n3 -> intersect n1 (union [n2, n3]) == union [intersect n1 n2, intersect n1 n3]

    it "intersect is idempotent" $
      property $ \n1 -> intersect n1 n1 == n1

  describe "reduction" $ do
    it "reduction preserves naiveDenotation" $
      property $ mapSize (min 3) $ \n -> HashSet.fromList (naiveDenotation n) `shouldBe` HashSet.fromList (naiveDenotation $ reducePartially n)

    it "reducing a single constraint is idempotent 1" $
      property $ \e -> let ns  = edgeChildren e
                           ecs = edgeEcs e
                           ns' = reduceEqConstraints ecs ns
                       in  ns' == reduceEqConstraints ecs ns'

    it "reducing a single constraint is idempotent 2" $
      property $ \e1 e2 -> let maybeE'  = intersectEdge e1 e2
                           in (maybeE' /= Nothing) ==>
                                 let Just e' = maybeE'
                                     ns  = edgeChildren e'
                                     ecs = edgeEcs e'
                                     ns' = reduceEqConstraints ecs ns
                                 in  ns' == reduceEqConstraints ecs ns'

    --   TODO (6/29/21): Need a better way to visualize the type nodes. Cannot figure out why this fails.
    --   Reversing the order that eclasses are processed seems to make no difference.
    {-
    it "reducing a constraint is idempotent: buggy input 6/27/21" $
      forAllShrink bug062721NonIdempotentEqConstraintReductionGen shrink
                                                                   (\ns -> let ecs = fst bug062721NonIdempotentEqConstraintReduction
                                                                               ns' = reduceEqConstraints ecs ns
                                                                           in ns' == reduceEqConstraints ecs ns')
     -}

    -- TODO: I've become less convinced this can actually be done in one pass. But this test passes.
    it "leaf reduction means, for everything at a path, there is something matching at the other paths" $
      property $ \e -> let e' = reduceEdgeIntersection e
                           ns = edgeChildren e' in
                      (e' /= emptyEdge && edgeEcs e' /= EmptyConstraints) ==>
                         and [intersect n1 n2 /= EmptyNode | ec <- unsafeGetEclasses (edgeEcs e')
                                                           , p1 <- unPathEClass ec
                                                           , p2 <- unPathEClass ec
                                                           , n1 <- getAllAtPath p1 ns
                                                           , let n2 = getPath p2 ns]

  describe "(un)folding" $ do
    it "unfolding a mu node once unfolds it once" $
      unfoldOuterRec infiniteFNode `shouldBe` (Node [Edge "f" [infiniteFNode]])

    it "recursive terms are unrolled to the depth of the constraints and no more" $
      let ecs  = (mkEqConstraints [[path [0,0,0,0], path [1,0,0]]])
          ns   = [infiniteFNode, infiniteFNode]
          ns'  = reduceEqConstraints ecs ns
          ns'' = reduceEqConstraints ecs ns'
          f    = \n -> Node [Edge "f" [n]]
      in (ns' == ns'') && ns' == [f $ f $ f $ f infiniteFNode, f $ f $ f $ infiniteFNode]
         `shouldBe` True

    it "refold folds the simplest unrolled input" $
      refold (Node [Edge "f" [infiniteFNode]]) `shouldBe` infiniteFNode

  describe "traversals" $ do
    it "mapNodes hits each node exactly once" $
      -- Note: If the Arbitrary Node instance is changed to return empty or mu nodes, this will need to change
      property $ \n -> unsafePerformIO $ do v <- newIORef 0
                                            let n' = mapNodes (\m -> unsafePerformIO (modifyIORef v (+1) >> pure m)) n
                                            let k = nodeCount n'
                                            numInvocations <- k `seq` readIORef v
                                            return $ k == numInvocations

    it "nodeCount works on a trivial recursive node" $
      nodeCount infiniteFNode `shouldBe` 1

  describe "enumeration" $ do
    it "naive and sophisticated enumeration are equivalent on nodes without mu" $
      property $ mapSize (min 3) $
        \n -> HashSet.fromList (naiveDenotation n) `shouldBe` HashSet.fromList (getAllTerms $ reducePartially n)

  describe "counted nested Mu" $ do
    it "no Mu" $
      numNestedMu (Node [Edge "a" []]) `shouldBe` 0
    it "single Mu" $
      numNestedMu (Mu $ \x -> Node [Edge "f" [x]]) `shouldBe` 1
    it "two parallel Mus" $
      numNestedMu (Node [Edge "h" [Mu $ \x -> Node [Edge "g" [x]], Mu $ \x -> Node [Edge "h" [x]]]]) `shouldBe` 1
    it "nested" $
      numNestedMu (Mu $ \x -> Node [Edge "f" [x], Edge "g" [Mu $ \y -> Node [Edge "g" [y]]]]) `shouldBe` 2

  describe "nested Mu" $
    it "references to different Mu nodes are not confused" $
     property $ do
       -- Two nodes with very similar structure
       -- We are precise about evaluation order here: what we are testing is that after the first term have been
       -- interned, we do /NOT/ reuse that term when interning the second. (If we /did/ confuse different references
       -- to 'Mu' nodes, @m@ looks precisely like the inner @Mu@ node of @n@.)
       n <- evaluate $ Mu $ \r1 -> Mu $ \r2 -> Node [Edge "f" [r1], Edge "g" [r2], Edge "a" []]
       m <- evaluate $ Mu $ \r              -> Node [Edge "f" [r ], Edge "g" [r ], Edge "a" []]

       -- This is a low-level test; crush doesn't work, because we don't see what 'InternedMu' caches.
       let collectAllIds :: Node -> Set Int
           collectAllIds EmptyNode           = Set.empty
           collectAllIds (InternedNode node) = Set.unions [
                                                   Set.singleton (internedNodeId node)
                                                 , Set.unions $ concatMap (map collectAllIds . edgeChildren) (internedNodeEdges node)
                                                 ]
           collectAllIds (InternedMu   mu)   = Set.unions [
                                                   Set.singleton (internedMuId mu)
                                                 , Set.union (collectAllIds (internedMuBody mu)) (collectAllIds (internedMuShape mu))
                                                 ]
           collectAllIds (Rec _)             = Set.empty

       Set.intersection (collectAllIds n) (collectAllIds m) `shouldBe` Set.empty

