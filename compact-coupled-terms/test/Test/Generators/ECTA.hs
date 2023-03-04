{-# Language OverloadedStrings #-}

module Test.Generators.ECTA () where

import Prelude hiding ( max )

import Control.Monad ( replicateM )
import Data.List ( subsequences, (\\) )

import Test.QuickCheck

import Data.ECTA
import Data.ECTA.Internal.ECTA.Type
import Data.ECTA.Paths
import Data.ECTA.Term

-----------------------------------------------------------------------------------------------


-- Cap size at 3 whenever you will generate all denotations
_MAX_NODE_DEPTH :: Int
_MAX_NODE_DEPTH = 5

capSize :: Int -> Gen a -> Gen a
capSize max g = sized $ \n -> if n > max then
                                resize max g
                              else
                                g

instance Arbitrary Node where
  arbitrary = capSize _MAX_NODE_DEPTH $ sized $ \_n -> do
    k <- chooseInt (1, 3) -- TODO: Should this depend on n?
    Node <$> replicateM k arbitrary

  shrink EmptyNode = []
  shrink (Node es) = [Node es' | s <- subsequences es \\ [es], es' <- mapM shrink s] ++ concatMap (\e -> edgeChildren e) es
  shrink (Mu _)    = []
  shrink (Rec _)   = []


testEdgeTypes :: [(Symbol, Int)]
testEdgeTypes = [ ("f", 1)
                , ("g", 2)
                , ("h", 1)
                , ("w", 3)
                , ("a", 0)
                , ("b", 0)
                , ("c", 0)
                ]

testConstants :: [Symbol]
testConstants = map fst $ filter ((== 0) . snd) testEdgeTypes

randPathPair :: [Node] -> Gen [Path]
randPathPair ns = do p1 <- randPath ns
                     p2 <- randPath ns
                     return [p1, p2]

randPath :: [Node] -> Gen Path
randPath [] = return EmptyPath
randPath ns = do i <- chooseInt (0, length ns - 1)
                 let Node es = ns !! i
                 ns' <- edgeChildren <$> elements es
                 b <- arbitrary
                 if b then return (path [i]) else ConsPath i <$> randPath ns'

instance Arbitrary Edge where
  arbitrary =
    sized $ \n -> case n of
                   0 -> Edge <$> elements testConstants <*> pure []
                   _ -> do (sym, arity) <- elements testEdgeTypes
                           ns <- replicateM arity (resize (n-1) (arbitrary `suchThat` (/= EmptyNode)))
                           numConstraintPairs <- elements [0,0,1,1,2,3]
                           ps <- replicateM numConstraintPairs (randPathPair ns)
                           return $ mkEdge sym ns (mkEqConstraints ps)

  shrink e = mkEdge (edgeSymbol e) <$> (mapM shrink (edgeChildren e)) <*> pure (edgeEcs e)

