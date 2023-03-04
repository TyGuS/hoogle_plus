module Utility.HashJoin (
    nubById
  , nubByIdSinglePass
  , hashClusterIdNub
  , clusterByHash
  , hashJoin
  ) where

import Control.Monad ( forM_, void )
import Control.Monad.ST ( ST, runST )
import Data.Foldable ( foldrM )

import qualified Data.HashTable.ST.Basic as HT

-------------------------------------
--- Hash join / clustering / nub
-------------------------------------


-- | PRECONDITION: (h x == h y) => x == y
nubById :: (a -> Int) -> [a] -> [a]
nubById _ [x] = [x]
nubById h ls = runST $ do
    ht <- HT.newSized 101
    mapM_ (\x -> HT.insert ht (h x) x) ls
    HT.foldM (\res (_, v) -> return $ v : res) [] ht

nubByIdSinglePass :: forall a. (a -> Int) -> [a] -> [a]
nubByIdSinglePass _ [x] = [x]
nubByIdSinglePass h ls = runST (go ls [] =<< HT.new)
  where
    go :: [a] -> [a] -> HT.HashTable s Int Bool -> ST s [a]
    go []     acc    _  = return acc
    go (x:xs) acc ht = do alreadyPresent <- HT.mutate ht
                                                      (h x)
                                                      (\case Nothing -> (Just True, False)
                                                             Just _  -> (Just True, True))
                          if alreadyPresent then
                            go xs acc ht
                          else
                            go xs (x:acc) ht


maybeAddToHt :: v -> Maybe [v] -> (Maybe [v], ())
maybeAddToHt v = \case Nothing -> (Just [v], ())
                       Just vs -> (Just (v : vs), ())

-- This is testing slower than running clusterByHash and nubByIdSinglePass separately. How?
hashClusterIdNub :: (a -> Int) -> (a -> Int) -> [a] -> [[a]]
hashClusterIdNub _ _ [x] = [[x]]
hashClusterIdNub hCluster hNub ls = runST $ do
    clusters <- HT.new
    seen <- HT.new

    forM_ ls $ \x -> do
      alreadyPresent <- HT.mutate seen
                                  (hNub x)
                                  (\case Nothing -> (Just True, False)
                                         Just _  -> (Just True, True))
      if alreadyPresent then
        return ()
       else do
        void $ HT.mutate clusters (hCluster x) (maybeAddToHt x)

    HT.foldM (\res (_, vs) -> return $ vs : res) [] clusters

clusterByHash :: (a -> Int) -> [a] -> [[a]]
clusterByHash h ls = runST $ do
    ht <- HT.new
    mapM_ (\x -> HT.mutate ht (h x) (maybeAddToHt x)) ls
    HT.foldM (\res (_, vs) -> return $ vs : res) [] ht

hashJoin :: (a -> Int) -> (a -> a -> b) -> [a] -> [a] -> [b]
hashJoin h j l1 l2 = runST $ do
    ht2 <- HT.new
    mapM_ (\x -> HT.mutate ht2 (h x) (maybeAddToHt x)) l2
    foldrM (\x res -> do maybeCluster <- HT.lookup ht2 (h x)
                         case maybeCluster of
                           Nothing  -> return res
                           Just vs2 -> return $ [j x v2 | v2 <- vs2] ++ res )
           []
           l1
