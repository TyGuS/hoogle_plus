-- | Lightweight union-find implementation suitable for use with nondeterminism

-- Mutable union-find, as in Data.Equivalence.Monad, should be faster overall,
-- but this persistent implementation is suitable for use in nondeterministic search
-- (e.g.: in the list monad)

module Data.Persistent.UnionFind (
    UVarGen
  , initUVarGen
  , nextUVar

  , UVar
  , uvarToInt
  , intToUVar

  , UnionFind
  , empty
  , withInitialValues
  , union
  , find
  ) where

import Control.Monad.State.Strict ( State, runState, execState, get, put, modify' )
import Data.Coerce ( coerce )
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IntMap


----------------------------------------------------------

---------------------------
-------- UVarGen
---------------------------

newtype UVarGen = UVarGen Int
  deriving ( Eq, Ord, Show )

initUVarGen :: UVarGen
initUVarGen = UVarGen 0

nextUVar :: UVarGen -> (UVarGen, UVar)
nextUVar (UVarGen n) = (UVarGen (n+1), UVar n)


---------------------------
-------- UVar
---------------------------

newtype UVar = UVar Int
  deriving ( Eq, Ord, Show )

uvarToInt :: UVar -> Int
uvarToInt (UVar i) = i

intToUVar :: Int -> UVar
intToUVar = UVar

---------------------------
-------- Union-find data structure
---------------------------

newtype UnionFind = UnionFind { getUnionFindMap :: IntMap Int }
  deriving ( Eq, Ord, Show )

empty :: UnionFind
empty = UnionFind IntMap.empty

withInitialValues :: [UVar] -> UnionFind
withInitialValues uvs = UnionFind $ IntMap.fromList $ map (,-1) $ coerce uvs

---------------------------
-------- Union-find operations
---------------------------

union :: UVar -> UVar -> UnionFind -> UnionFind
union uv1 uv2 uf
   | otherwise   = flip execState uf $ do
                     (uv1Rep, negativeUv1Size) <- findWithNegSize uv1
                     (uv2Rep, negativeUv2Size) <- findWithNegSize uv2
                     if uv1Rep == uv2Rep then
                       return ()
                      else if negativeUv1Size > negativeUv2Size then
                       do modify' (coerce (IntMap.insert @Int) uv1Rep uv2Rep)
                          modify' (coerce (IntMap.insert @Int) uv2Rep (negativeUv1Size + negativeUv2Size))
                      else
                       do modify' (coerce (IntMap.insert @Int) uv2Rep uv1Rep)
                          modify' (coerce (IntMap.insert @Int) uv1Rep (negativeUv1Size + negativeUv2Size))

findWithNegSize :: UVar -> State UnionFind (UVar, Int)
findWithNegSize uv = do
  m <- get
  case coerce (IntMap.lookup @Int) uv m of
    Nothing -> put (coerce (IntMap.insert @Int) uv (-1 :: Int) m) >> return (uv, -1)
    Just x
       | x < 0     -> return (uv, x)
       | otherwise -> do (rep,size) <- findWithNegSize (UVar x)
                         put (coerce (IntMap.insert @Int) uv rep m)
                         return (rep, size)


find :: UVar -> UnionFind -> (UVar, UnionFind)
find uv uf = coerce runState (fst <$> findWithNegSize uv) uf
