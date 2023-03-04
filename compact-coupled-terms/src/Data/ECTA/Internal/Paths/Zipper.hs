-- | These were used in an earlier version of the enumeration algorithm, but no longer.
--
--   They are being kept around just in case.


module Data.ECTA.Internal.Paths.Zipper (
    unionPathTrie

  , InvertedPathTrie(..)

  , PathTrieZipper(..)
  , emptyPathTrieZipper
  , pathTrieToZipper
  , zipperCurPathTrie
  , pathTrieZipperDescend
  , pathTrieZipperAscend
  , unionPathTrieZipper
  ) where

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector ( unsafeWrite )

import GHC.Exts ( inline )

import Data.ECTA.Internal.Paths

-----------------------------------------------------------------------

---------------------
------- Path trie union
------- (7/9/21: only used as utility for unionPathTrieZipper)
---------------------

unionPathTrie :: PathTrie -> PathTrie -> Maybe PathTrie
unionPathTrie EmptyPathTrie                pt                           = Just pt
unionPathTrie pt                           EmptyPathTrie                = Just pt
unionPathTrie TerminalPathTrie             TerminalPathTrie             = Just TerminalPathTrie
unionPathTrie TerminalPathTrie             _                            = Nothing
unionPathTrie _                            TerminalPathTrie             = Nothing
unionPathTrie (PathTrieSingleChild i1 pt1) (PathTrieSingleChild i2 pt2) =
    if i1 == i2 then
      PathTrieSingleChild i1 <$> unionPathTrie pt1 pt2
    else
      Just $ PathTrie $ Vector.generate (1 + max i1 i2) $ \j -> if j == i1 then
                                                                  pt1
                                                                else if j == i2 then
                                                                  pt2
                                                                else
                                                                  EmptyPathTrie
unionPathTrie (PathTrieSingleChild i pt)   (PathTrie vec) =
  if Vector.length vec > i then
    do updated <- unionPathTrie pt (vec `Vector.unsafeIndex` i)
       Just $ PathTrie $ Vector.modify (\v -> Vector.unsafeWrite v i updated) vec
  else
    Just $ PathTrie $ Vector.generate (i+1) $ \j -> if j < Vector.length vec then
                                                      vec `Vector.unsafeIndex` j
                                                    else if j == i then
                                                      pt
                                                    else
                                                      EmptyPathTrie


unionPathTrie pt1@(PathTrie _)             pt2@(PathTrieSingleChild _ _) = inline unionPathTrie pt2 pt1 -- TODO: Check whether this inlining is effective
unionPathTrie (PathTrie vec1)              (PathTrie vec2)               =
  let newLength = max (Vector.length vec1) (Vector.length vec2)
      smallerLength = min (Vector.length vec1) (Vector.length vec2)
      bigVec   = if Vector.length vec1 > Vector.length vec2 then vec1 else vec2
      smallVec = if Vector.length vec1 > Vector.length vec2 then vec2 else vec1
  in fmap PathTrie $ Vector.generateM newLength $ \i -> if i >= smallerLength then
                                                          return (bigVec `Vector.unsafeIndex` i)
                                                        else
                                                          unionPathTrie (bigVec `Vector.unsafeIndex` i) (smallVec `Vector.unsafeIndex` i)



---------------------
------- Zippers
---------------------

data InvertedPathTrie = PathZipperRoot
                      | PathTrieAt {-# UNPACK #-} !Int !PathTrie !InvertedPathTrie
  deriving ( Eq, Ord, Show )

data PathTrieZipper = PathTrieZipper !PathTrie !InvertedPathTrie
  deriving ( Eq, Ord, Show )

emptyPathTrieZipper :: PathTrieZipper
emptyPathTrieZipper = PathTrieZipper EmptyPathTrie PathZipperRoot

pathTrieToZipper :: PathTrie -> PathTrieZipper
pathTrieToZipper pt = PathTrieZipper pt PathZipperRoot

zipperCurPathTrie :: PathTrieZipper -> PathTrie
zipperCurPathTrie (PathTrieZipper pt _) = pt

unionInvertedPathTrie :: InvertedPathTrie -> InvertedPathTrie -> Maybe InvertedPathTrie
unionInvertedPathTrie PathZipperRoot           ipt                      = Just ipt
unionInvertedPathTrie ipt                      PathZipperRoot           = Just ipt
unionInvertedPathTrie (PathTrieAt i1 pt1 ipt1) (PathTrieAt i2 pt2 ipt2) =
  if i1 /= i2 then
    Nothing
  else
    PathTrieAt i1 <$> unionPathTrie pt1 pt2 <*> unionInvertedPathTrie ipt1 ipt2


unionPathTrieZipper :: PathTrieZipper -> PathTrieZipper -> Maybe PathTrieZipper
unionPathTrieZipper (PathTrieZipper pt1 ipt1) (PathTrieZipper pt2 ipt2) =
  PathTrieZipper <$> unionPathTrie pt1 pt2 <*> unionInvertedPathTrie ipt1 ipt2

pathTrieZipperDescend :: PathTrieZipper -> Int -> PathTrieZipper
pathTrieZipperDescend (PathTrieZipper pt z) i = PathTrieZipper (pathTrieDescend pt i) (PathTrieAt i pt z)

-- | The semantics of this may not be what you expect: Path trie zippers do not support editing currently, only traversing.
--   The value at the cursor (as well as the index) is ignored except when traversing above the root, where it uses those
--   values to extend the path trie upwards.
pathTrieZipperAscend :: PathTrieZipper -> Int -> PathTrieZipper
pathTrieZipperAscend (PathTrieZipper pt PathZipperRoot)         i = PathTrieZipper (PathTrieSingleChild i pt) PathZipperRoot
pathTrieZipperAscend (PathTrieZipper _  (PathTrieAt _ pt' ipt)) _ = PathTrieZipper pt'                        ipt
