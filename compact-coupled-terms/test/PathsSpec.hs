module PathsSpec ( spec ) where

import Data.List ( (\\), nub, sort, subsequences )
import qualified Data.Vector as Vector

import Test.Hspec
import Test.QuickCheck

import Data.ECTA.Internal.Paths
import Data.ECTA.Internal.Paths.Zipper

-----------------------------------------------------------------

-----------------------------------
------ PathTrie testing utils
-----------------------------------

data PathTrieCommand = PathTrieZipperAscend  Int
                     | PathTrieZipperDescend Int
  deriving ( Show )

instance Arbitrary PathTrieCommand where
  arbitrary = do b <- arbitrary
                 i <- chooseInt (0, 4)
                 return $ if b then PathTrieZipperAscend i else PathTrieZipperDescend i

  shrink _ = []


invertPathTrieCommand :: PathTrieCommand -> PathTrieCommand
invertPathTrieCommand (PathTrieZipperAscend i)  = PathTrieZipperDescend i
invertPathTrieCommand (PathTrieZipperDescend i) = PathTrieZipperAscend  i

-- | A variant of pathTrieZipperDescend that allows for descending out of bounds.
--   Makes the "descend/ascend are inverses" property easy to write
extendedPathTrieZipperDescend :: PathTrieZipper -> Int -> PathTrieZipper
extendedPathTrieZipperDescend (PathTrieZipper (PathTrie v) z') i
                                      | i >= Vector.length v     = PathTrieZipper EmptyPathTrie (PathTrieAt i (PathTrie v) z')
extendedPathTrieZipperDescend z                                i = pathTrieZipperDescend z i

applyPathTrieCommand :: PathTrieCommand -> PathTrieZipper -> PathTrieZipper
applyPathTrieCommand (PathTrieZipperAscend  i) z = pathTrieZipperAscend z i
applyPathTrieCommand (PathTrieZipperDescend i) z = extendedPathTrieZipperDescend z i



-----------------------------------
------ Random generation
-----------------------------------

instance Arbitrary Path where
  arbitrary = path <$> listOf (chooseInt (0, 4))
  shrink = map Path . shrink . unPath


instance Arbitrary PathTrie where
  arbitrary = do paths <- suchThat arbitrary (\ps -> not (isContradicting [ps]))
                 return $ toPathTrie $ nub paths

  shrink EmptyPathTrie              = []
  shrink TerminalPathTrie           = []
  shrink (PathTrieSingleChild _ pt) = [pt]
  shrink (PathTrie vec)             = let l = Vector.toList vec
                                      in l ++ (map (PathTrie . Vector.fromList) (subsequences l \\ [l]))


-----------------------------------
------ Constructing test inputs
-----------------------------------

mkTestPaths1 :: [[Int]] -> [[Path]]
mkTestPaths1 = map (map (path . (:[])))

mkTestPathsN :: [[[Int]]] -> [[Path]]
mkTestPathsN = map (map path)

--------

spec :: Spec
spec = do
  describe "subpath checking" $ do
    it "empty path is always subpath" $
      property $ \p -> isSubpath EmptyPath p

    it "is subpath of concatenation" $
      property $ \xs ys -> isSubpath (path xs) (path $ xs ++ ys)

    it "non-empty concatenation is not subpath of orig" $
      property $ \xs ys -> ys /= [] ==> not $ isSubpath (path $ xs ++ ys) (path xs)

    it "empty path is strict subpath of nonempty" $
      property $ \p -> p /= EmptyPath ==> isStrictSubpath EmptyPath p

    it "nothing is strict subpath of itself" $
      property $ \p -> not $ isStrictSubpath p p

  describe "substSubpath" $ do
    it "replaces prefix" $
      property $ \xs ys zs -> substSubpath (path zs) (path ys) (path $ ys ++ xs) `shouldBe` path (zs ++ xs)

  describe "path tries" $ do
    it "fromPathTrie and toPathTrie are inverses" $ do
      property $ \pt -> toPathTrie (fromPathTrie pt) == pt

    it "comparing path trie is same as comparing list of paths" $ do
      property $ \ps1 ps2 -> not (isContradicting [ps1] || isContradicting [ps2])
                             ==> compare (toPathTrie $ nub ps1) (toPathTrie $ nub ps2)
                                   == compare (sort $ nub ps1) (sort $ nub ps2)

    it "unioning path trie same as unioning lists of paths, checking contradiction" $ do
      property $ \pt1 pt2 -> case unionPathTrie pt1 pt2 of
                               Nothing  -> isContradicting [fromPathTrie pt1 ++ fromPathTrie pt2]
                               Just pt' -> fromPathTrie pt' == (sort $ nub $ fromPathTrie pt1 ++ fromPathTrie pt2)

    it "PathTrie-based hasSubsumingMember same as list-based implementation" $ do
      property $ \pt1 pt2 -> let pec1 = PathEClass (fromPathTrie pt1)
                                 pec2 = PathEClass (fromPathTrie pt2)
                             in hasSubsumingMember pec1 pec2 == hasSubsumingMemberListBased (unPathEClass pec1) (unPathEClass pec2)



  describe "path trie zipper" $ do
    it "smallestNonempty works" $ do
      smallestNonempty (Vector.fromList [EmptyPathTrie, EmptyPathTrie, TerminalPathTrie, TerminalPathTrie, EmptyPathTrie]) `shouldBe` 2

    it "largestNonempty works" $ do
      largestNonempty  (Vector.fromList [EmptyPathTrie, EmptyPathTrie, TerminalPathTrie, TerminalPathTrie, EmptyPathTrie]) `shouldBe` 3

    it "ascending a zipper well beyond the root == adding ints to a path" $ do
      forAll (listOf (chooseInt (0, 4))) $ \ns -> fromPathTrie (zipperCurPathTrie $ foldr (flip pathTrieZipperAscend) (pathTrieToZipper $ toPathTrie [EmptyPath]) ns) == [path ns]

    it "a sequence of path trie zipper ascends/descends followed by its reverse yields the identity" $ do
      property $ \actions pt -> (zipperCurPathTrie $ foldr applyPathTrieCommand (pathTrieToZipper pt) (reverse (map invertPathTrieCommand actions) ++ actions))
                                == pt

  describe "PathEClass" $ do
    it "both ways of getting list of paths from a PathEClass are identical" $ do
      property $ \pt -> fromPathTrie (getPathTrie (PathEClass (fromPathTrie pt))) == getOrigPaths (PathEClass (fromPathTrie pt))


  describe "mkEqConstraints" $ do
    it "removes unitary" $
      property $ \ps -> mkEqConstraints (map (:[]) ps) == EmptyConstraints

    it "removes empty" $
      property $ \n -> mkEqConstraints (replicate n []) == EmptyConstraints

    it "completes equalities" $
      mkEqConstraints (mkTestPaths1 [[1,2], [2,3], [4,5], [6,7], [7,1]]) `shouldBe` rawMkEqConstraints (sort $ mkTestPaths1 [[1,2,3,6,7], [4,5]])

    it "adds congruences" $
      mkEqConstraints (mkTestPathsN [[[0],[1]], [[2], [0]], [[0, 0], [0, 1]]]) `shouldBe` rawMkEqConstraints (sort $ (mkTestPathsN [[[0],[1],[2]], [[0, 0], [0, 1], [1, 0], [1,1], [2,0], [2,1]]]))

    it "detects contradictions from congruences" $
      -- This test input is from unifying `(a -> b) -> (a -> b)` and `(a -> (a -> a)) -> (a -> ([a] -> a))`
      constraintsAreContradictory (mkEqConstraints $ mkTestPathsN [ [[1, 1], [2,1]]
                                                                  , [[1, 1], [1, 2, 1], [1,2, 2], [2, 1], [2, 2, 1, 0], [2, 2, 2]]
                                                                  , [[1, 2], [2, 2]]
                                                                  ])
        `shouldBe` True

  -- TODO: (6/23/21) QuickCheck generates very large lists, much larger than currently seen in actual inputs.
  -- mkEqConstraints contains a very inefficient addCongruences implementation. Therefore, these run too slowly.
  {-
  describe "constraintsImply" $ do
    modifyMaxSuccess (const 2) $
      it "Implies removed constraints" $
        property $ \cs1 cs2 -> length (concat cs1) < 300 && length (concat cs2) < 300
                               ==> constraintsImply (mkEqConstraints $ cs1 ++ cs2) (mkEqConstraints cs1)


    modifyMaxSuccess (const 2) $
      it "Does not imply added constraints" $
        property $ \cs1 cs2 -> length (concat cs1) < 300 && length (concat cs2) < 300
                               ==> let ecs1 = mkEqConstraints $ cs1 ++ cs2
                                       ecs2 = mkEqConstraints cs1
                                   in ecs1 /= ecs2 ==> not (constraintsImply ecs2 ecs1)
   -}

