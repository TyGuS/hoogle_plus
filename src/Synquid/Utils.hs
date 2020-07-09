-- | Common types and helper functions
module Synquid.Utils where

import Types.Common
import Data.Maybe
import Data.Either
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Char
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.Text.Lazy
import System.Environment

import Control.Applicative
import Control.Monad
import Control.Lens hiding (both)
import Numeric

import Debug.Trace
import Text.Pretty.Simple
import Debug.Pretty.Simple
import Text.Regex (mkRegex, subRegex)

maxCnt = 99999
infinity = 1.0 / 0.0

splitBy :: Char -> String -> [String]
splitBy _ "" = [];
splitBy delimiterChar inputString = foldr f [""] inputString
  where f :: Char -> [String] -> [String]
        f currentChar allStrings@(partialString:handledStrings)
          -- start a new partial string at the head of the list of all strings
          | currentChar == delimiterChar = "":allStrings
          -- add the current char to the partial string
          | otherwise = (currentChar:partialString):handledStrings

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = reverse res
  where
    base = (0, [])
    fold_fn (idx, a) x = if idx == i then (idx+1, a) else (idx+1, x:a)
    (_, res) = foldl fold_fn base xs

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

mapRight :: (a -> b) -> Either c a -> Either c b
mapRight _ (Left x) = Left x
mapRight f (Right x) = Right $ f x

-- | `mappedCompare` @f x y@ : compare @f x@ and @f y@
mappedCompare :: Ord b => (a -> b) -> a -> a -> Ordering
mappedCompare f x y = f x `compare` f y

-- | Map a function on a pair
both :: (a -> b) -> (a, a) -> (b, b)
both f (x1, x2) = (f x1, f x2)

-- | Map a two-argument function on two pairs
both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 f (x1, x2) (y1, y2) = (f x1 y1, f x2 y2)

-- | Map a monadic action on a pair
bothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
bothM f (x1, x2) = do
  y1 <- f x1
  y2 <- f x2
  return (y1, y2)

setCompare :: Ord a => Set a -> Set a -> Ordering
setCompare x y = case compare (Set.size x) (Set.size y) of
                  EQ -> compare x y
                  res -> res

-- | 'disjoint' @s1 s2@ : are @s1@ and @s2@ disjoint?
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint s1 s2 = Set.null $ s1 `Set.intersection` s2

-- | 'restrictDomain' @keys m@ : map @m@ restricted on the set of keys @keys@
restrictDomain :: Ord k => Set k -> Map k a -> Map k a
restrictDomain keys m = fst $ partitionDomain keys m

-- | 'removeDomain' @keys m@ : map @m@ with the set of keys @keys@ removed from its domain
removeDomain :: Ord k => Set k -> Map k a -> Map k a
removeDomain keys m = snd $ partitionDomain keys m

-- | 'partitionDomain' @keys m@ : map @m@ partitioned into two maps, restricted to @keys@ and the rest
partitionDomain :: Ord k => Set k -> Map k a -> (Map k a, Map k a)
partitionDomain keys = Map.partitionWithKey (\k _ -> k `Set.member` keys)

-- | 'constMap' @keys val@ : map that maps each of @keys@ to @val@
constMap :: Ord k => Set k -> a -> Map k a
constMap keys val = Set.fold (\k m -> Map.insert k val m) Map.empty keys

-- | Analogue of 'concatMap' for sets.
setConcatMap :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
setConcatMap f s = Set.foldr Set.union Set.empty (Set.map f s)

-- | 'boundedSubsets' @n s@: all subsets of @s@ of sizes no greater than @n@
boundedSubsets :: Ord k => Int -> Set k -> Set (Set k)
boundedSubsets 0 _ = Set.singleton Set.empty
boundedSubsets n s
  | Set.null s = Set.singleton Set.empty
  | otherwise = let (x, xs) = Set.deleteFindMin s in
      Set.map (Set.insert x) (boundedSubsets (n - 1) xs) `Set.union` boundedSubsets n xs -- x is in or x is out

-- | Partition a set-valued map into sub-maps where value non-disjoint value sets are grouped together
toDisjointGroups :: (Ord k, Ord v) => Map k (Set v) -> [(Set k, Set v)]
toDisjointGroups m = toDisjointGroups' m []
  where
    toDisjointGroups' :: (Ord k, Ord v) => Map k (Set v) -> [(Set k, Set v)] -> [(Set k, Set v)]
    toDisjointGroups' m acc
      | Map.null m  = acc
      | otherwise   = let ((key, vals), m') = Map.deleteFindMin m in
                      let (keys', vals') = close (Set.singleton key) vals m' in
                      let m'' = removeDomain keys' m' in
                      toDisjointGroups' m'' ((keys', vals'):acc)

    close :: (Ord k, Ord v) => Set k -> Set v -> Map k (Set v) -> (Set k, Set v)
    close keys vals m =
      let (mDisj, mNonDisj) = Map.partition (disjoint vals) m in
      if Map.null mNonDisj
        then (keys, vals)
        else close (keys `Set.union` Map.keysSet mNonDisj) (vals `Set.union` (Set.unions $ Map.elems mNonDisj)) mDisj

-- | Monadic version of 'find' (finds the first element in a list for which a computation evaluates to True)
findJustM :: (Functor m, Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findJustM _ [] = return Nothing
findJustM f (x : xs) = do
    resMb <- f x
    case resMb of
        Nothing -> findJustM f xs
        Just res -> return $ Just res

-- | 'pairGetter' @g1 g2@ : combine two getters into one that gets a pair
pairGetter g1 g2 = to (\x -> (view g1 x, view g2 x))

asInteger :: String -> Maybe Integer
asInteger s = if all isDigit s then Just $ read s else Nothing

{- Debug output -}

-- | 'debugOutLevel' : Level above which debug output is ignored
debugOutLevel = 1

-- | 'debug' @level msg@ : output @msg@ at level @level@
debug level msg = if level <= debugOutLevel then traceShow msg else id

-- This should cover the colour codes:
ansiRegex = mkRegex "\\[[0-9]+m"

-- Filtering involves stripping out the ^[ preceding the codes
filterAnsi :: String -> String
filterAnsi line = subRegex ansiRegex stripped ""
  where stripped = filter (/= '\ESC') line

groupTuples :: (Eq a, Ord a) => [(a, [b])] -> [(a, [b])]
groupTuples = map (\l -> (fst . head $ l, concatMap snd l)) . groupBy ((==) `on` fst)
          . sortBy (comparing fst)

showme :: (Show a) => String -> a -> a
showme label thing = let
    newlabel = Data.Text.Lazy.pack (label ++ ": \n")
    in
    pTraceShow (newlabel `Data.Text.Lazy.append` (pShow thing)) thing

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

showFloat :: Double -> String
showFloat x = showFFloat (Just 2) x ""

mkOneLine :: String -> String
mkOneLine = unwords . (map trim) . lines
  where
   trim = dropWhileEnd isSpace . dropWhile isSpace

getTmpDir :: IO String
getTmpDir = (fromMaybe "/tmp" <$> lookupEnv "TMPDIR") >>= (\x -> return $ x ++ "/")

lookupWithError :: (Ord k, Show k) => String -> k -> Map k v -> v
lookupWithError blame k mp = case Map.lookup k mp of
    Just v -> v
    Nothing -> error $ "Failed to find in " ++ blame ++ ": " ++ show k

groupByMap :: (Ord k, Ord v) => Map k v -> Map v [k]
groupByMap mp = foldr (\(k, v) newMap -> Map.insertWith (++) v [k] newMap) Map.empty (Map.toList mp)

-- Assumes that the v are all distinct.
unPartitionMap :: (Ord k, Ord v) => Map k [v] -> Map v k
unPartitionMap mp = foldr update Map.empty (Map.toList mp)
    where
        update (k, vs) m = foldr (`Map.insert` k) m vs

permuteBy :: [Int] -> [a] -> [a]
permuteBy ord xs = map snd $ sortOn fst $ zip ord xs
