module Utility.Utils
  (
    -- * List
    permuteBy
  , multiPermutation

    -- * Set
  , (>.>)
  , shrinkSet
  , firstMatch

    -- * Map
  , lookupWithError
  , groupByMap
  , unPartitionMap

    -- * Text
  , text
  , textElem
  , textParens
  , replaceId
  , removeLast
  , stripSuffix
  , unHTML
  , appendSuffix
  , appendIndex

    -- * Other
  , asInteger
  , showFullPrecision
  , getTmpDir
  , writeLog
  ) where

import           Control.Monad                  ( forM_
                                                , join
                                                , when
                                                )
import           Control.Monad.ST               ( ST
                                                , runST
                                                )
import           Control.Monad.State            ( StateT )
import           Data.Array.ST                  ( STArray
                                                , getElems
                                                , newListArray
                                                , readArray
                                                , writeArray
                                                )
import           Data.Char                      ( isDigit
                                                , isSpace
                                                )
import           Data.Hashable                  ( Hashable
                                                , hash
                                                )
import           Data.List                      ( dropWhileEnd )
import           Data.List.Extra                ( sortOn
                                                , unescapeHTML
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Debug.Trace                    ( trace )
import           Numeric                        ( showFFloat )
import           System.Environment             ( lookupEnv )

import           Text.PrettyPrint.ANSI.Leijen   ( Doc
                                                , plain
                                                )
import qualified Text.PrettyPrint.ANSI.Leijen  as L
import           Text.Printf                    ( printf )

import           Types.Common
import           Types.Log

--------------------------------------------------------------------------------
--------------------------- List Operations ------------------------------------
--------------------------------------------------------------------------------

permuteBy :: [Int] -> [a] -> [a]
permuteBy ord xs = map snd $ sortOn fst $ zip ord xs

-- Thanks, this only helps when you get >100 elements, otherwise, use nubOrd:
-- https://github.com/AndreasPK/nubBench/blob/038bc644f32aaa47035484b4384a4aaf5b78320c/app/Main.hs
nubSpence :: (Hashable a, Eq a) => [a] -> [a]
nubSpence l = runST $ do
  arr <- mr
  forM_ l $ \j -> do
    let index = hash j `mod` 255
    current <- readArray arr index
    let new = if j `elem` current then current else j : current
    writeArray arr index new
  join <$> getElems arr
 where
  mr :: ST s (STArray s Int [a])
  mr = newListArray (0, 255) (replicate 256 [])

multiPermutation :: (Hashable a, Eq a) => Int -> [a] -> [[a]]
multiPermutation len elmts | len == 0 = [[]]
multiPermutation len elmts | len == 1 = [ [e] | e <- elmts ]
multiPermutation len elmts =
  nubSpence [ l : r | l <- elmts, r <- multiPermutation (len - 1) elmts ]

--------------------------------------------------------------------------------
---------------------------  Set Operations ------------------------------------
--------------------------------------------------------------------------------

(>.>) :: Ord a => [a] -> [a] -> [a]
xs >.> ys = let ys' = Set.fromList ys in filter (`Set.notMember` ys') xs

shrinkSet :: Set Id -> Set Id -> Maybe (Set Id)
shrinkSet toRemove ids =
  let ids' = Set.difference ids toRemove
  in  if Set.null ids' then Nothing else Just ids'

firstMatch :: (a -> Bool) -> Set a -> Maybe a
firstMatch p s = Set.lookupMin $ Set.filter p s

--------------------------------------------------------------------------------
---------------------------  Map Operations ------------------------------------
--------------------------------------------------------------------------------

lookupWithError :: (Ord k, Show k) => String -> k -> Map k v -> v
lookupWithError blame k mp = case Map.lookup k mp of
  Just v  -> v
  Nothing -> error $ "Failed to find in " ++ blame ++ ": " ++ show k

groupByMap :: (Ord k, Ord v) => Map k v -> Map v [k]
groupByMap mp = foldr (\(k, v) newMap -> Map.insertWith (++) v [k] newMap)
                      Map.empty
                      (Map.toList mp)

-- Assumes that the v are all distinct.
unPartitionMap :: (Ord k, Ord v) => Map k [v] -> Map v k
unPartitionMap mp = foldr update Map.empty (Map.toList mp)
  where update (k, vs) m = foldr (`Map.insert` k) m vs

--------------------------------------------------------------------------------
--------------------------- Text Operations ------------------------------------
--------------------------------------------------------------------------------

text :: Text -> Doc
text = L.text . Text.unpack

textElem :: Char -> Text -> Bool
textElem c = Text.any (== c)

textParens :: Text -> Text
textParens t = "(" `Text.append` t `Text.append` ")"

replaceId :: String -> String -> String -> String
replaceId a b =
  Text.unpack . Text.replace (Text.pack a) (Text.pack b) . Text.pack

removeLast :: Char -> Id -> Id
removeLast c1 = Text.pack . snd . remLast . Text.unpack
 where
  remLast :: String -> (Bool, String)
  remLast []        = (False, [])
  remLast (c2 : cs) = case remLast cs of
    (True , cs') -> (True, c2 : cs')
    (False, cs') -> if c1 == c2 then (True, []) else (False, c2 : cs')

innerTextHTML :: String -> String
innerTextHTML ('<' : xs) = innerTextHTML $ drop 1 $ dropWhile (/= '>') xs
innerTextHTML (x   : xs) = x : innerTextHTML xs
innerTextHTML []         = []

unHTML :: String -> String
unHTML = unescapeHTML . innerTextHTML

appendSuffix :: Text -> String -> Text
appendSuffix a b = Text.pack $ Text.unpack a ++ b

appendIndex :: Text -> Int -> Text
appendIndex b i = appendSuffix b (show i)

--------------------------------------------------------------------------------
--------------------------- Other Operations -----------------------------------
--------------------------------------------------------------------------------

asInteger :: String -> Maybe Integer
asInteger s = if not (null s) && all isDigit s then Just $ read s else Nothing

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

mkOneLine :: String -> String
mkOneLine = unwords . map trim . lines
  where trim = dropWhileEnd isSpace . dropWhile isSpace

getTmpDir :: IO String
getTmpDir = lookupEnv "TMPDIR" >>= (\x -> return $ x ++ "/") . fromMaybe "/tmp"

stripSuffix :: Id -> Id -> Id
stripSuffix suffix =
  Text.pack . replaceId (Text.unpack suffix) "" . Text.unpack

writeLog :: Loggable m => Int -> String -> Doc -> m ()
writeLog level tag msg = do
  logLevel <- getLogLevel
  when (level <= logLevel)
    $ trace (printf "[%s]: %s\n" tag (show $ plain msg))
    $ return ()
