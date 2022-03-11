module Utility.Container
  (
    -- * List
    permuteBy

    -- * Set
  , (>.>)

    -- * Text
  ) where


import           Data.List.Extra ( sortOn )
import           Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Set as Set

--------------------------------------------------------------------------------
--------------------------- List Operations ------------------------------------
--------------------------------------------------------------------------------

permuteBy :: [Int] -> [a] -> [a]
permuteBy ord xs = map snd $ sortOn fst $ zip ord xs

--------------------------------------------------------------------------------
---------------------------  Set Operations ------------------------------------
--------------------------------------------------------------------------------

(>.>) :: Ord a => [a] -> [a] -> [a]
xs >.> ys = let ys' = Set.fromList ys in filter (flip Set.notMember ys') xs


--------------------------------------------------------------------------------
--------------------------- Text Operations ------------------------------------
--------------------------------------------------------------------------------

textElem :: Char -> Text -> Bool
textElem c = Text.any (== c)