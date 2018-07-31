module Database.Util where

import qualified Data.Set as Set

type Version = String
type PkgName = String

downloadDir = "/tmp/"

-- (>.<) :: Ord a => [a] -> [a] -> [a]
-- xs >.< ys = Set.toList $ Set.fromList xs `Set.intersection` Set.fromList ys

(>.>) :: Ord a => [a] -> [a] -> [a]
xs >.> ys = let ys' = Set.fromList ys in filter (flip Set.notMember ys') xs