module Database.Util where

import qualified Data.Set as Set

type Version = String
type PkgName = String

downloadDir = "/tmp/"

(>.<) :: Ord a => [a] -> [a] -> [a]
(>.<) xs ys = Set.toList $ Set.fromList xs `Set.intersection` Set.fromList ys