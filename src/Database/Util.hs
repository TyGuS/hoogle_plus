module Database.Util where

import qualified Data.Set as Set

type Version = String
type PkgName = String

downloadDir = "/tmp/hoogle_plus/"

(>.<) :: Ord a => [a] -> [a] -> [a]
xs >.< ys = let ys' = Set.fromList ys in filter (flip Set.member ys') xs

(>.>) :: Ord a => [a] -> [a] -> [a]
xs >.> ys = let ys' = Set.fromList ys in filter (flip Set.notMember ys') xs
