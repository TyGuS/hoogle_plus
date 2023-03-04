module Data.HashTable.Extended (
    getKeys
  , resetHashTable
  , AnyHashTable(..)
  ) where


import Data.Hashable ( Hashable )
import Data.HashTable.Class ( HashTable )
import qualified Data.HashTable.IO as HT


------------------------------------------------------------------------------

getKeys :: (HashTable h) => HT.IOHashTable h k v -> IO [k]
getKeys ht = HT.foldM f [] ht
  where f !l !(k, _) = return (k : l)

resetHashTable :: AnyHashTable -> IO ()
resetHashTable (AnyHashTable ht) = do
  keys <- getKeys ht
  mapM_ (\k -> HT.mutate ht k (const (Nothing, ()))) keys


data AnyHashTable where
  AnyHashTable :: (HashTable h, Eq k, Hashable k) => HT.IOHashTable h k v -> AnyHashTable
