{-# LANGUAGE CPP #-}

module Data.Interned.Extended.HashTableBased
  ( Id
  , Cache(..)
  , freshCache
  , cacheSize
  , resetCache

#ifdef PROFILE_CACHES
  , getMetrics
#endif

  , Interned(..)
  , intern
  ) where

import Data.Hashable
import qualified Data.HashTable.IO as HT
import Data.IORef
import GHC.IO ( unsafeDupablePerformIO )

import Data.HashTable.Extended

#ifdef PROFILE_CACHES
import Data.Memoization.Metrics ( CacheMetrics(CacheMetrics) )
#endif

----------------------------------------------------------------------------------------------------------

--------------------
------- Caches
--------------------

type Id = Int

-- | Tried using the BasicHashtable size function to remove need for this IORef
-- ( see https://github.com/gregorycollins/hashtables/pull/68 ), but it was slower
data Cache t = Cache { fresh :: !(IORef Id)
                     , content :: !(HT.BasicHashTable (Description t) t)
#ifdef PROFILE_CACHES
                     , queryCount :: !(IORef Int)
                     , missCount  :: !(IORef Int)
#endif
                     }

freshCache :: IO (Cache t)
freshCache = Cache <$> newIORef 0
                   <*> HT.new
#ifdef PROFILE_CACHES
                   <*> newIORef 0
                   <*> newIORef 0
#endif

cacheSize :: Cache t -> IO Int
cacheSize Cache {fresh = refI} = readIORef refI

resetCache :: (Interned t) => Cache t -> IO ()
resetCache _c@(Cache {fresh=refI, content=ht}) = do
  writeIORef refI 0
  resetHashTable (AnyHashTable ht)
#ifdef PROFILE_CACHES
  writeIORef (queryCount _c) 0
  writeIORef (missCount  _c) 0
#endif

bumpQueryCount :: Cache t -> IO ()
#ifdef PROFILE_CACHES
bumpQueryCount Cache {queryCount = ref} = modifyIORef ref (+1)
#else
bumpQueryCount _ = return ()
#endif
{-# INLINE bumpQueryCount #-}

bumpMissCount :: Cache t -> IO ()
#ifdef PROFILE_CACHES
bumpMissCount Cache {missCount = ref} = modifyIORef ref (+1)
#else
bumpMissCount _ = return ()
#endif
{-# INLINE bumpMissCount #-}


#ifdef PROFILE_CACHES
getMetrics :: Cache t -> IO CacheMetrics
getMetrics Cache {queryCount = qc, missCount = mc} = CacheMetrics <$> readIORef qc <*> readIORef mc
#endif

--------------------
------- Interning
--------------------

class ( Eq (Description t)
      , Hashable (Description t)
      ) => Interned t where
  data Description t
  type Uninterned t
  describe :: Uninterned t -> Description t
  identify :: Id -> Uninterned t -> t
  cache        :: Cache t

intern :: Interned t => Uninterned t -> t
intern !bt = unsafeDupablePerformIO $ do
    let c    = cache
    let refI = fresh c
    let ht   = content c
    bumpQueryCount c
    v <- HT.lookup ht dt
    case v of
      Nothing -> do bumpMissCount c
                    i <- atomicModifyIORef' refI (\i -> (i + 1, i))
                    let t = identify i bt
                    HT.insert ht dt t
                    return t
      Just t  -> return t
  where
  !dt = describe bt
