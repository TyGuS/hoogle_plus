module Data.Interned.Extended.SingleThreaded (
    intern
  ) where


import Data.Array
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import GHC.IO (unsafeDupablePerformIO)

import Data.Interned.Internal hiding ( intern )

--------------------------------------------------

intern :: Interned t => Uninterned t -> t
intern !bt = unsafeDupablePerformIO $ modifyAdvice $ do
    CacheState i m <- readIORef slot
    case HashMap.lookup dt m of
      Nothing -> do let t = identify (wid * i + r) bt
                    writeIORef slot (CacheState (i + 1) (HashMap.insert dt t m))
                    return t
      Just t  -> return t
  where
  slot = getCache cache ! r
  !dt = describe bt
  !hdt = hash dt
  !wid = cacheWidth dt
  r = hdt `mod` wid