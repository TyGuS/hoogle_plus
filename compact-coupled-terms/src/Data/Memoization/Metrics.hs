{-# LANGUAGE OverloadedStrings #-}

module Data.Memoization.Metrics (
    CacheMetrics(..)
  ) where


import qualified Data.Text as Text

import Data.Text.Extended.Pretty

----------------------------------------------------------

data CacheMetrics = CacheMetrics { queryCount :: {-# UNPACK #-} !Int
                                 , missCount  :: {-# UNPACK #-} !Int
                                 }
  deriving ( Eq, Ord, Show )


instance Pretty CacheMetrics where
  pretty cm = "Misses/Queries: " <> (Text.pack $ show $ missCount cm) <> " / " <> (Text.pack $ show $ queryCount cm)

