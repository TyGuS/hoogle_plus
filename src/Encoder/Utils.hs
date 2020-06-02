module Encoder.Utils where

import Data.Maybe
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

findVariable :: (Eq k, Hashable k, Show k) => String -> k -> HashMap k v -> v
findVariable blame k m = fromMaybe (error $ "cannot find in " ++ blame ++ " variable for " ++ show k)
                                   (HashMap.lookup k m)

