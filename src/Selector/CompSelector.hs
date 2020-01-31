module Selector.CompSelector where

import Types.Type
import Types.Common
import Types.Environment
import Synquid.Type
import Database.Util

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens
import Debug.Trace

selectByType :: RType -> Environment -> Environment
selectByType query env = let
    dts = allDatatypes query
    defs = Set.map ((env ^. datatypes) Map.!) dts
    mdls = "Data.Function" : "Data.Bool" : Map.keys (env ^. arguments) ++ Set.toList (Set.map _srcModule defs)
    updatedSymbol = symbols %~ (Map.filterWithKey (\k _ -> inModules mdls k)) $ env
    updatedEnv = typeClasses %~ (Map.filterWithKey (\k _ -> k `Set.member` dts)) $ updatedSymbol
    in traceShow mdls updatedEnv
    where
        inModules mdls m = or (map (`isInfixOf` m) mdls)