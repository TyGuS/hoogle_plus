module Selector.CompSelector where

import Database.Util
import Synquid.Type
import Types.Common
import Types.Environment
import Types.Type

import Control.Lens
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace

defaultPkgs = ["Data.Function", "Data.Bool", "GHC.List"]

selectByType :: RType -> Environment -> Environment
selectByType query env =
    let dts = allDatatypes query
        defs = Set.map ((env ^. datatypes) Map.!) dts
        mdls = Map.keys (env ^. arguments) ++ Set.toList (Set.map _srcModule defs) ++ defaultPkgs
        updatedSymbol = symbols %~ (Map.filterWithKey (\k _ -> inModules mdls k)) $ env
        updatedEnv = typeClasses %~ (Map.filterWithKey (\k _ -> k `Set.member` dts)) $ updatedSymbol
     in traceShow mdls updatedEnv
  where
    inModules mdls m = or (map (`isInfixOf` m) mdls)
