module Datalog.Utils where

import Types.Type
import Synquid.Type

import qualified Data.Set as Set
import qualified Data.Map as Map

varToDatatype :: SType -> SType
varToDatatype tArg =
    let varList = Set.toList (typeVarsOf tArg)
        subst = Map.fromList (zip varList (map datatype_ varList))
     in stypeSubstitute subst tArg
