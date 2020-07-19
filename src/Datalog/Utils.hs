module Datalog.Utils where

import Types.Common
import Types.Type
import Synquid.Type

import qualified Data.Set as Set
import qualified Data.Map as Map

varToDatatype :: TypeSkeleton -> TypeSkeleton
varToDatatype tArg =
    let varList = Set.toList (typeVarsOf tArg)
        subst = Map.fromList (zip varList (map DatatypeT varList))
     in typeSubstitute subst tArg
