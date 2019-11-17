{-# LANGUAGE FlexibleContexts #-}
-- | Refinement Types
module Synquid.Type where

import Types.Common hiding (varName)
import Types.Type
import Synquid.Tokens
import Synquid.Util

import Data.Maybe
import Data.Either
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Control.Lens
import GHC.Generics

isFunctionType :: TypeSkeleton -> Bool
isFunctionType (FunctionT _ _ _) = True
isFunctionType _ = False

argType :: TypeSkeleton -> TypeSkeleton
argType (FunctionT _ t _) = t

resType :: TypeSkeleton -> TypeSkeleton
resType (FunctionT _ _ t) = t

isHigherOrder :: TypeSkeleton -> Bool
isHigherOrder (FunctionT _ tArg tRet) = isFunctionType tArg || isHigherOrder tRet
isHigherOrder _ = False

hasAny :: TypeSkeleton -> Bool
hasAny AnyT = True
hasAny (TyAppT tFun tArg) = hasAny tFun || hasAny tArg
hasAny (TyFunT tArg tRes) = hasAny tArg || hasAny tRes
hasAny (FunctionT _ tArg tRes) = hasAny tArg || hasAny tRes
hasAny _ = False

scalarName :: TypeSkeleton -> String
scalarName (DatatypeT name _) = name
scalarName (TypeVarT name) = name
scalarName t = error $ "scalarName error: cannot be applied to nonscalar type "

allDatatypes :: TypeSkeleton -> [String]
allDatatypes (FunctionT _ tArg tRet) = allDatatypes tArg `Set.union` allDatatypes tRet
allDatatypes (DatatypeT id _) = Set.singleton id
allDatatypes (TypeVarT id) = Set.empty
allDatatypes (TyAppT tFun tArg) = allDatatypes tFun `Set.union` allDatatypes tArg
allDatatypes (TyFunT tArg tRes) = allDatatypes tArg `Set.union` allDatatypes tRes

arity :: TypeSkeleton -> Int
arity (FunctionT _ _ t) = 1 + arity t
arity _ = 0

kindOf :: TypeSkeleton -> Kind
kindOf (TyAppT _ _ k) = k
kindOf (TypeVarT _ k) = k
kindOf (DatatypeT _ k) = k
kindOf _ = KnStar

argKind :: Kind -> Kind
argKind (TyArr k _) = k
argKind _ = error "Cannot get the kind arg from non-arrow kind"

retKind :: Kind -> Kind
retKind (TyArr _ k) = k
argKind _ = error "Cannot get the kind ret from non-arrow kind"

mkKind :: Int -> Kind
mkKind 0 = KnStar
mkKind i = KnArr KnStar (mkKind (i - 1))

lastType :: TypeSkeleton -> TypeSkeleton
lastType (FunctionT _ _ tRes) = lastType tRes
lastType t = t

allArgTypes :: TypeSkeleton -> [TypeSkeleton]
allArgTypes (FunctionT x tArg tRes) = tArg : (allArgTypes tRes)
allArgTypes _ = []

allBaseTypes :: TypeSkeleton -> [TypeSkeleton]
allBaseTypes t@TypeVarT {} = [t]
allBaseTypes t@TyAppT {} = [t]
allBaseTypes t@TyFunT {} = [t]
allBaseTypes (FunctionT _ tArg tRet) = allBaseTypes tArg ++ allBaseTypes tRet
allBaseTypes _ = error "allBaseTypes: applied to unsupported types"

-- | Polymorphic type skeletons (parametrized by refinements)

toMonotype :: SchemaSkeleton r -> TypeSkeleton r
toMonotype (Monotype t) = t
toMonotype (ForallT _ t) = toMonotype t

boundVarsOf :: SchemaSkeleton r -> [Id]
boundVarsOf (ForallT a sch) = a : boundVarsOf sch
boundVarsOf _ = []

typeSubstitute :: Map Id TypeSkeleton -> TypeSkeleton -> TypeSkeleton
typeSubstitute subst t@(TypeVarT id) 
    | id `Map.member` subst = fromJust $ Map.lookup id subst
    | otherwise = t
typeSubstitute subst (TyAppT tFun tArg) = TyAppT tFun' tArg'
    where
        tFun' = typeSubstitute subst tFun
        tArg' = typeSubstitute subst tArg
typeSubstitute subst (TyFunT tArg tRes) = TyFunT tArg' tRes'
    where
        tArg' = typeSubstitute subst tArg
        tRes' = typeSUbstitute subst tRes
typeSubstitute subst (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = typeSubstitute subst tArg
        tRes' = typeSubstitute subst tRes
stypeSubstitute subst t = t

-- | 'typeVarsOf' @t@ : all type variables in @t@
typeVarsOf :: TypeSkeleton -> Set Id
typeVarsOf (TypeVarT id) = Set.singleton id
typeVarsOf (TyAppT tFun tArg) = typeVarsOf tFun `Set.union` typeVarsOf tArg
typeVarsOf (TyFunT tArg tRes) = typeVarsOf tArg `Set.union` typeVarsOf tRes
typeVarsOf (FunctionT _ tArg tRes) = typeVarsOf tArg `Set.union` typeVarsOf tRes
typeVarsOf _ = Set.empty

longScalarName :: TypeSkeleton -> String
longScalarName (DatatypeT name _) = name ++ (concatMap longScalarName rs)
longScalarName (TypeVarT name) = name
longScalarName (TyAppT tFun _) = longScalarName tFun
longScalarName t = error $ "longScalarName error: cannot be applied to nonscalar type "