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
hasAny (TyAppT tFun tArg _) = hasAny tFun || hasAny tArg
hasAny (TyFunT tArg tRes) = hasAny tArg || hasAny tRes
hasAny (FunctionT _ tArg tRes) = hasAny tArg || hasAny tRes
hasAny _ = False

scalarName :: TypeSkeleton -> String
scalarName (DatatypeT name _) = name
scalarName (TypeVarT name _) = name
scalarName t = error $ "scalarName error: cannot be applied to nonscalar type "

allDatatypes :: TypeSkeleton -> Set String
allDatatypes (FunctionT _ tArg tRet) = allDatatypes tArg `Set.union` allDatatypes tRet
allDatatypes (DatatypeT id _) = Set.singleton id
allDatatypes (TypeVarT id _) = Set.empty
allDatatypes (TyAppT tFun tArg _) = allDatatypes tFun `Set.union` allDatatypes tArg
allDatatypes (TyFunT tArg tRes) = allDatatypes tArg `Set.union` allDatatypes tRes

arity :: TypeSkeleton -> Int
arity (FunctionT _ _ t) = 1 + arity t
arity _ = 0

kindOf :: TypeSkeleton -> Kind
kindOf (TypeVarT _ k) = k
kindOf (DatatypeT _ k) = k
kindOf (TyAppT _ _ k) = k
kindOf _ = KnStar

argKind :: Kind -> Kind
argKind (KnArr k _) = k
argKind _ = error "Cannot get the kind arg from non-arrow kind"

retKind :: Kind -> Kind
retKind (KnArr _ k) = k
retKind _ = error "Cannot get the kind ret from non-arrow kind"

mkKind :: Int -> Kind
mkKind 0 = KnStar
mkKind i = KnArr KnStar (mkKind (i - 1))

applyKind :: Kind -> Kind -> Kind
applyKind KnStar _ = error "Cannot apply other kinds to *"
applyKind (KnArr arg res) k 
    | arg == k = res
    | otherwise = error "The given argument has the wrong kind" 

substituteKind :: Map Id Kind -> Kind -> Kind
substituteKind kass (KnVar k)
    | k `Map.member` kass = kass Map.! k
    | otherwise = KnAny
substituteKind kass (KnArr k1 k2) = KnArr k1' k2'
    where
        k1' = substituteKind kass k1
        k2' = substituteKind kass k2
substituteKind kass k = k

substituteKindInType :: Map Id Kind -> TypeSkeleton -> TypeSkeleton
substituteKindInType kass (TypeVarT v k) = TypeVarT v (substituteKind kass k)
substituteKindInType kass (DatatypeT v k) = DatatypeT v (substituteKind kass k)
substituteKindInType kass (TyAppT tFun tArg k) = TyAppT tFun' tArg' k'
    where
        tFun' = substituteKindInType kass tFun
        tArg' = substituteKindInType kass tArg
        k' = substituteKind kass k
substituteKindInType kass (TyFunT tArg tRes) = TyFunT tArg' tRes'
    where
        tArg' = substituteKindInType kass tArg
        tRes' = substituteKindInType kass tRes
substituteKindInType kass (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = substituteKindInType kass tArg
        tRes' = substituteKindInType kass tRes
substituteKindInType _ t = t

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

toMonotype :: SchemaSkeleton -> TypeSkeleton
toMonotype (Monotype t) = t
toMonotype (ForallT _ t) = toMonotype t

boundVarsOf :: SchemaSkeleton -> [Id]
boundVarsOf (ForallT a sch) = a : boundVarsOf sch
boundVarsOf _ = []

typeSubstitute :: Map Id TypeSkeleton -> TypeSkeleton -> TypeSkeleton
typeSubstitute subst t@(TypeVarT id _) | id `Map.member` subst = 
    fromJust $ Map.lookup id subst
typeSubstitute subst (TyAppT tFun tArg k) = TyAppT tFun' tArg' k
    where
        tFun' = typeSubstitute subst tFun
        tArg' = typeSubstitute subst tArg
typeSubstitute subst (TyFunT tArg tRes) = TyFunT tArg' tRes'
    where
        tArg' = typeSubstitute subst tArg
        tRes' = typeSubstitute subst tRes
typeSubstitute subst (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = typeSubstitute subst tArg
        tRes' = typeSubstitute subst tRes
typeSubstitute subst t = t

-- | 'typeVarsOf' @t@ : all type variables in @t@
typeVarsOf :: TypeSkeleton -> Set Id
typeVarsOf (TypeVarT id _) = Set.singleton id
typeVarsOf (TyAppT tFun tArg _) = typeVarsOf tFun `Set.union` typeVarsOf tArg
typeVarsOf (TyFunT tArg tRes) = typeVarsOf tArg `Set.union` typeVarsOf tRes
typeVarsOf (FunctionT _ tArg tRes) = typeVarsOf tArg `Set.union` typeVarsOf tRes
typeVarsOf _ = Set.empty

longScalarName :: TypeSkeleton -> String
longScalarName (DatatypeT name _) = name
longScalarName (TypeVarT name _) = name
longScalarName (TyAppT tFun tArg _) = longScalarName tFun ++ longScalarName tArg
longScalarName t = error $ "longScalarName error: cannot be applied to nonscalar type "

{- kind unification -}
kindUnification :: Kind -> Kind -> Kind
kindUnification (KnVar _) k = k
kindUnification k (KnVar _) = k
kindUnification KnStar KnStar = KnStar
kindUnification (KnArr kArg1 kRes1) (KnArr kArg2 kRes2) = KnArr kArg' kRes'
    where
        kArg' = kindUnification kArg1 kArg2
        kRes' = kindUnification kRes1 kRes2
kindUnification k1 k2 = error "Two kinds does not unify"

compareKind :: Kind -> Kind -> Bool
compareKind k1 KnAny = True
compareKind KnAny k2 = True
compareKind k1 k2 | k1 == k2 = True
compareKind (KnArr k1 k2) (KnArr k1' k2') = compareKind k1 k1' && compareKind k2 k2'
compareKind _ _ = False