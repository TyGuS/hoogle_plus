{-# LANGUAGE FlexibleContexts #-}
-- | Refinement Types
module Synquid.Type where

import Types.Common hiding (varName)
import Types.Type
import Synquid.Utils

import Data.Maybe
import Data.Either
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad
import Control.Lens
import GHC.Generics

isFunctionType :: TypeSkeleton -> Bool
isFunctionType FunctionT {} = True
isFunctionType _ = False

argType :: TypeSkeleton -> TypeSkeleton
argType (FunctionT _ t _) = t

resType :: TypeSkeleton -> TypeSkeleton
resType (FunctionT _ _ t) = t

isHigherOrder :: TypeSkeleton -> Bool
isHigherOrder (FunctionT _ tArg tRet) = isFunctionType tArg || isHigherOrder tRet
isHigherOrder _ = False

isBot :: AbstractSkeleton -> Bool
isBot BottomT = True
isBot _ = False

hasAny :: TypeSkeleton -> Bool
hasAny AnyT = True
hasAny (TyAppT tFun tArg) = hasAny tFun || hasAny tArg
hasAny (TyFunT tArg tRes) = hasAny tArg || hasAny tRes
hasAny (FunctionT _ tArg tRes) = hasAny tArg || hasAny tRes
hasAny _ = False

scalarName :: TypeSkeleton -> String
scalarName (DatatypeT name) = name
scalarName (TypeVarT name) = name
scalarName t = error $ "scalarName error: cannot be applied to nonscalar type "

allDatatypes :: TypeSkeleton -> Set String
allDatatypes (FunctionT _ tArg tRet) = allDatatypes tArg `Set.union` allDatatypes tRet
allDatatypes (DatatypeT id) = Set.singleton id
allDatatypes (TypeVarT id) = Set.empty
allDatatypes (TyAppT tFun tArg) = allDatatypes tFun `Set.union` allDatatypes tArg
allDatatypes (TyFunT tArg tRes) = allDatatypes tArg `Set.union` allDatatypes tRes

arity :: TypeSkeleton -> Int
arity (FunctionT _ _ t) = 1 + arity t
arity _ = 0

lastType :: TypeSkeleton -> TypeSkeleton
lastType (FunctionT _ _ tRes) = lastType tRes
lastType t = t

allArgTypes :: TypeSkeleton -> [TypeSkeleton]
allArgTypes (FunctionT x tArg tRes) = tArg : (allArgTypes tRes)
allArgTypes _ = []

breakdown :: TypeSkeleton -> [TypeSkeleton]
breakdown (FunctionT _ tArg tRes) = tArg : (breakdown tRes)
breakdown t = [t]

allBaseTypes :: TypeSkeleton -> [TypeSkeleton]
allBaseTypes t@TypeVarT {} = [t]
allBaseTypes t@DatatypeT {} = [t]
allBaseTypes t@TyAppT {} = [t]
allBaseTypes t@TyFunT {} = [t]
allBaseTypes (FunctionT _ tArg tRet) = allBaseTypes tArg ++ allBaseTypes tRet
allBaseTypes _ = error "allBaseTypes: applied to unsupported types"

collectArgs :: TypeSkeleton -> (Id, [TypeSkeleton])
collectArgs (TyAppT tFun tArg) = (dt, reverse (tArg : args))
    where
        (dt, args) = collectArgs tFun
collectArgs (DatatypeT dt) = (dt, [])
collectArgs (TypeVarT id) = error "we do not have higher order kinded type variables"

-- | Polymorphic type skeletons (parametrized by refinements)
toMonotype :: SchemaSkeleton -> TypeSkeleton
toMonotype (Monotype t) = t
toMonotype (ForallT _ t) = toMonotype t

toPolytype :: [Id] -> TypeSkeleton -> SchemaSkeleton
toPolytype bound t = foldr ForallT (Monotype t) vars
    where
        vars = typeVarsOf t `Set.difference` Set.fromList bound

boundVarsOf :: SchemaSkeleton -> [Id]
boundVarsOf (ForallT a sch) = a : boundVarsOf sch
boundVarsOf _ = []

typeSubstitute :: Map Id TypeSkeleton -> TypeSkeleton -> TypeSkeleton
typeSubstitute subst t@(TypeVarT id) | id `Map.member` subst =
    typeSubstitute subst $ fromJust $ Map.lookup id subst
typeSubstitute subst (TyAppT tFun tArg) = TyAppT tFun' tArg'
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
typeVarsOf (TypeVarT id) = Set.singleton id
typeVarsOf (TyAppT tFun tArg) = typeVarsOf tFun `Set.union` typeVarsOf tArg
typeVarsOf (TyFunT tArg tRes) = typeVarsOf tArg `Set.union` typeVarsOf tRes
typeVarsOf (FunctionT _ tArg tRes) = typeVarsOf tArg `Set.union` typeVarsOf tRes
typeVarsOf _ = Set.empty

longScalarName :: TypeSkeleton -> String
longScalarName (DatatypeT name) = name
longScalarName (TypeVarT name) = name
longScalarName (TyAppT tFun tArg) = longScalarName tFun ++ longScalarName tArg
longScalarName t = error $ "longScalarName error: cannot be applied to nonscalar type "

argsWithName :: TypeSkeleton -> [(Id, TypeSkeleton)]
argsWithName (FunctionT x tArg tRes) = (x, tArg) : argsWithName tRes
argsWithName _ = []

eqType :: TypeSkeleton -> TypeSkeleton -> Bool
eqType t1 t2 = let (r, _, _) = eqType' [] [] t1 t2 in r

type Matches = [(Id, Id)]

eqType' :: Matches -> Matches -> TypeSkeleton -> TypeSkeleton -> (Bool, Matches, Matches)
eqType' m1 m2  (TypeVarT v1) (TypeVarT v2) =
    case lookup v1 m1 of
        Nothing -> case lookup v2 m2 of
                        Nothing -> (True, (v1, v2):m1, (v2, v1):m2)
                        Just v1'    | v1 == v1' -> (True, m1, m2)
                                    | otherwise -> (False, m1, m2)
        Just v  | v == v2 -> (True, m1, m2)
                | otherwise -> (False, m1, m2)
eqType' m1 m2 (DatatypeT dt1) (DatatypeT dt2) = (dt1 == dt2, m1, m2)
eqType' m1 m2 (TyAppT tFun1 tArg1) (TyAppT tFun2 tArg2) =
    if b
        then eqType' m1' m2' tArg1 tArg2
        else (b, m1', m2')
    where
        (b, m1', m2') = eqType' m1 m2 tFun1 tFun2
eqType' m1 m2 (TyFunT tArg1 tRes1) (TyFunT tArg2 tRes2) =
    if b
        then eqType' m1' m2' tRes1 tRes2
        else (b, m1', m2')
    where
        (b, m1', m2') = eqType' m1 m2 tArg1 tArg2
eqType' m1 m2 (FunctionT _ tArg1 tRes1) (FunctionT _ tArg2 tRes2) =
    if b
        then eqType' m1' m2' tRes1 tRes2
        else (False, m1', m2')
    where
        (b, m1', m2') = eqType' m1 m2 tArg1 tArg2
eqType' m1 m2 _ _ = (False, m1, m2)

permuteArgs :: [Int] -> SchemaSkeleton -> SchemaSkeleton
permuteArgs ords (ForallT x t) = ForallT x (permuteArgs ords t)
permuteArgs ords (Monotype t) = let args = argsWithName t
                                    ret = lastType t
                                 in Monotype $ foldr (uncurry FunctionT) ret (permuteBy ords args)

withSchema :: (TypeSkeleton -> TypeSkeleton) -> SchemaSkeleton -> SchemaSkeleton
withSchema f (ForallT x t) = ForallT x (withSchema f t)
withSchema f (Monotype t) = Monotype (f t)

hoArgsOf :: TypeSkeleton -> [TypeSkeleton]
hoArgsOf (TyAppT tFun tArg) = hoArgsOf tFun ++ if isFunctionType tArg then [tArg] else hoArgsOf tArg
hoArgsOf (FunctionT _ tArg tRes) = hoArgsOf tRes ++ if isFunctionType tArg then [tArg] else hoArgsOf tArg
hoArgsOf _ = []

containsType :: TypeSkeleton -> [TypeSkeleton] -> [TypeSkeleton]
containsType t = filter (\tt -> tt == t || t `elem` hoArgsOf tt)
