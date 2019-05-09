{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}

module PetriNet.AbstractType where

import Types.Abstract
import Types.Type
import Types.Common
import Synquid.Type
import Types.Solver
import PetriNet.Util
import Synquid.Pretty

import Control.Lens
import GHC.Generics
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson
import Data.Maybe
import Data.Either (isLeft)
import Data.List
import Text.Printf
import Control.Monad.State
import Debug.Trace

isAFunctionT :: AbstractSkeleton -> Bool
isAFunctionT (AFunctionT {}) = True
isAFunctionT _ = False

isAHigherOrder :: AbstractSkeleton -> Bool
isAHigherOrder (AFunctionT tArg tRet) = isAFunctionT tArg || isAHigherOrder tRet
isAHigherOrder _ = False

lastAbstract :: AbstractSkeleton -> AbstractSkeleton
lastAbstract (AFunctionT _ tRet) = lastAbstract tRet
lastAbstract t = t

-- | get all the leaf types in an abstraction tree, which corresponds to
-- all the possible abstract types in our current abstraction level
leafTypes :: AbstractionTree -> [Abstraction]
leafTypes (ALeaf t) = [t]
leafTypes (ANode _ l r) = leafTypes l ++ leafTypes r

allTypes :: AbstractionTree -> [Abstraction]
allTypes (ALeaf t) = [t]
allTypes (ANode t l r) = t : leafTypes l ++ leafTypes r

decompose :: AbstractSkeleton -> [Abstraction]
decompose (AFunctionT tArg tRet) = nub (decompose tArg ++ decompose tRet)
decompose (AScalar t) = [t]

decomposeHo :: AbstractSkeleton -> [AbstractSkeleton]
decomposeHo (AFunctionT tArg tRet) = nub (tArg : decomposeHo tRet)
decomposeHo t = [t]

toAbstractType :: SType -> AbstractSkeleton
toAbstractType t@(ScalarT {}) = AScalar (Set.singleton (TypeShape (ScalarT (TypeVarT Map.empty varName) ()) t))
toAbstractType (FunctionT x tArg tRet) = AFunctionT (toAbstractType tArg) (toAbstractType tRet)
toAbstractType AnyT = AScalar Set.empty

nodeValue :: AbstractionTree -> Abstraction
nodeValue (ALeaf t) = t
nodeValue (ANode t _ _) = t

abstractionRename :: Id -> Id -> Abstraction -> Abstraction
abstractionRename old new = Set.map (constraintRename old new)

constraintRename :: Id -> Id -> UnifConstraint -> UnifConstraint
constraintRename old new (TypeShape t1 t2) = TypeShape t1' t2'
  where
    m = Map.singleton old (ScalarT (TypeVarT Map.empty new) ())
    t1' = stypeSubstitute m t1
    t2' = stypeSubstitute m t2
constraintRename old new (NotShape t1 t2) = NotShape t1' t2'
  where
    m = Map.singleton old (ScalarT (TypeVarT Map.empty new) ())
    t1' = stypeSubstitute m t1
    t2' = stypeSubstitute m t2

abstractSkeletonRename :: Id -> Id -> AbstractSkeleton -> AbstractSkeleton
abstractSkeletonRename old new (AScalar cs) = AScalar (abstractionRename old new cs)
abstractSkeletonRename old new (AFunctionT tArg tRes) = AFunctionT tArg' tRes'
  where
    tArg' = abstractSkeletonRename old new tArg
    tRes' = abstractSkeletonRename old new tRes

weaker :: [Id] -> UnifConstraint -> UnifConstraint -> Bool
weaker bound (TypeShape (ScalarT (TypeVarT _ v1) _) (ScalarT (DatatypeT id1 tys1 _) _)) 
             (TypeShape (ScalarT (TypeVarT _ v2) _) (ScalarT (DatatypeT id2 tys2 _) _)) | v1 == v2 && id1 == id2 =
    foldr (\(t1, t2) -> (&&) (stypeWeaker bound t1 t2)) True (zip tys1 tys2)
weaker bound (TypeShape (ScalarT (TypeVarT _ v1) _) (ScalarT (TypeVarT _ v2) _)) 
             (TypeShape (ScalarT (TypeVarT _ v3) _) (ScalarT (TypeVarT _ v4) _)) | v1 == v3 && v2 `elem` bound && v4 `elem` bound = v2 == v4
weaker bound (TypeShape (ScalarT (TypeVarT _ v1) _) (ScalarT (TypeVarT _ v2) _)) 
             (TypeShape (ScalarT (TypeVarT _ v3) _) _) | v1 == v3 && not (v2 `elem` bound) = True 
weaker bound (NotShape (ScalarT (TypeVarT _ v1) _) t1) 
             (NotShape (ScalarT (TypeVarT _ v2) _) t2) | v1 == v2 = 
    weaker bound (TypeShape (ScalarT (TypeVarT Map.empty varName) ()) t2) (TypeShape (ScalarT (TypeVarT Map.empty varName) ())t1)
weaker bound (NotShape (ScalarT (TypeVarT _ v3) _) (ScalarT (TypeVarT _ v4) _))
             (TypeShape (ScalarT (TypeVarT _ v1) _) (ScalarT (TypeVarT _ v2) _)) | v1 == v3 && v2 /= v4 = True
weaker bound (NotShape (ScalarT (TypeVarT _ v1) _) (ScalarT (DatatypeT id1 _ _) _))
             (TypeShape (ScalarT (TypeVarT _ v2) _) (ScalarT (DatatypeT id2 _ _) _)) | v1 == v2 && id1 /= id2 = True
weaker bound (NotShape (ScalarT (TypeVarT _ v1) _) (ScalarT (TypeVarT {}) _))
             (TypeShape (ScalarT (TypeVarT _ v2) _) (ScalarT (DatatypeT {}) _)) | v1 == v2 = True
weaker bound (NotShape (ScalarT (TypeVarT _ v1) _) (ScalarT (DatatypeT {}) _))
             (TypeShape (ScalarT (TypeVarT _ v2) _) (ScalarT (TypeVarT {}) _)) | v1 == v2 = True
weaker _ _ _ = False

stypeWeaker :: [Id] -> SType -> SType -> Bool
stypeWeaker bound (ScalarT (TypeVarT _ v1) _) (ScalarT (TypeVarT _ v2) _) | v1 == v2 = True
stypeWeaker bound (ScalarT (TypeVarT _ v1) _) _ | v1 `elem` bound = False
stypeWeaker bound (ScalarT (TypeVarT _ v1) _) _ | otherwise = True
stypeWeaker bound (ScalarT (DatatypeT id1 tys1 _) _) (ScalarT (DatatypeT id2 tys2 _) _) | id1 == id2 =
    foldr (\(t1, t2) -> (&&) (stypeWeaker bound t1 t2)) True (zip tys1 tys2)
stypeWeaker _ _ _ = False

simplify :: [Id] -> Abstraction -> Abstraction
simplify bound at = Set.fromList (simplify' bound (Set.toList at))

simplify' :: [Id] -> [UnifConstraint] -> [UnifConstraint]
simplify' _ [] = []
simplify' bound (c:cs) = if or (map (weaker bound c) cs') then simplify' bound cs' 
                                                          else c : simplify' bound cs'
  where
    cs' = filter (\c' -> not (weaker bound c' c)) cs

isSubtypeOf :: [Id] -> Abstraction -> Abstraction -> Bool
isSubtypeOf bound at1 at2 = and (map (\c -> or (map (weaker bound c) (Set.toList at1))) (Set.toList at2))

checkUnification :: [Id] -> Map Id SType -> Map Id SType -> UnifConstraint -> Maybe (Map Id SType, Map Id SType)
checkUnification bound tass dass (TypeShape t t') | t == t' = Just (tass, dass)
checkUnification bound tass _ (TypeShape (ScalarT (TypeVarT _ id) _) (ScalarT (TypeVarT _ id') _)) | id `elem` bound && id' `elem` bound = Nothing
checkUnification bound tass dass (TypeShape t'@(ScalarT (TypeVarT _ id) _) t) | id `elem` bound = checkUnification bound tass dass (TypeShape t t')
checkUnification bound tass dass (TypeShape (ScalarT (TypeVarT _ id) _) t) | id `Map.member` tass =
    checkUnification bound tass dass (TypeShape assigned t)
  where
    assigned = fromJust (Map.lookup id tass)
checkUnification bound tass _ (TypeShape (ScalarT (DatatypeT {}) _) (ScalarT (TypeVarT _ id) _)) | id `elem` bound = Nothing
checkUnification bound tass dass (TypeShape t@(ScalarT (DatatypeT {}) _) (ScalarT (TypeVarT _ id) _)) = Just (Map.insert id t tass, dass)
checkUnification bound tass dass (TypeShape (ScalarT (TypeVarT _ id) _) t) = Just (Map.insert id t tass, dass)
checkUnification bound tass _ (TypeShape (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _)) | id /= id' = Nothing
checkUnification bound tass dass (TypeShape (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _)) | id == id' = checkArgs tass dass tArgs tArgs'
  where
    checkArgs m d [] [] = Just (m, d)
    checkArgs m d (arg:args) (arg':args') =
        case checkUnification bound m d (TypeShape arg arg') of
            Nothing -> Nothing
            Just (m', d')  -> checkArgs m' d' args args'
checkUnification bound tass _ (TypeShape _ _) = Nothing
checkUnification bound tass dass (NotShape t@(ScalarT (TypeVarT _ id) _) t') | id `Map.member` tass =
    let assigned = fromJust (Map.lookup id tass)
        substed = stypeSubstitute tass assigned
     in if stypeWeaker bound t' substed 
           then Nothing 
           else case checkUnification bound dass Map.empty (TypeShape t t') of
                    Just (dass', _) -> Just (tass, dass')
                    Nothing -> Just (tass, dass)
checkUnification bound tass _ (NotShape (ScalarT (TypeVarT _ id) _) t) | otherwise = error $ "Unknown type variable " ++ id

typeConstraints :: MonadIO m => AbstractSkeleton -> AbstractSkeleton -> PNSolver m (Set UnifConstraint)
typeConstraints (AFunctionT tArg tRet) (AFunctionT tArg' tRet') = do
    argCons <- typeConstraints tArg tArg'
    retCons <- typeConstraints tRet tRet'
    return (argCons `Set.union` retCons)
typeConstraints (AScalar t1) (AScalar t2) = do
    v <- freshId varName
    return (abstractionRename varName v (t1 `Set.union` t2))

getUnifier :: [Id] -> [UnifConstraint] -> Maybe (Map Id SType, Map Id SType)
getUnifier bound cs = getUnifier' bound (Just Map.empty) Map.empty cs

getUnifier' :: [Id] -> Maybe (Map Id SType) -> Map Id SType -> [UnifConstraint] -> Maybe (Map Id SType, Map Id SType)
getUnifier' _ Nothing _ _ = Nothing
getUnifier' bound (Just tass) dass [] = Just (tass, dass)
getUnifier' bound (Just tass) dass (c:cs) =
    case checkUnification bound tass dass c of
        Nothing -> Nothing
        Just (m, d)  -> getUnifier' bound (Just m) d cs

abstractSubstitute :: Id -> SType -> AbstractSkeleton -> AbstractSkeleton
abstractSubstitute id typ (AScalar cons) = AScalar (Set.map (constraintSubstitute id typ) cons)
abstractSubstitute id typ (AFunctionT tArg tRes) = AFunctionT tArg' tRes'
  where
    tArg' = abstractSubstitute id typ tArg
    tRes' = abstractSubstitute id typ tRes

constraintSubstitute :: Id -> SType -> UnifConstraint -> UnifConstraint
constraintSubstitute id typ (TypeShape t1 t2) = TypeShape t1' t2'
  where
    t1' = stypeSubstitute (Map.singleton id typ) t1
    t2' = stypeSubstitute (Map.singleton id typ) t2
constraintSubstitute id typ (NotShape t1 t2) = NotShape t1' t2'
  where
    t1' = stypeSubstitute (Map.singleton id typ) t1
    t2' = stypeSubstitute (Map.singleton id typ) t2

addDisunification :: Map Id SType -> AbstractSkeleton -> AbstractSkeleton
addDisunification dass (AScalar cons) = AScalar (Set.unions $ cons : map (corrDisunif dass) (Set.toList cons))
addDisunification dass (AFunctionT tArg tRes) = AFunctionT tArg' tRes'
  where
    tArg' = addDisunification dass tArg
    tRes' = addDisunification dass tRes

corrDisunif :: Map Id SType -> UnifConstraint -> Set UnifConstraint
corrDisunif dass (TypeShape t1 t2) = Set.fromList disunifs
  where
    vars = typeVarsOf t1 `Set.union` typeVarsOf t2
    hasVar k = Set.member k vars
    relConstraints = Map.filterWithKey (\k _ -> hasVar k) dass
    disunifs = map (\(k, t) -> NotShape (ScalarT (TypeVarT Map.empty k) ()) t) (Map.toList relConstraints)
corrDisunif dass (NotShape t1 t2) = corrDisunif dass (TypeShape t1 t2)
