{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}

module PetriNet.AbstractType where

import Synquid.Type
import Synquid.Util

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

data AbstractSkeleton = 
      ADatatypeT Id [AbstractSkeleton] -- explicit datatypes
    | AExclusion (Set Id) -- not included datatypes
    | ATypeVarT Id -- type variable is only temporarily before building the PetriNet
    | AFunctionT AbstractSkeleton AbstractSkeleton
    deriving (Eq, Ord, Show, Generic)

data AbstractionTree = 
      ALeaf AbstractSkeleton 
    | ANode AbstractSkeleton AbstractionTree AbstractionTree
    deriving (Eq, Ord, Show, Generic)

isAFunctionT (AFunctionT {}) = True
isAFunctionT _ = False
notEx (AExclusion _) = False
notEx _ = True
isAHigherOrder (AFunctionT tArg tRet) = isAFunctionT tArg || isAHigherOrder tRet
isAHigherOrder _ = False

abstractionSize :: AbstractSkeleton -> Int
abstractionSize (AExclusion _) = 1
abstractionSize (ADatatypeT id args) = 1 + maximum (map abstractionSize args)
abstractionSize (ATypeVarT _) = 1
abstractionSize (AFunctionT tArg tRet) = abstractionSize tArg + abstractionSize tRet + 1

-- | if t1 is the subtype of t2
isSubtypeOf :: AbstractSkeleton -> AbstractSkeleton -> Bool
isSubtypeOf (ATypeVarT id1) (ATypeVarT id2) = id1 == id2
isSubtypeOf (AExclusion s1) (AExclusion s2) = True -- s2 `Set.isSubsetOf` s1 -- t1 excludes more types than t2
isSubtypeOf (ADatatypeT id1 tys1) (ADatatypeT id2 tys2) = id1 == id2 && foldr ((&&) . uncurry isSubtypeOf) True (zip tys1 tys2)
isSubtypeOf (ADatatypeT id tys) (AExclusion s) = id `Set.notMember` s
isSubtypeOf (ATypeVarT id) (AExclusion s) = id `Set.notMember` s
isSubtypeOf _ _ = False

-- | get the closest abstraction to the given type
closestTree :: AbstractionTree -> AbstractSkeleton -> AbstractionTree
closestTree (ANode t lt rt) at | isSubtypeOf at (valueType lt) = closestTree lt at
closestTree (ANode t lt rt) at | isSubtypeOf at (valueType rt) = closestTree rt at
closestTree tree@(ANode t _ _) at | isSubtypeOf at t = tree
closestTree tree@(ALeaf t) at | isSubtypeOf at t = tree
closestTree tree at = error (printf "cannot find the closest tree for %s in %s" (show at) (show tree))

-- | get all the leaf types in an abstraction tree, which corresponds to
-- all the possible abstract types in our current abstraction level
leafTypes :: AbstractionTree -> [AbstractSkeleton]
leafTypes (ALeaf t) = [t]
leafTypes (ANode _ l r) = leafTypes l ++ leafTypes r

-- | get the rightmost leaf of an abstraction tree, which corresponds to
-- the most generic type we have in the current abstraction level
rightmostType :: AbstractionTree -> AbstractSkeleton
rightmostType (ALeaf t) = t
rightmostType (ANode _ _ r) = rightmostType r

-- | get the type value of the tree node
valueType :: AbstractionTree -> AbstractSkeleton
valueType (ALeaf t) = t
valueType (ANode t _ _) = t

-- | get all the subtypes of some given abstract type
subtypesOf :: AbstractionTree -> AbstractSkeleton -> [AbstractSkeleton]
subtypesOf (ALeaf t) typ | isSubtypeOf t typ || isSubtypeOf typ t = [t]
subtypesOf (ALeaf t) typ | otherwise = []
subtypesOf (ANode t lt rt) typ | not (isSubtypeOf typ t) = []
subtypesOf (ANode t lt rt) typ | t == typ = leafTypes lt ++ leafTypes rt
subtypesOf (ANode t lt rt) typ | otherwise = subtypesOf lt typ ++ subtypesOf rt typ

-- | exclude one type from its parent abstraction level
typeDifference :: AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
typeDifference (AExclusion s) (ADatatypeT id args) = AExclusion (Set.insert id s)
typeDifference (AExclusion s) (ATypeVarT id) = AExclusion (Set.insert id s)
typeDifference (ADatatypeT id1 args1) (ADatatypeT id2 args2) | id1 == id2 =
    ADatatypeT id1 (firstDifference args1 args2)
  where
    firstDifference [] [] = error "two types are identical to each other"
    firstDifference (arg:args) (arg':args') | arg == arg' = arg:(firstDifference args args')
    firstDifference (arg:args) (arg':_) = (typeDifference arg arg'):args
typeDifference t1 t2 = error (printf "cannot compute difference between %s and %s" (show t1) (show t2))

decompose :: AbstractSkeleton -> [AbstractSkeleton]
decompose (AFunctionT tArg tRet) = decompose tArg ++ decompose tRet
decompose t = [t]

toAbstractType :: SType -> AbstractSkeleton
toAbstractType (ScalarT (TypeVarT _ id) _) = ATypeVarT id
toAbstractType (ScalarT (DatatypeT id tArgs _) _) = ADatatypeT id (map toAbstractType tArgs)
toAbstractType (FunctionT x tArg tRet) = AFunctionT (toAbstractType tArg) (toAbstractType tRet)
toAbstractType AnyT = AExclusion Set.empty

allAbstractDts :: [Id] -> SType -> Set AbstractSkeleton
allAbstractDts tvs (FunctionT _ tArg tRet) = allAbstractDts tvs tArg `Set.union` allAbstractDts tvs tRet
allAbstractDts tvs (ScalarT (DatatypeT id tArgs _) _) = outerDt `Set.insert` argDts
  where
    outerDt = ADatatypeT id (map (\_ -> AExclusion Set.empty) tArgs)
    argDts = foldr (Set.union . allAbstractDts tvs) Set.empty tArgs
allAbstractDts _ (ScalarT IntT _) = Set.empty
allAbstractDts _ (ScalarT BoolT _) = Set.empty
allAbstractDts tvs (ScalarT (TypeVarT _ id) _) | id `elem` tvs = Set.singleton (ATypeVarT id)
allAbstractDts tvs (ScalarT (TypeVarT _ id) _) | otherwise = Set.empty

outerName :: AbstractSkeleton -> Either (Set Id) (Set Id)
outerName (ADatatypeT id _) = Left (Set.singleton id)
outerName (ATypeVarT id) = Right Set.empty
outerName (AExclusion names) = Right names

allAbstractBase :: [Id] -> AbstractSkeleton -> [AbstractSkeleton]
allAbstractBase bound t@(ADatatypeT _ _)     = if hasAbstractVar t then [] else [t]
allAbstractBase bound (AFunctionT tArg tRet) = allAbstractBase bound tArg ++ allAbstractBase bound tRet
allAbstractBase bound t@(ATypeVarT id)       = if id `elem` bound then [t] else []
allAbstractBase bound t@(AExclusion ts)      = if Set.null ts then [] else [t]

allAbstractVar :: AbstractSkeleton -> Set Id
allAbstractVar (ATypeVarT id)         = Set.singleton id
allAbstractVar (ADatatypeT _ tys)     = foldr (Set.union . allAbstractVar) Set.empty tys
allAbstractVar (AFunctionT tArg tRet) = allAbstractVar tArg `Set.union` allAbstractVar tRet
allAbstractVar (AExclusion _)         = Set.empty

hasAbstractVar :: AbstractSkeleton -> Bool
hasAbstractVar (ATypeVarT id)         = True
hasAbstractVar (ADatatypeT _ tys)     = foldr ((||) . hasAbstractVar) False tys
hasAbstractVar (AExclusion _)         = False
hasAbstractVar (AFunctionT tArg tRet) = hasAbstractVar tArg || hasAbstractVar tRet

abstractSubstitute :: [Id] -> Id -> AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
abstractSubstitute bound id bt t@(ADatatypeT name ts) = ADatatypeT name (map (abstractSubstitute bound id bt) ts)
abstractSubstitute bound id bt t@(AExclusion _)       = t
abstractSubstitute bound id bt t@(ATypeVarT var)      = if id == var && id `notElem` bound then bt else t
abstractSubstitute bound id bt (AFunctionT tArg tRet) = AFunctionT (abstractSubstitute bound id bt tArg) (abstractSubstitute bound id bt tRet)

type AbstractSubstitution = Map Id (Set AbstractSkeleton)

unfoldSubst :: AbstractSubstitution -> [ Map Id AbstractSkeleton ]
unfoldSubst subst = map Map.fromList (combinations (Map.toList subst))
  where
    combinations [] = [] 
    combinations [(id, set)] = map (\t -> [(id, t)]) (Set.toList set) 
    combinations ((id, set):substs) = [ (id, x):xs | x <- Set.toList set
                                                   , xs <- combinations substs ]

applySubst :: [Id] -> AbstractSubstitution -> AbstractSkeleton -> [AbstractSkeleton]
applySubst bound subst typ = map (applySubst' . Map.toList) allSubsts
  where
    allSubsts = unfoldSubst subst
    applySubst' = foldr (uncurry (abstractSubstitute bound)) typ

unifier :: AbstractionTree -> [Id] -> AbstractSkeleton -> AbstractSkeleton -> Maybe AbstractSubstitution
unifier tree bound (ATypeVarT id1) (ATypeVarT id2) | id1 == id2 = Just Map.empty
unifier tree bound (ATypeVarT id1) (ATypeVarT id2) | id1 `elem` bound && id2 `elem` bound = Nothing
unifier tree bound (ATypeVarT id1) t@(ATypeVarT id2) | id2 `elem` bound = Just (Map.singleton id1 (Set.singleton t))
unifier tree bound t@(ATypeVarT id1) (ATypeVarT id2) | id1 `elem` bound = Just (Map.singleton id2 (Set.singleton t))
-- TODO we should get all the types having the similar shape of t2 but more refined than t2
unifier tree bound t (ATypeVarT id) = Just (Map.singleton id (Set.fromList (subtypesOf tree t)))
unifier tree bound (ATypeVarT id) t = Just (Map.singleton id (Set.fromList (subtypesOf tree t)))
-- TODO we should intersect all the substitutions get from inner types
unifier tree bound (ADatatypeT dt1 tys1) (ADatatypeT dt2 tys2) | dt1 == dt2 =
    if null maps 
       then Just Map.empty 
       else if null emptyMaps
               then Just (Map.unionsWith Set.intersection (map fromJust maps))
               else Nothing
  where
    maps = map (uncurry (unifier tree bound)) (zip tys1 tys2)
    emptyMaps = filter isNothing maps
unifier tree bound (ADatatypeT dt tys) (AExclusion s) | dt `Set.notMember` s =
    Just (Map.unionsWith Set.intersection (map fromJust maps))
  where
    maps = map (unifier tree bound (AExclusion Set.empty)) tys
unifier tree bound t1@(AExclusion {}) t2@(ADatatypeT {}) = unifier tree bound t2 t1
unifier bound _ _ _ = Nothing
