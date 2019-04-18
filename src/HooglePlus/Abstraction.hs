module HooglePlus.Abstraction where

import Types.Common
import Types.Type
import Types.Abstract
import Types.Environment
import PetriNet.AbstractType
import Synquid.Type
import Types.Environment
import Synquid.Util
import Synquid.Program

import Data.Maybe
import Data.List
import Data.List.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Lens
import qualified Data.Char as Char
import Text.Printf

firstLvAbs :: Environment -> [RSchema] -> AbstractionTree
firstLvAbs env schs =
    Set.foldl' (updateSemantic env) (ALeaf (AExclusion Set.empty)) dts
  where
    typs = map (shape . toMonotype) schs
    dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars)) typs)


-- Produce the most specific abstraction possible from the given types.
specificAbstractionFromTypes :: Environment -> [RSchema] -> AbstractionTree
specificAbstractionFromTypes env schemas = let
    abstrSkels = map (toAbstractType . shape) (concatMap (allBaseTypes . toMonotype) schemas)
    baseTree = ALeaf (AExclusion Set.empty)
    in
        foldr (flip $ updateSemantic env) baseTree abstrSkels


updateSemantic :: Environment -> AbstractionTree -> AbstractSkeleton -> AbstractionTree
updateSemantic env semantic typ@(ATypeVarT id) | isBound env id =
  case semantic of
    ALeaf t
        | t == typ -> semantic
        | otherwise -> ANode t (ALeaf typ) (ALeaf (typeDifference t typ))
    ANode t lt rt
        | t == typ -> semantic
        | isSubtypeOf typ t && isSubtypeOf typ (valueType lt) -> ANode t (updateSemantic env lt typ) rt
        | isSubtypeOf typ t -> ANode t lt (updateSemantic env rt typ)
        | otherwise -> error (printf "%s is not a subtype of %s, not a subtype of %s, not a subtype of %s, we should not reach here" (show typ) (show t) (show (valueType lt)) (show (valueType rt)))
updateSemantic _ semantic (ATypeVarT id) | otherwise = semantic
updateSemantic _ semantic@(ALeaf t) typ@(ADatatypeT {}) | typ == t = semantic
updateSemantic env semantic@(ALeaf t@(ADatatypeT id tArgs)) typ@(ADatatypeT id' tArgs') =
  case (tArgs, tArgs') of
    ([], []) -> semantic
    _ -> firstDiff semantic [] tArgs tArgs'
  where
    wrapTree preArgs postArgs (ALeaf t) = ALeaf (ADatatypeT id (preArgs ++ [t] ++ postArgs))
    wrapTree preArgs postArgs (ANode t lt rt) =
        ANode (ADatatypeT id (preArgs ++ [t] ++ postArgs))
              (wrapTree preArgs postArgs lt)
              (wrapTree preArgs postArgs rt)

    rootType (ALeaf t) = t
    rootType (ANode t _ _) = t

    replaceTree (ALeaf t) tree | rootType tree == t = tree
    replaceTree (ANode t lt rt) tree | rootType tree == t = error "should not replace a non-leaf type"
    replaceTree (ANode t lt rt) tree | isSubtypeOf (rootType tree) (valueType lt) = ANode t (replaceTree lt tree) rt
    replaceTree (ANode t lt rt) tree | isSubtypeOf (rootType tree) (valueType rt) = ANode t lt (replaceTree rt tree)
    replaceTree t t' = error $ "unhandled case with " ++ show t ++ " and " ++ show t'

    firstDiff s _ [] [] = s
    firstDiff s pre (arg:args) (arg':args')
      | arg == arg' = firstDiff s (pre ++ [arg]) args args'
      | otherwise   = let tmp = updateSemantic env (ALeaf arg) arg'
                          s' = replaceTree s (wrapTree pre args tmp)
                      in firstDiff s' (pre ++ [arg']) args args'
updateSemantic env semantic@(ALeaf t) typ@(ADatatypeT id tArgs) | t /= typ =
    updateSemantic env semantic' typ
  where
    emptyArgs = map fillAny tArgs
    typ' = ADatatypeT id emptyArgs
    semantic' = ANode t (ALeaf typ') (ALeaf (typeDifference t typ'))
updateSemantic env semantic@(ANode t lt rt) typ
  | t == typ = semantic
  | isSubtypeOf typ t && isSubtypeOf typ (valueType lt) = ANode t (updateSemantic env lt typ) rt
  | isSubtypeOf typ t = ANode t lt (updateSemantic env rt typ)
  | otherwise = error (printf "%s is not a subtype of %s, not a subtype of %s, not a subtype of %s, we should not reach here" (show typ) (show t) (show (valueType lt)) (show (valueType rt)))
updateSemantic env semantic (AFunctionT tArg tRet) = semantic''
  where
    semantic' = updateSemantic env semantic tArg
    semantic'' = updateSemantic env semantic' tRet
updateSemantic env semantic@(ALeaf (AExclusion s)) (AExclusion s') =
    foldl' (updateSemantic env) semantic (absVars ++ absDts)
  where
    buildDt dt = case Map.lookup dt (env ^. datatypes) of
                   Nothing -> error $ "cannot find datatype " ++ dt
                   Just dtDef -> ADatatypeT dt (map fillAny (dtDef ^. typeParams))
    (vars, dts) = partition (Char.isLower . head) (Set.toList (Set.difference s' s))
    absVars = map ATypeVarT vars
    absDts = map buildDt dts
updateSemantic env semantic (AExclusion s) =
    -- add all the complementary datatypes or type variables into the semantic
    foldl' (updateSemantic env) semantic (absVars ++ absDts)
  where
    buildDt dt = case Map.lookup dt (env ^. datatypes) of
                   Nothing -> error $ "cannot find datatype " ++ dt
                   Just dtDef -> ADatatypeT dt (map fillAny (dtDef ^. typeParams))
    (vars, dts) = partition (Char.isLower . head) (Set.toList s)
    absVars = map ATypeVarT vars
    absDts = map buildDt dts


cutoff :: Environment -> AbstractionTree -> AbstractSkeleton -> [AbstractSkeleton]
cutoff env semantic (ATypeVarT id) | not (isBound env id) = [rightmostType semantic]
cutoff env semantic typ@(ATypeVarT id) | isBound env id =
    case semantic of
      ALeaf t -> [t]
      ANode t lt rt | t == typ -> [typ]
      ANode t lt rt | isSubtypeOf typ (valueType lt) -> cutoff env lt typ
      ANode t lt rt | isSubtypeOf typ (valueType rt) -> cutoff env rt typ
cutoff _ (ALeaf t) (ADatatypeT id tArgs) = [t]
cutoff env (ANode _ lt rt) typ@(ADatatypeT {}) | isSubtypeOf typ (valueType lt) = cutoff env lt typ
cutoff env (ANode _ lt rt) typ@(ADatatypeT {}) | isSubtypeOf typ (valueType rt) = cutoff env rt typ
cutoff env semantic@(ANode t lt rt) typ@(ADatatypeT {}) | isSubtypeOf t typ = leafTypes semantic
cutoff env semantic (AFunctionT tArg tRet) =
    [ AFunctionT a r | a <- args, r <- rets, isJust (isConsistent (AFunctionT tArg tRet) (AFunctionT a r)) ]
  where
    args = cutoff env semantic tArg
    rets = cutoff env semantic tRet

    isMapConsistent m1 m2 = foldr (\(k,v) -> (&&) (Map.findWithDefault v k m2 == v)) True (Map.toList m1)
    isConsistent t t'@(ATypeVarT {}) = Just (Map.singleton t t')
    isConsistent t t'@(ADatatypeT {}) = Just (Map.singleton t t')
    isConsistent t t'@(AExclusion {}) = Just (Map.singleton t t')
    isConsistent (AFunctionT tArg tRet) (AFunctionT arg ret) =
        let argMap = isConsistent tArg arg
            retMap = isConsistent tRet ret
        in if isJust argMap && isJust retMap && isMapConsistent (fromJust argMap) (fromJust retMap)
              then Just (Map.union (fromJust argMap) (fromJust retMap))
              else Nothing
cutoff _ semantic t@(AExclusion _) = [rightmostType semantic]
cutoff _ semantic t = filter (isJust . abstractIntersection t) (leafTypes (closestTree semantic t))


abstractParamList :: AbstractSkeleton -> [AbstractSkeleton]
abstractParamList t@(ADatatypeT _ _) = [t]
abstractParamList t@(AExclusion _)   = [t]
abstractParamList (AFunctionT tArg tFun) =
    case tFun of
        ADatatypeT _ _  -> [tArg]
        AExclusion _    -> [tArg]
        ATypeVarT  _    -> [tArg]
        _               -> (tArg) : (abstractParamList tFun)
abstractParamList t = error $ "Unexpected type " ++ show t

lastAbstractType :: AbstractSkeleton -> AbstractSkeleton
lastAbstractType (AFunctionT tArg tFun) = lastAbstractType tFun
lastAbstractType t                      = t

fillAny arg = AExclusion Set.empty
