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
import Synquid.Logic (ftrue)
import Types.Solver
import Synquid.Pretty
import PetriNet.Util

import Data.Maybe
import Data.List
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Lens
import qualified Data.Char as Char
import Text.Printf
import Control.Monad.State

firstLvAbs :: Environment -> [RSchema] -> AbstractionTree
firstLvAbs env schs =
    Set.foldl' (updateSemantic env) (ALeaf Set.empty) dts
  where
    typs = map (shape . toMonotype) schs
    dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars)) typs)

allAbstractDts :: [Id] -> SType -> Set Abstraction
allAbstractDts bound t@(ScalarT (TypeVarT _ v) _) | v `elem` bound = Set.singleton $ Set.singleton (TypeShape (ScalarT (TypeVarT Map.empty varName) ()) t)
allAbstractDts bound t@(ScalarT (DatatypeT {}) _) = Set.singleton $ Set.singleton (TypeShape (ScalarT (TypeVarT Map.empty varName) ()) t)
allAbstractDts bound (FunctionT _ tArg tRes) = allAbstractDts bound tArg `Set.union` allAbstractDts bound tRes
allAbstractDts _ _ = Set.empty

-- Produce the most specific abstraction possible from the given types.
specificAbstractionFromTypes :: Environment -> [RSchema] -> AbstractionTree
specificAbstractionFromTypes env schemas = let
    abstrSkels = concatMap (decompose . toAbstractType . shape . toMonotype) schemas
    baseTree = ALeaf Set.empty
    in
        foldr (flip $ updateSemantic env) baseTree abstrSkels

updateSemantic :: Environment -> AbstractionTree -> Abstraction -> AbstractionTree
updateSemantic env (ALeaf t) at | isSubtypeOf (env ^. boundTypeVars) at t = 
    ANode t (ALeaf at') (ALeaf (t `Set.union` negations))
  where
    sameShape (TypeShape _ t1) (TypeShape _ t2) = sameType (env ^. boundTypeVars) t1 t2
    sameShape (NotShape _ t1) (NotShape _ t2) = sameType (env ^. boundTypeVars) t1 t2
    sameShape _ _ = False

    sameType bound (ScalarT (TypeVarT _ id1) _) (ScalarT (TypeVarT _ id2) _) | id1 `notElem` bound && id2 `notElem` bound = True
    sameType bound (ScalarT (TypeVarT _ id1) _) (ScalarT (TypeVarT _ id2) _) | id1 == id2 = True
    sameType bound (ScalarT (DatatypeT id1 tys1 _) _) (ScalarT (DatatypeT id2 tys2 _) _) | id1 == id2 = and (map (uncurry (sameType bound)) (zip tys1 tys2))
    sameType bound (FunctionT _ tArg tRes) (FunctionT _ tArg' tRes') = sameType bound tArg tArg' && sameType bound tRes tRes'
    sameType _ _ _ = False

    at' = simplify (env ^. boundTypeVars) at
    common = Set.filter (\c -> not (Set.null (Set.filter (sameShape c) t))) at'
    special = Set.difference at' common
    negations = Set.map (\(TypeShape v t1) -> NotShape v t1) special

updateSemantic env (ANode t l r) at | isSubtypeOf (env ^. boundTypeVars) at t = 
    ANode t (updateSemantic env l at) (updateSemantic env r at)
updateSemantic env tree at = tree

abstractParamList :: AbstractSkeleton -> [AbstractSkeleton]
abstractParamList t@(AScalar {}) = [t]
abstractParamList (AFunctionT tArg tFun) =
    case tFun of
        AScalar _  -> [tArg]
        _          -> (tArg) : (abstractParamList tFun)

lastAbstractType :: AbstractSkeleton -> AbstractSkeleton
lastAbstractType (AFunctionT tArg tFun) = lastAbstractType tFun
lastAbstractType t                      = t

