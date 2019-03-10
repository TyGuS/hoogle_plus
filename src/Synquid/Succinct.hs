{-# LANGUAGE DeriveGeneric  #-}

module Synquid.Succinct (
  SuccinctType (..),
  extractSuccinctTyVars,
  hasSuccinctAny,
  isSuccinctAll,
  isSuccinctComposite,
  isSuccinctConcrete,
  isSuccinctFunction,
  isSuccinctInhabited,
  lastSuccinctType,
  mergeMapOfSet,
  outOfSuccinctAll,
  succinctAnyEq,
  succinctTypeSubstitute,
  toSuccinctType,
  unifySuccinct
  ) where

import Synquid.Type hiding (set)
import Synquid.Util
import Synquid.Logic

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Serialize
import Data.List
import Data.Maybe
import Control.Lens
import GHC.Generics

data SuccinctType =
  SuccinctScalar (BaseType Formula) |
  SuccinctFunction Int (Set SuccinctType) SuccinctType | -- # of params -> param set -> return type
  SuccinctDatatype (Id, Int) (Set (Id, Int)) (Set SuccinctType) (Map Id Id) (Set Id) | -- Outmost datatype, Datatype names, included types, datatype constructors, (datatype measures, vars in refinements)
  SuccinctAll (Set Id) SuccinctType | -- type variables, type
  SuccinctComposite (Set SuccinctType) | -- composite type nodes for the function parameters
  SuccinctLet Id SuccinctType SuccinctType | -- actually, this can be removed, [TODO] safely check this type
  SuccinctAny | -- any type
  SuccinctInhabited SuccinctType -- inhabited type node, only work in the graph
  deriving (Eq, Ord, Generic)

type SuccinctParams = [SuccinctType]

baseToSuccinctType :: BaseType Formula -> SuccinctType
baseToSuccinctType typ@(DatatypeT id ts _) = if "_" == id
  then SuccinctAny
  else SuccinctDatatype (id, length ts) resIds resTys Map.empty Set.empty
  where
    mergeDt t (accIds, accTys) = case outOfSuccinctAll (toSuccinctType t) of
      SuccinctDatatype id' ids tys _ _ -> (Set.insert id' (ids `Set.union` accIds), tys `Set.union` accTys)
      ty                         -> (accIds, Set.singleton ty `Set.union` accTys)
    (resIds, resTys) = foldr mergeDt (Set.empty, Set.empty) ts
baseToSuccinctType t = SuccinctScalar t

toSuccinctType :: RType -> SuccinctType
toSuccinctType t@(ScalarT bt _) = let
  vars = extractSTyVars t
  ty = baseToSuccinctType bt
  in if Set.size vars == 0 then ty else simplifySuccinctType $ SuccinctAll vars ty
toSuccinctType t@(FunctionT _ param ret) = let
  vars = extractSTyVars t
  ty = case outOfSuccinctAll (toSuccinctType ret) of
    SuccinctFunction paramCnt paramSet retTy -> SuccinctFunction (paramCnt+1) (Set.insert (toSuccinctType param) paramSet) retTy
    ty'                                      -> SuccinctFunction 1 (Set.singleton (toSuccinctType param)) ty'
  in if Set.size vars == 0 then ty else simplifySuccinctType $ SuccinctAll vars ty
toSuccinctType t@(LetT id varty bodyty) = let
  vars = extractSTyVars t
  ty = toSuccinctType bodyty
  in if Set.size vars == 0 then ty else simplifySuccinctType $ SuccinctAll vars ty
toSuccinctType AnyT = SuccinctAny

lastSuccinctType :: SuccinctType -> SuccinctType
lastSuccinctType (SuccinctFunction _ _ retTy) = retTy
lastSuccinctType (SuccinctLet _ _ typ) = typ
lastSuccinctType (SuccinctAll ids typ) = SuccinctAll ids (lastSuccinctType typ)
lastSuccinctType ty = ty

extractBaseTyVars :: BaseType Formula -> Set Id
extractBaseTyVars (TypeVarT _ id) = Set.singleton id
extractBaseTyVars (DatatypeT id ts _) = foldr (\t acc -> acc `Set.union` (extractSTyVars t)) Set.empty ts
extractBaseTyVars _ = Set.empty

extractSTyVars :: RType -> Set Id
extractSTyVars (ScalarT bt _) = extractBaseTyVars bt
extractSTyVars (FunctionT _ param ret) = (extractSTyVars param) `Set.union` (extractSTyVars ret)
extractSTyVars _ = Set.empty

extractSuccinctTyVars :: SuccinctType -> Set Id
extractSuccinctTyVars (SuccinctScalar t) = extractBaseTyVars t
extractSuccinctTyVars (SuccinctFunction _ args ret) = Set.foldr (\t acc -> (extractSuccinctTyVars t) `Set.union` acc) (extractSuccinctTyVars ret) args
extractSuccinctTyVars (SuccinctDatatype _ _ tys _ _) = foldr (\t acc -> (extractSuccinctTyVars t) `Set.union` acc) Set.empty tys
extractSuccinctTyVars (SuccinctAll ids _) = ids
extractSuccinctTyVars (SuccinctComposite tys) = foldr (\t acc -> (extractSuccinctTyVars t) `Set.union` acc) Set.empty tys
extractSuccinctTyVars _ = Set.empty

outOfSuccinctAll :: SuccinctType -> SuccinctType
outOfSuccinctAll (SuccinctFunction paramCnt paramSet ret) = SuccinctFunction paramCnt (Set.map outOfSuccinctAll paramSet) (outOfSuccinctAll ret)
outOfSuccinctAll (SuccinctDatatype id ids tys cons measures) = SuccinctDatatype id ids (Set.map outOfSuccinctAll tys) cons measures
outOfSuccinctAll (SuccinctAll ids ty) = ty
outOfSuccinctAll ty = ty

simplifySuccinctType :: SuccinctType -> SuccinctType
simplifySuccinctType t@(SuccinctFunction paramCnt paramSet ret) = case ret of
  SuccinctFunction pc ps rt -> SuccinctFunction (paramCnt + pc) (paramSet `Set.union` ps) rt
  _ -> t
simplifySuccinctType t@(SuccinctDatatype idIn idsIn tysIn consIn measuresIn) = let
  fold_fun ty (accIds, accTys, accCons) = case ty of
    SuccinctDatatype id ids tys cons _ -> (if id /= ("_",0)  then Set.insert id (ids `Set.union` accIds) else ids `Set.union` accIds, tys `Set.union` accTys, accCons  `Map.union` cons)
    _ -> (accIds, Set.insert ty accTys, accCons)
  (ids, tys, cons) = Set.foldr fold_fun (idsIn, Set.empty, consIn) (Set.map simplifySuccinctType tysIn)
  (names,_) = unzip (Set.toList ids)
  cons' = Map.filterWithKey (\k v -> elem k names) cons
  in SuccinctDatatype idIn ids tys cons' measuresIn
simplifySuccinctType t@(SuccinctAll ids ty) = case simplifySuccinctType ty of
  SuccinctAll ids' ty' -> SuccinctAll (ids `Set.union` ids') (outOfSuccinctAll ty')
  _ -> SuccinctAll ids (outOfSuccinctAll ty)
simplifySuccinctType t@(SuccinctComposite tys) = SuccinctComposite (Set.map simplifySuccinctType tys)
simplifySuccinctType t@(SuccinctInhabited ty) = SuccinctInhabited (simplifySuccinctType ty)
simplifySuccinctType t = t

type SuccTypeSubstitution = Map Id SuccinctType

succinctTypeSubstitute :: SuccTypeSubstitution -> SuccinctType -> SuccinctType
succinctTypeSubstitute subst (SuccinctScalar baseT) = case baseT of
  TypeVarT _ a -> case Map.lookup a subst of
    Just t -> simplifySuccinctType $ succinctTypeSubstitute subst t
    Nothing -> SuccinctScalar baseT
  _ -> SuccinctScalar baseT
succinctTypeSubstitute subst (SuccinctFunction paramCnt tArgs tRes) = simplifySuccinctType $ SuccinctFunction paramCnt tArgs' tRes'
  where
    tArgs' = Set.map (succinctTypeSubstitute subst) tArgs
    tRes' = succinctTypeSubstitute subst tRes
succinctTypeSubstitute subst (SuccinctDatatype id idSet tySet constructors measures) = simplifySuccinctType $ SuccinctDatatype id idSet tySet' constructors measures
  where
    tySet' = Set.map (succinctTypeSubstitute subst) tySet
succinctTypeSubstitute subst (SuccinctAll idSet ty) = simplifySuccinctType $ SuccinctAll idSet' ty'
  where
    idSet' = Set.filter (\id -> not $ isJust (Map.lookup id subst)) idSet
    ty' = succinctTypeSubstitute subst ty
succinctTypeSubstitute subst (SuccinctComposite tySet) = simplifySuccinctType $ SuccinctComposite tySet'
  where
    tySet' = Set.map (succinctTypeSubstitute subst) tySet
succinctTypeSubstitute subst (SuccinctLet id tDef tBody) = simplifySuccinctType $ SuccinctLet id tDef' tBody'
  where
    tDef' = succinctTypeSubstitute subst tDef
    tBody' = succinctTypeSubstitute subst tBody
succinctTypeSubstitute subst SuccinctAny = SuccinctAny
succinctTypeSubstitute subst (SuccinctInhabited t) = simplifySuccinctType $ SuccinctInhabited (succinctTypeSubstitute subst t)

unifySuccinct :: SuccinctType -> SuccinctType -> [Id] -> (Bool, [SuccTypeSubstitution])
unifySuccinct comp target boundedTys = case (comp, target) of
  (SuccinctScalar (TypeVarT _ id), target) -> if id `elem` boundedTys
    then if comp == target
      then (True, [Map.empty])
      else (False, [Map.empty])
    else (True, [Map.singleton id target])
  (SuccinctScalar t1, SuccinctScalar t2) -> (t1 == t2, [Map.empty])
  (SuccinctDatatype id1 idSet1 tySet1 consMap1 measures1, SuccinctDatatype id2 idSet2 tySet2 consMap2 measures2) ->
    -- if Set.size idSet1 > 5 || Set.size idSet2 > 5
    --   then (False, [Map.empty]) -- [TODO] return anyT here
      -- else if idSet1 `Set.isSubsetOf` idSet2
      if idSet1 `Set.isSubsetOf` idSet2
        then
          let
            isTyVar ty = case ty of
              SuccinctScalar (TypeVarT _ _) -> True
              _                             -> False
            getTyVar (SuccinctScalar (TypeVarT _ id)) = id
            isBound tv = tv `elem` boundedTys -- [TODO] is the bounded value checked correctly?
            -- bound1 = tySet1
            bound1 = (Set.filter (not . isTyVar) tySet1) `Set.union` (Set.filter (isBound . getTyVar) (Set.filter isTyVar tySet1))
            -- bound2 = (Set.filter (not . isTyVar) tySet2) `Set.union` (Set.filter (isBound . getTyVar) (Set.filter isTyVar tySet2))
            bound2 = tySet2
          in
            if bound1 `Set.isSubsetOf` bound2
              then
                let consMapDiff = Map.intersection consMap1 consMap2
                    isConsMatch = Map.null consMapDiff
                in if isConsMatch && id1 == id2
                  then
                    let
                      consDiff = idSet2 `Set.difference` idSet1
                      tyDiff = bound2 `Set.difference` bound1
                      freeVt = tySet1 `Set.difference` bound1
                      optCons = idSet2 `Set.intersection` idSet1
                      optTy = tySet1 `Set.intersection` tySet2
                      anyFreeMap = Set.foldr (\(SuccinctScalar (TypeVarT _ t)) accMap -> Map.insert t SuccinctAny accMap) Map.empty freeVt
                    in if Set.size freeVt > 3 || Set.size bound2 > 5
                      then (True, [anyFreeMap])
                      else (True, allCombos consDiff tyDiff freeVt optCons optTy (Map.union consMap1 consMap2) id2 measures2)
                  else (False, [Map.empty])
              else (False, [Map.empty])
        else (False, [Map.empty])
  _ -> (False, [Map.empty])
  where
    powerset s =
      if s == Set.empty
        then Set.singleton Set.empty
        else Set.map (Set.insert x) pxs `Set.union` pxs
          where (x, xs) = Set.deleteFindMin s
                pxs = powerset xs

    distribute :: (Ord a) => Int -> Set a -> [[Set a]]
    distribute 1 elements = [[elements]]
    distribute n elements =
      let pset = powerset elements
          allRemain s = Set.toList $ Set.filter ((elements `Set.difference` s) `Set.isSubsetOf`) pset
          mergeRemain s ss acc = acc ++ (map ((:) s) (distribute (n-1) ss))
      in Set.foldr (\s acc -> acc ++ (foldr (mergeRemain s) [] (allRemain s))) [] pset

    allCombos :: Set (Id,Int) -> Set SuccinctType -> Set SuccinctType -> Set (Id,Int) -> Set SuccinctType -> Map Id Id -> (Id, Int) -> Set Id -> [SuccTypeSubstitution]
    allCombos cons tys freeVars tcons tty consMaps outerId measures =
      if length freeVars == 0 -- (length cons /= 0 || length tys /= 0) && (length freeVars == 0) -- no freeVars to fill
        then [Map.empty]
        else
          let mustTys = distribute (Set.size freeVars) tys
              mustCons = distribute (Set.size freeVars) cons
              -- optTy = Set.toList $ powerset tty
              -- optCon = Set.toList $ powerset tcons
              optTys = replicate (Set.size freeVars) $ Set.toList $ powerset tty
              optCons = replicate (Set.size freeVars) $ Set.toList $ powerset tcons
              optAssign [] = [[]]
              optAssign (t:ts) = [x:y | x <- t, y <- optAssign ts]
              combine x y = map (\(a,b) -> a `Set.union` b) (zip x y)
              finalTys = [combine x y | x <- mustTys, y <- optAssign optTys]
              finalCons = [combine x y | x <- mustCons, y <- optAssign optCons]
              assign vars x y includeOut = case (vars, x, y) of
                ((SuccinctScalar (TypeVarT _ id)):vs, xx:xs, yy:ys) -> if Set.null xx
                  then if Set.size yy > 0
                    then Map.insert id (Set.findMin yy) (assign vs xs ys includeOut) -- [TODO] the include out also need to be permutated
                    else Map.empty
                  else Set.foldr (\out acc ->
                    let (_, outPlace) = out in
                      if outPlace > 0 || (outPlace == 0 && length (if includeOut then xx else Set.delete out xx) == 0)
                        then Map.insert id (if includeOut then (SuccinctDatatype out xx yy consMaps Set.empty) else (SuccinctDatatype out (Set.delete out xx) yy consMaps Set.empty)) acc
                        else acc) (assign vs xs ys includeOut) xx
                _ -> Map.empty
              isValid x y = if Set.null x
                then Set.size y == 1
                else let cnt = Set.foldr (\(_,n) acc ->if n>acc then n else acc) 0 x
                         len = (Set.size y) + (Set.foldr (\(_,n) acc ->if n==0 then acc+1 else acc) 0 x)
                    -- in (cnt > 1 && len >= 1) || (cnt == 0 && (len == 0 || len == 1)) || (cnt == 1 && len == 1)
                     in (cnt > 1 && len >= 1) || (cnt == 0 && (len == 0 || len == 1)) || (cnt == 1 && len == 1)
              -- resultMap = filter (not . Map.null) ([assign (Set.toList freeVars) c t False | c <- finalCons, t <- finalTys,
              --                                                 (foldr (\(x,y) acc -> acc && (isValid x y)) True (zip c t))]
              --                                   ++ [assign (Set.toList freeVars) c t True | c <- finalCons, t <- finalTys,
              --                                                 (foldr (\(x,y) acc -> acc && (isValid x y)) True (zip c t))])
              resultMap = [ m | c <- finalCons, t <- finalTys, let m = assign (Set.toList freeVars) c t False,
                                      (foldr (\(x,y) acc -> acc && (isValid x y)) (not (Map.null m)) (zip c t))]
                        ++ [ m | c <- finalCons, t <- finalTys,let m = assign (Set.toList freeVars) c t True,
                                      (foldr (\(x,y) acc -> acc && (isValid x y)) (not (Map.null m)) (zip c t))]
          in resultMap

allSuccinctIndices :: Map SuccinctType Int -> Set Int
allSuccinctIndices nodesMap = Set.fromList $ Map.elems nodesMap

mergeMapOfSet :: (Hashable a, Ord a, Ord b) => HashMap a (Set b) -> HashMap a (Set b) -> HashMap a (Set b)
mergeMapOfSet new old = HashMap.foldrWithKey fold_fun old new
  where
    fold_fun kty idSet accMap = HashMap.insert kty ((HashMap.lookupDefault Set.empty kty accMap) `Set.union` idSet) accMap

isSuccinctInhabited ty@(SuccinctInhabited _) = True
isSuccinctInhabited ty = False

isSuccinctFunction ty@(SuccinctFunction _ _ _) = True
isSuccinctFunction ty = False

isSuccinctComposite ty@(SuccinctComposite _) = True
isSuccinctComposite ty = False

isSuccinctAll (SuccinctAll _ _) = True
isSuccinctAll _ = False

isSuccinctConcrete (SuccinctInhabited _) = False
isSuccinctConcrete (SuccinctComposite _) = False
isSuccinctConcrete (SuccinctFunction _ _ _) = False
isSuccinctConcrete (SuccinctAny) = False
isSuccinctConcrete (SuccinctDatatype _ _ tys _ _) = Set.foldr (\t acc -> acc && isSuccinctConcrete t) True tys
isSuccinctConcrete _ = True

hasSuccinctAny (SuccinctComposite tys) = Set.foldr (\t acc -> acc || hasSuccinctAny t) False tys
hasSuccinctAny (SuccinctFunction _ targ tret) = (hasSuccinctAny (SuccinctComposite targ)) || (hasSuccinctAny tret)
hasSuccinctAny (SuccinctAny) = True
hasSuccinctAny (SuccinctDatatype _ _ tys _ _) = hasSuccinctAny (SuccinctComposite tys)
hasSuccinctAny _ = False

hasSuccinctFunction (SuccinctComposite tys) = Set.foldr (\t acc -> acc || isSuccinctFunction t) False tys
hasSuccinctFunction ty = error "judge has higher order function from non-composite nodes"

-- does the first type make contribution to the second one's refinement
contributeTo measures (SuccinctDatatype id1 ids1 tys1 cons1 measures1) (SuccinctDatatype id2 ids2 tys2 cons2 measures2) =
  Set.null $ (measures2) `Set.intersection` (measures1)
contributeTo _ sty2 SuccinctAny = True
contributeTo _ sty1 sty2 = sty1 == sty2

isStrongerThan (SuccinctDatatype id1 ids1 tys1 cons1 measures1) (SuccinctDatatype id2 ids2 tys2 cons2 measures2) =
  tys1 == tys2 && id1 == id2 && ids1 == ids2 && (measures2) `Set.isSubsetOf` (measures1)
isStrongerThan SuccinctAny sty2 = True
isStrongerThan sty1 sty2 = sty1 == sty2

measuresOf (SuccinctDatatype _ _ _ _ measures) = measures
measuresOf _ = Set.empty

-- | can typ2 be kind of typ1?
succinctAnyEq :: SuccinctType -> SuccinctType -> Bool
succinctAnyEq (SuccinctScalar t1) (SuccinctScalar t2) = t1 == t2
succinctAnyEq (SuccinctFunction cnt1 targ1 tret1) (SuccinctFunction cnt2 targ2 tret2) = cnt1 == cnt2 && (succinctAnyEq (SuccinctComposite targ1) (SuccinctComposite targ2)) && (tret1 == tret2 || (tret1 == SuccinctAny) || (tret2 == SuccinctAny))
succinctAnyEq t1@(SuccinctDatatype id1 ids1 tys1 cons1 measures1) t2@(SuccinctDatatype id2 ids2 tys2 cons2 measures2) =
  if hasSuccinctAny t1 || hasSuccinctAny t2
    then (succinctAnyEq (SuccinctComposite tys1) (SuccinctComposite tys2)) && (Set.isSubsetOf ids1 ids2 || Set.isSubsetOf ids2 ids1) && id1 == id2 && ((not . Set.null $ measures1 `Set.intersection` measures2)  || Set.null measures1)
    else tys1 == tys2 && id1 == id2 && ids1 == ids2 && (Map.null cons1 || Map.null cons2 || cons1 == cons2) && ((not . Set.null $ measures1 `Set.intersection` measures2) || Set.null measures1)
succinctAnyEq (SuccinctComposite tys1) (SuccinctComposite tys2) =
  if Set.member SuccinctAny tys1
    then let diff = tys1 `Set.difference` tys2 in Set.size diff == 0 || (Set.size diff == 1 && Set.findMin diff == SuccinctAny)
    else let diff = tys2 `Set.difference` tys1 in Set.size diff == 0 || (Set.size diff == 1 && Set.findMin diff == SuccinctAny)
succinctAnyEq SuccinctAny _ = True
succinctAnyEq _ SuccinctAny = True
succinctAnyEq _ _ = False

base2str :: BaseType Formula -> String
base2str IntT = "Int"
base2str BoolT = "Bool"
base2str (TypeVarT _ id) = id
base2str _ = ""

sizeof :: SuccinctType -> Int
sizeof (SuccinctComposite tySet) = Set.foldr (\x -> (+) (sizeof x)) 0 tySet
sizeof (SuccinctFunction _ argSet _) = sizeof (SuccinctComposite argSet)
sizeof (SuccinctDatatype id ids tys cons _) = 1 + (Set.size ids) + (Set.size tys)
sizeof (SuccinctInhabited _) = 0
sizeof _ = 1
