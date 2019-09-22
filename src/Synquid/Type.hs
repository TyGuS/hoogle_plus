{-# LANGUAGE FlexibleContexts #-}
-- | Refinement Types
module Synquid.Type where

import Types.Common hiding (varName)
import Types.Type
import Synquid.Logic
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


contextual x tDef (FunctionT y tArg tRes) = FunctionT y (contextual x tDef tArg) (contextual x tDef tRes)
contextual _ _ AnyT = AnyT

isScalarType (ScalarT _ _) = True
-- isScalarType (LetT _ _ t) = isScalarType t
isScalarType _ = False
baseTypeOf (ScalarT baseT _) = baseT
baseTypeOf _ = error "baseTypeOf: applied to a function type"
isFunctionType (FunctionT _ _ _) = True
-- isFunctionType (LetT _ _ t) = isFunctionType t
isFunctionType _ = False
argType (FunctionT _ t _) = t
resType (FunctionT _ _ t) = t
isHigherOrder (FunctionT _ tArg tRet) = isFunctionType tArg || isHigherOrder tRet
isHigherOrder _ = False

hasAny AnyT = True
hasAny (ScalarT baseT _) = baseHasAny baseT
  where
    baseHasAny (DatatypeT _ tArgs _) = any hasAny tArgs
    baseHasAny _ = False
hasAny (FunctionT _ tArg tRes) = hasAny tArg || hasAny tRes

-- | Convention to indicate "any datatype" (for synthesizing match scrtuinees)
anyDatatype = ScalarT (DatatypeT dontCare [] []) ftrue

toSort BoolT = BoolS
toSort IntT = IntS
toSort (DatatypeT name tArgs _) = DataS name (map (toSort . baseTypeOf) tArgs)
toSort (TypeVarT _ name) = VarS name

fromSort BoolS = ScalarT BoolT ftrue
fromSort IntS = ScalarT IntT ftrue
fromSort (VarS name) = ScalarT (TypeVarT Map.empty name) ftrue
fromSort (DataS name sArgs) = ScalarT (DatatypeT name (map fromSort sArgs) []) ftrue -- TODO: what to do with pArgs?
fromSort AnyS = AnyT

scalarName :: RType -> String
scalarName (ScalarT (DatatypeT name _ _) _) = name
scalarName (ScalarT IntT _) = "Int"
scalarName (ScalarT BoolT _) = "Bool"
scalarName (ScalarT (TypeVarT _ name) _) = name
scalarName t = error $ "scalarName error: cannot be applied to nonscalar type "

allDatatypes (FunctionT _ tArg tRet) = allDatatypes tArg `Set.union` allDatatypes tRet
allDatatypes (ScalarT (DatatypeT id tArgs _) _) = id `Set.insert` foldr (Set.union . allDatatypes) Set.empty tArgs
allDatatypes (ScalarT IntT _) = Set.singleton "Int"
allDatatypes (ScalarT BoolT _) = Set.singleton "Bool"
allDatatypes (ScalarT (TypeVarT _ id) _) = Set.empty

arity :: TypeSkeleton r -> Int
arity (FunctionT _ _ t) = 1 + arity t
arity _ = 0

lastType (FunctionT _ _ tRes) = lastType tRes
lastType t = t

allArgTypes (FunctionT x tArg tRes) = tArg : (allArgTypes tRes)
allArgTypes _ = []

allArgs (ScalarT _ _) = []
allArgs (FunctionT x (ScalarT baseT _) tRes) = (Var (toSort baseT) x) : (allArgs tRes)
allArgs (FunctionT x _ tRes) = (allArgs tRes)

allBaseTypes :: RType -> [RType]
allBaseTypes t@(ScalarT _ _) = [t]
allBaseTypes (FunctionT _ tArg tRet) = allBaseTypes tArg ++ allBaseTypes tRet
allBaseTypes _ = error "allBaseTypes: applied to unsupported types"

-- | Free variables of a type
varsOfType :: RType -> Set Id
varsOfType (ScalarT baseT fml) = varsOfBase baseT `Set.union` (Set.map varName $ varsOf fml)
  where
    varsOfBase (DatatypeT name tArgs pArgs) = Set.unions (map varsOfType tArgs) `Set.union` (Set.map varName $ Set.unions (map varsOf pArgs))
    varsOfBase _ = Set.empty
varsOfType (FunctionT x tArg tRes) = varsOfType tArg `Set.union` (Set.delete x $ varsOfType tRes)
varsOfType AnyT = Set.empty

-- | Free variables of a type
predsOfType :: RType -> Set Id
predsOfType (ScalarT baseT fml) = predsOfBase baseT `Set.union` predsOf fml
  where
    predsOfBase (DatatypeT name tArgs pArgs) = Set.unions (map predsOfType tArgs) `Set.union` (Set.unions (map predsOf pArgs))
    predsOfBase _ = Set.empty
predsOfType (FunctionT x tArg tRes) = predsOfType tArg `Set.union` predsOfType tRes
predsOfType AnyT = Set.empty

varRefinement x s = Var s valueVarName |=| Var s x
isVarRefinemnt (Binary Eq (Var _ v) (Var _ _)) = v == valueVarName
isVarRefinemnt _ = False

-- | Polymorphic type skeletons (parametrized by refinements)

toMonotype :: SchemaSkeleton r -> TypeSkeleton r
toMonotype (Monotype t) = t
toMonotype (ForallT _ t) = toMonotype t
toMonotype (ForallP _ t) = toMonotype t

boundVarsOf :: SchemaSkeleton r -> [Id]
boundVarsOf (ForallT a sch) = a : boundVarsOf sch
boundVarsOf _ = []

-- | Building types
bool = ScalarT BoolT
bool_ = bool ()
boolAll = bool ftrue

int = ScalarT IntT
int_ = int ()
intAll = int ftrue
nat = int (valInt |>=| IntLit 0)
pos = int (valInt |>| IntLit 0)

vart n = ScalarT (TypeVarT Map.empty n)
vart_ n = vart n ()
vartAll n = vart n ftrue

asSortSubst :: TypeSubstitution -> SortSubstitution
asSortSubst = Map.map (toSort . baseTypeOf)

-- typeSubstituteWore :: Map Id (TypeSkeleton r) -> TypeSkeleton r -> TypeSkeleton r
-- typeSubstituteWore subst t@(ScalarT (TypeVarT _ id) r) =
--   if id `Map.member` subst then fromJust $ Map.lookup id subst else t
-- typeSubstituteWore subst t@(ScalarT (DatatypeT name tArgs p) r) = ScalarT (DatatypeT name (map (typeSubstituteWore subst) tArgs) p) r
-- typeSubstituteWore subst t@(ScalarT _ _) = t
-- typeSubstituteWore subst (FunctionT x tArg tRes) = FunctionT x (typeSubstituteWore subst tArg) (typeSubstituteWore subst tRes)
-- typeSubstituteWore subst t = t

-- | 'typeSubstitute' @t@ : substitute all free type variables in @t@
typeSubstitute :: TypeSubstitution -> RType -> RType
typeSubstitute subst (ScalarT baseT r) = addRefinement substituteBase (sortSubstituteFml (asSortSubst subst) r)
  where
    substituteBase = case baseT of
      TypeVarT varSubst a -> case Map.lookup a subst of
        Just t -> substituteInType (not . (`Map.member` subst)) varSubst $ typeSubstitute subst t
        Nothing -> ScalarT (TypeVarT varSubst a) ftrue
      DatatypeT name tArgs pArgs ->
        let
          tArgs' = map (typeSubstitute subst) tArgs
          pArgs' = map (sortSubstituteFml (asSortSubst subst)) pArgs
        in ScalarT (DatatypeT name tArgs' pArgs') ftrue
      _ -> ScalarT baseT ftrue
typeSubstitute subst (FunctionT x tArg tRes) = FunctionT x (typeSubstitute subst tArg) (typeSubstitute subst tRes)
typeSubstitute _ t = t

noncaptureTypeSubst :: [Id] -> [RType] -> RType -> RType
noncaptureTypeSubst tVars tArgs t =
  let tFresh = typeSubstitute (Map.fromList $ zip tVars (map vartAll distinctTypeVars)) t
  in typeSubstitute (Map.fromList $ zip distinctTypeVars tArgs) tFresh

schemaSubstitute :: TypeSubstitution -> RSchema -> RSchema
schemaSubstitute tass (Monotype t) = Monotype $ typeSubstitute tass t
schemaSubstitute tass (ForallT a sch) = ForallT a $ schemaSubstitute (Map.delete a tass) sch
schemaSubstitute tass (ForallP sig sch) = ForallP sig $ schemaSubstitute tass sch

typeSubstitutePred :: Substitution -> RType -> RType
typeSubstitutePred pSubst t = let tsp = typeSubstitutePred pSubst
  in case t of
    ScalarT (DatatypeT name tArgs pArgs) fml -> ScalarT (DatatypeT name (map tsp tArgs) (map (substitutePredicate pSubst) pArgs)) (substitutePredicate pSubst fml)
    ScalarT baseT fml -> ScalarT baseT (substitutePredicate pSubst fml)
    FunctionT x tArg tRes -> FunctionT x (tsp tArg) (tsp tRes)
    AnyT -> AnyT

-- | 'typeVarsOf' @t@ : all type variables in @t@
typeVarsOf :: TypeSkeleton r -> Set Id
typeVarsOf t@(ScalarT baseT r) = case baseT of
  TypeVarT _ name -> Set.singleton name
  DatatypeT _ tArgs _ -> Set.unions (map typeVarsOf tArgs)
  _ -> Set.empty
typeVarsOf (FunctionT _ tArg tRes) = typeVarsOf tArg `Set.union` typeVarsOf tRes
typeVarsOf _ = Set.empty

{- Refinement types -}

-- | Forget refinements of a type
shape :: RType -> SType
shape (ScalarT (DatatypeT name tArgs pArgs) _) = ScalarT (DatatypeT name (map shape tArgs) (replicate (length pArgs) ())) ()
shape (ScalarT IntT _) = ScalarT IntT ()
shape (ScalarT BoolT _) = ScalarT BoolT ()
shape (ScalarT (TypeVarT _ a) _) = ScalarT (TypeVarT Map.empty a) ()
shape (FunctionT x tArg tFun) = FunctionT x (shape tArg) (shape tFun)
shape AnyT = AnyT
shape BotT = BotT

-- | Conjoin refinement to a type
addRefinement (ScalarT base fml) fml' = if isVarRefinemnt fml'
  then ScalarT base fml' -- the type of a polymorphic variable does not require any other refinements
  else ScalarT base (fml `andClean` fml')
addRefinement t (BoolLit True) = t
addRefinement AnyT _ = AnyT
addRefinement t _ = error $ "addRefinement: applied to function type"

-- | Conjoin refinement to the return type
addRefinementToLast t@(ScalarT _ _) fml = addRefinement t fml
addRefinementToLast (FunctionT x tArg tRes) fml = FunctionT x tArg (addRefinementToLast tRes fml)

-- | Conjoin refinement to the return type inside a schema
addRefinementToLastSch (Monotype t) fml = Monotype $ addRefinementToLast t fml
addRefinementToLastSch (ForallT a sch) fml = ForallT a $ addRefinementToLastSch sch fml
addRefinementToLastSch (ForallP sig sch) fml = ForallP sig $ addRefinementToLastSch sch fml

-- | Apply variable substitution in all formulas inside a type
substituteInType :: (Id -> Bool) -> Substitution -> RType -> RType
substituteInType isBound subst (ScalarT baseT fml) = ScalarT (substituteBase baseT) (substitute subst fml)
  where
    substituteBase (TypeVarT oldSubst a) = TypeVarT oldSubst a
      -- Looks like pending substitutions on types are not actually needed, since renamed variables are always out of scope
       -- if isBound a
          -- then TypeVarT oldSubst a
          -- else TypeVarT (oldSubst `composeSubstitutions` subst) a
    substituteBase (DatatypeT name tArgs pArgs) = DatatypeT name (map (substituteInType isBound subst) tArgs) (map (substitute subst) pArgs)
    substituteBase baseT = baseT
substituteInType isBound subst (FunctionT x tArg tRes) =
  if Map.member x subst
    then error $ unwords ["Attempt to substitute variable", x, "bound in a function type"]
    else FunctionT x (substituteInType isBound subst tArg) (substituteInType isBound subst tRes)
substituteInType isBound subst AnyT = AnyT

-- | 'renameVar' @old new t typ@: rename all occurrences of @old@ in @typ@ into @new@ of type @t@
renameVar :: (Id -> Bool) -> Id -> Id -> RType -> RType -> RType
renameVar isBound old new (ScalarT b _)     t = substituteInType isBound (Map.singleton old (Var (toSort b) new)) t
renameVar _ _ _ _                           t = t -- function arguments cannot occur in types (and AnyT is assumed to be function)

-- | Intersection of two types (assuming the types were already checked for consistency)
intersection _ t AnyT = t
intersection _ AnyT t = t
intersection isBound (ScalarT baseT fml) (ScalarT baseT' fml') = case baseT of
  DatatypeT name tArgs pArgs -> let DatatypeT _ tArgs' pArgs' = baseT' in
                                  ScalarT (DatatypeT name (zipWith (intersection isBound) tArgs tArgs') (zipWith andClean pArgs pArgs')) (fml `andClean` fml')
  _ -> ScalarT baseT (fml `andClean` fml')
intersection isBound (FunctionT x tArg tRes) (FunctionT y tArg' tRes') = FunctionT x tArg (intersection isBound tRes (renameVar isBound y x tArg tRes'))
-- intersection _ t1 t2 = error $ "cannot intersection between" ++ show (shape t1) ++ " and " ++ show (shape t2)

-- | Instantiate unknowns in a type
typeApplySolution :: Solution -> RType -> RType
typeApplySolution sol (ScalarT (DatatypeT name tArgs pArgs) fml) = ScalarT (DatatypeT name (map (typeApplySolution sol) tArgs) (map (applySolution sol) pArgs)) (applySolution sol fml)
typeApplySolution sol (ScalarT base fml) = ScalarT base (applySolution sol fml)
typeApplySolution sol (FunctionT x tArg tRes) = FunctionT x (typeApplySolution sol tArg) (typeApplySolution sol tRes)
typeApplySolution _ AnyT = AnyT

typeDepth :: RType -> Int
typeDepth (ScalarT (DatatypeT _ tys _) _) | length tys == 0 = 0
typeDepth (ScalarT (DatatypeT _ tys _) _) | otherwise       = 1 + (maximum $ map typeDepth tys)
typeDepth (ScalarT _ _) = 0
typeDepth (FunctionT _ tArg tRet) = max (typeDepth tArg) (typeDepth tRet)
typeDepth t = error $ "typeDepth: I have no idea when I come across this type"

longScalarName :: RType -> String
longScalarName (ScalarT (DatatypeT name rs _) _) = name ++ (concatMap longScalarName rs)
longScalarName (ScalarT IntT _) = "Int"
longScalarName (ScalarT BoolT _) = "Bool"
longScalarName (ScalarT (TypeVarT _ name) _) = name
longScalarName t = error $ "longScalarName error: cannot be applied to nonscalar type "

toFunDts :: RType -> RType
toFunDts (ScalarT (DatatypeT id args p) r) = ScalarT (DatatypeT id (map toFunDts args) p) r
toFunDts (FunctionT _ tArg tRes) = ScalarT (DatatypeT "Fun" [toFunDts tArg, toFunDts tRes] []) ftrue
toFunDts t = t