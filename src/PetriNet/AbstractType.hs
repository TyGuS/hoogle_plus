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
import Debug.Trace

data AbstractSkeleton =
      ADatatypeT Id [AbstractSkeleton] -- explicit datatypes
    | AExclusion (Set Id) -- not included datatypes
    | AOneOf (Set Id) -- one of these datatypes
    | ATypeVarT Id -- type variable is only temporarily before building the PetriNet
    | AFunctionT AbstractSkeleton AbstractSkeleton
    deriving (Eq, Ord, Show, Generic)

type AbstractionSemantic = Map Id (Set (Either Id (Set Id)))
-- type AbstractSemantic = Map Id (Set Id)

isAFunctionT (AFunctionT {}) = True
isAFunctionT _ = False

withinSemantic :: AbstractionSemantic -> Id -> Id -> (Bool, Set Id)
withinSemantic semantic key id = (id `Set.member` possibleIds, possibleIds)
  where
    currIds        = Map.findWithDefault Set.empty key semantic
    unionEither id = Set.union (if isLeft id then Set.singleton (fromLeft id) else fromRight id)
    possibleIds    = Set.foldr unionEither Set.empty currIds


-- Abstract a type into its skeleton
abstract :: [Id] -> AbstractionSemantic -> Id -> SType -> AbstractSkeleton
abstract bound semantic key (ScalarT tyapp@(TypeAppT tCon tArg) fml) = let
    (tcon, args) = typeAppToArgs tyapp
    in case tcon of
        TypeConT id -> let
            (inSeman, allIds) = withinSemantic semantic key id
            in if inSeman
                then ADatatypeT id (map (abstract bound semantic (key ++ "," ++ id)) args)
                else AExclusion allIds
        tyvar@(TypeVarT m id) -> Debug.Trace.trace ("abstracting higher kinded type variable: " ++ id) (
            abstract bound semantic key (ScalarT tyvar fml))
        _ -> error "the impossible has happened"
abstract bound semantic key (ScalarT (TypeConT id) _) | not (key `Map.member` semantic) = AExclusion Set.empty
abstract bound semantic key (ScalarT (TypeConT id) _) = ADatatypeT id []
abstract bound semantic key (ScalarT BoolT _) = abstract bound semantic key (ScalarT (TypeConT "Bool") ())
abstract bound semantic key (ScalarT IntT _) = abstract bound semantic key (ScalarT (TypeConT "Int") ())
abstract bound semantic key (ScalarT (TypeVarT _ id) _) | id `elem` bound || key /= "" =
    if inSeman then ATypeVarT id
               else AExclusion allIds
  where
    (inSeman, allIds) = withinSemantic semantic key id
abstract bound semantic key (ScalarT (TypeVarT _ id) _) = ATypeVarT id
abstract bound semantic key (FunctionT x tArg tRet) = AFunctionT (abstract bound semantic key tArg) (abstract bound semantic key tRet)

outerName :: AbstractSkeleton -> Either (Set Id) (Set Id)
outerName (ADatatypeT id _) = Left (Set.singleton id)
outerName (ATypeVarT id) = Right Set.empty
outerName (AExclusion names) = Right names
outerName (AOneOf names) = Left names

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
abstractSubstitute bound id bt t@(AOneOf _)           = t
