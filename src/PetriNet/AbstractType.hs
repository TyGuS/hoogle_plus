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

data AbstractSkeleton = 
      ADatatypeT Id [AbstractSkeleton] -- explicit datatypes
    | AExclusion (Set Id) -- not included datatypes
    | ATypeVarT Id -- type variable is only temporarily before building the PetriNet
    | AFunctionT AbstractSkeleton AbstractSkeleton
    deriving (Eq, Ord, Show, Generic)

type AbstractionSemantic = Map Id (Set Id)

isAFunctionT (AFunctionT {}) = True
isAFunctionT _ = False

withinSemantic :: AbstractionSemantic -> Id -> Id -> (Bool, Set Id)
withinSemantic semantic key id = (id `Set.member` currIds, currIds)
  where
    currIds        = Map.findWithDefault Set.empty key semantic

toAbstractType :: SType -> AbstractSkeleton
toAbstractType (ScalarT (TypeVarT _ id) _) = ATypeVarT id
toAbstractType (ScalarT (DatatypeT id tArgs _) _) = ADatatypeT id (map toAbstractType tArgs)
toAbstractType (FunctionT x tArg tRet) = AFunctionT (toAbstractType tArg) (toAbstractType tRet)

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
