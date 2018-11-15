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
import Data.List.Extra (nubOrd)
import qualified Data.Set as Set
import Data.Aeson
import Data.Maybe
import Data.List

data AbstractSkeleton =
      ADatatypeT Id [AbstractSkeleton] -- explicit datatypes
    | AExclusion (Set Id) -- not included datatypes
    | ATypeVarT Id -- type variable is only temporarily before building the PetriNet
    | AFunctionT AbstractSkeleton AbstractSkeleton
    deriving (Eq, Ord, Show, Generic)

abstract :: [Id] -> Map Id (Set Id) -> Id -> SType -> AbstractSkeleton
-- abstract bound level key (ScalarT (DatatypeT id tArgs _) _) | key `Map.member` level =
    -- let currIds = fromJust $ Map.lookup key level
    -- in if id `Set.member` currIds then ADatatypeT id (nubOrd $ map (abstract bound level (key ++ "," ++ id)) tArgs)
                                  -- else AExclusion currIds
-- abstract bound level key (ScalarT (DatatypeT id tArgs _) _) = AExclusion Set.empty
-- abstract bound level key (ScalarT BoolT _) = abstract bound level key (ScalarT (DatatypeT "Bool" [] []) ())
-- abstract bound level key (ScalarT IntT _) = abstract bound level key (ScalarT (DatatypeT "Int"  [] []) ())
abstract bound level key (ScalarT (TypeVarT _ id) _) | id `elem` bound =
    if key `Map.member` level then let currIds = fromJust $ Map.lookup key level
                                    in if id `Set.member` currIds then ATypeVarT id
                                                                  else AExclusion currIds
                              else AExclusion Set.empty
abstract bound level key (ScalarT (TypeVarT _ id) _) = ATypeVarT id
abstract bound level key (FunctionT x tArg tRet) = AFunctionT (abstract bound level key tArg) (abstract bound level key tRet)

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
