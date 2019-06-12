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

isBot :: AbstractSkeleton -> Bool
isBot ABottom = True
isBot _ = False

isAFunctionT :: AbstractSkeleton -> Bool
isAFunctionT (AFunctionT {}) = True
isAFunctionT _ = False

isAHigherOrder :: AbstractSkeleton -> Bool
isAHigherOrder (AFunctionT tArg tRet) = isAFunctionT tArg || isAHigherOrder tRet
isAHigherOrder _ = False

lastAbstract :: AbstractSkeleton -> AbstractSkeleton
lastAbstract (AFunctionT _ tRet) = lastAbstract tRet
lastAbstract t = t

absFunArgs :: Id -> AbstractSkeleton -> [AbstractSkeleton]
absFunArgs id (AFunctionT tArg tRes) | id == "pair_match" = [tArg]
absFunArgs id (AFunctionT tArg tRes) = tArg : absFunArgs id tRes
absFunArgs _ t = []

decompose :: AbstractSkeleton -> [AbstractSkeleton]
decompose (AFunctionT tArg tRet) = decompose tArg ++ decompose tRet
decompose t@(AScalar {}) = [t]

decomposeHo :: AbstractSkeleton -> [AbstractSkeleton]
decomposeHo (AFunctionT tArg tRet) = tArg : decomposeHo tRet
decomposeHo t = [t]

toAbstractType :: SType -> AbstractSkeleton
toAbstractType (ScalarT (TypeVarT _ id) _) = AScalar (ATypeVarT id)
toAbstractType (ScalarT (DatatypeT id args _) _) = AScalar (ADatatypeT id (map toAbstractType args))
toAbstractType (FunctionT x tArg tRet) = AFunctionT (toAbstractType tArg) (toAbstractType tRet)
toAbstractType AnyT = AScalar (ATypeVarT varName)
toAbstractType BotT = ABottom

-- this is not subtype relation!!!
isSubtypeOf :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Bool
isSubtypeOf bound t1 t2 = isJust unifier
  where
    unifier = getUnifier (bound ++ abstractTypeVars bound t1) [(t2, t1)]

checkUnification :: [Id] -> Map Id AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton -> Maybe (Map Id AbstractSkeleton)
checkUnification bound tass t1 t2 | t1 == t2 = Just tass
checkUnification bound tass (AScalar (ATypeVarT id)) t | id `Map.member` tass =
    -- keep the most informative substitution, eagerly substitute into the final result
    case checkUnification bound tass assigned t of
        Nothing -> Nothing
        Just u -> Just (Map.insert id (substed u) tass)
  where
    substed m = abstractSubstitute m assigned
    assigned = fromJust (Map.lookup id tass)
checkUnification bound tass t@(AScalar (ATypeVarT id)) t'@(AScalar (ATypeVarT id')) 
  | id `elem` bound && id' `elem` bound = Nothing
  | id `elem` bound && id' `notElem` bound = checkUnification bound tass t' t
  | id `notElem` bound = Just (Map.insert id t' tass)
checkUnification bound tass (AScalar (ATypeVarT id)) t | id `elem` bound = Nothing
checkUnification bound tass t@(AScalar (ADatatypeT {})) t'@(AScalar (ATypeVarT id)) = checkUnification bound tass t' t
checkUnification bound tass (AScalar (ATypeVarT id)) t = Just (Map.insert id t tass)
checkUnification bound tass (AScalar (ADatatypeT id tArgs)) (AScalar (ADatatypeT id' tArgs')) | id /= id' = Nothing
checkUnification bound tass (AScalar (ADatatypeT id tArgs)) (AScalar (ADatatypeT id' tArgs')) | id == id' = checkArgs tass tArgs tArgs'
  where
    checkArgs m [] [] = Just m
    checkArgs m (arg:args) (arg':args') =
        case checkUnification bound m arg arg' of
            Nothing -> Nothing
            Just m' -> checkArgs m' args args'
checkUnification bound tass _ _ = Nothing

typeConstraints :: MonadIO m => [Id] -> AbstractSkeleton -> AbstractSkeleton -> PNSolver m [UnifConstraint]
typeConstraints bound (AFunctionT tArg tRet) (AFunctionT tArg' tRet') = do
    argCons <- typeConstraints bound tArg tArg'
    retCons <- if isAFunctionT tRet then typeConstraints bound tRet tRet' else return []
    return (argCons ++ retCons)
typeConstraints bound t1 t2 = do
    t2' <- freshAbstract bound t2
    return [(t1, t2')]

getUnifier :: [Id] -> [UnifConstraint] -> Maybe (Map Id AbstractSkeleton)
getUnifier bound cs = getUnifier' bound (Just Map.empty) cs

getUnifier' :: [Id] -> Maybe (Map Id AbstractSkeleton) -> [UnifConstraint] -> Maybe (Map Id AbstractSkeleton)
getUnifier' _ Nothing _ = Nothing
getUnifier' bound (Just tass) [] = Just tass
getUnifier' bound (Just tass) (c:cs) =
    case uncurry (checkUnification bound tass) c of
        Nothing -> Nothing
        Just m  -> getUnifier' bound (Just m) cs

abstractSubstitute :: Map Id AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
abstractSubstitute tass typ = 
    if substed /= typ then abstractSubstitute tass substed
                      else substed
  where
    substed = foldr (uncurry abstractSubstitute') typ (Map.toList tass)

abstractSubstitute' :: Id -> AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
abstractSubstitute' id typ (AScalar (ATypeVarT id')) | id == id' = typ
abstractSubstitute' id typ t@(AScalar (ATypeVarT {})) = t
abstractSubstitute' id typ (AScalar (ADatatypeT id' args)) = AScalar (ADatatypeT id' (map (abstractSubstitute' id typ) args))
abstractSubstitute' id typ (AFunctionT tArg tRes) = AFunctionT tArg' tRes'
  where
    tArg' = abstractSubstitute' id typ tArg
    tRes' = abstractSubstitute' id typ tRes

abstractTypeVars :: [Id] -> AbstractSkeleton -> [Id]
abstractTypeVars bound (AScalar (ATypeVarT id)) 
  | id `elem` bound = []
  | otherwise = [id]
abstractTypeVars bound (AScalar (ADatatypeT id args)) = concatMap (abstractTypeVars bound) args 
abstractTypeVars bound (AFunctionT tArg tRes) = abstractTypeVars bound tArg ++ abstractTypeVars bound tRes
abstractTypeVars bound _ = []

equalAbstract :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Bool
equalAbstract tvs t1 t2 = isSubtypeOf tvs t1 t2 && isSubtypeOf tvs t2 t1

equalSplit :: [Id] -> SplitMsg -> SplitMsg -> Bool
equalSplit tvs s1 s2 = fst s1 == fst s2 && equalAbstract tvs (snd s1) (snd s2)

existAbstract :: [Id] -> Set AbstractSkeleton -> AbstractSkeleton -> Bool
existAbstract tvs cover t = or $ map (equalAbstract tvs t) (Set.toList cover)

abstractIntersect :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Maybe AbstractSkeleton
abstractIntersect bound t1 t2 = 
    case unifier of
      Nothing -> Nothing
      Just u -> Just $ abstractSubstitute u t1 
  where
    unifier = getUnifier bound [(t1, t2)]

-- | find the current most restrictive abstraction for a given type
currentAbst :: MonadIO m => [Id] -> Set AbstractSkeleton -> AbstractSkeleton -> PNSolver m AbstractSkeleton
currentAbst tvs cover (AFunctionT tArg tRes) = do
    tArg' <- currentAbst tvs cover tArg
    tRes' <- currentAbst tvs cover tRes
    return $ AFunctionT tArg' tRes'
currentAbst tvs cover at = currentAbst' tvs (Set.toList cover) at (AScalar (ATypeVarT varName))

currentAbst' :: MonadIO m => [Id] -> [AbstractSkeleton] -> AbstractSkeleton -> AbstractSkeleton -> PNSolver m AbstractSkeleton
currentAbst' _ [] _ sofar = return sofar
currentAbst' tvs (t:ts) at sofar = do
  freshT <- freshAbstract tvs t
  freshSofar <- freshAbstract tvs sofar
  -- writeLog 3 $ pretty at <+> text "<:" <+> pretty freshT
  -- writeLog 3 $ pretty t <+> text "<:" <+> pretty freshSofar
  if isSubtypeOf tvs at freshT && isSubtypeOf tvs t freshSofar
     then currentAbst' tvs ts at t
     else currentAbst' tvs ts at sofar

applySemantic :: MonadIO m => [Id] -> AbstractSkeleton -> [AbstractSkeleton] -> PNSolver m AbstractSkeleton
applySemantic tvs fun args = do
    let cargs = init (decompose fun)
    let ret = last (decompose fun)
    constraints <- mapM (uncurry $ typeConstraints tvs) (zip cargs args)
    let unifier = getUnifier tvs (concat constraints)
    case unifier of
        Nothing -> return ABottom
        Just m -> do
            cover <- gets (view abstractionTree)
            let substRes = abstractSubstitute m ret
            currentAbst tvs cover substRes

compareAbstract :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Ordering
compareAbstract tvs t1 t2 = if isSubtypeOf tvs t1 t2 && isSubtypeOf tvs t2 t1 then EQ
                            else if isSubtypeOf tvs t1 t2 then LT else GT
