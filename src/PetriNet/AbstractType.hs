{-# LANGUAGE FlexibleContexts #-}

module PetriNet.AbstractType where

import Types.Abstract
import Types.Type
import Types.Common
import Synquid.Type
import Types.Solver
import PetriNet.Util
import Synquid.Pretty
import Database.Util

import Control.Lens
import GHC.Generics
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson
import Data.Maybe
import Data.Either (isLeft)
import Data.List
import Data.List.Extra
import Text.Printf
import Control.Monad.State
import Debug.Trace

allTypesOf :: AbstractCover -> [AbstractSkeleton]
allTypesOf cover = nubOrd $ HashMap.keys cover ++ (Set.toList . Set.unions $ HashMap.elems cover)

coverSize :: AbstractCover -> Int
coverSize = length . allTypesOf

superTypeOf :: [Id] -> AbstractCover -> AbstractSkeleton -> [AbstractSkeleton]
superTypeOf tvs cover at = superTypeOf' tvs rootNode
  where
    superTypeOf' tvs paren = let
        children = Set.toList $ HashMap.lookupDefault Set.empty paren cover
        in if isSubtypeOf tvs at paren then paren : concatMap (superTypeOf' tvs) children
                                       else []

isBot :: AbstractSkeleton -> Bool
isBot ABottom = True
isBot _ = False

isAFunctionT :: AbstractSkeleton -> Bool
isAFunctionT AFunctionT {} = True
isAFunctionT _ = False

isAHigherOrder :: AbstractSkeleton -> Bool
isAHigherOrder (AFunctionT tArg tRet) = isAFunctionT tArg || isAHigherOrder tRet
isAHigherOrder _ = False

lastAbstract :: AbstractSkeleton -> AbstractSkeleton
lastAbstract (AFunctionT _ tRet) = lastAbstract tRet
lastAbstract t = t

abstractParamList :: AbstractSkeleton -> [AbstractSkeleton]
abstractParamList t@AScalar {} = [t]
abstractParamList (AFunctionT tArg tFun) =
    case tFun of
        AScalar _  -> [tArg]
        _          -> tArg : abstractParamList tFun

absFunArgs :: Id -> AbstractSkeleton -> [AbstractSkeleton]
absFunArgs id (AFunctionT tArg tRes) | id == "pair_match" = [tArg]
absFunArgs id (AFunctionT tArg tRes) = tArg : absFunArgs id tRes
absFunArgs _ t = []

decompose :: AbstractSkeleton -> [AbstractSkeleton]
decompose (AFunctionT tArg tRet) = decompose tArg ++ decompose tRet
decompose t@AScalar {} = [t]

decomposeHo :: AbstractSkeleton -> [AbstractSkeleton]
decomposeHo (AFunctionT tArg tRet) = tArg : decomposeHo tRet
decomposeHo t = [t]

toAbstractType :: SType -> AbstractSkeleton
toAbstractType (ScalarT (TypeVarT _ id) _) = AScalar (ATypeVarT id)
toAbstractType (ScalarT (DatatypeT id args _) _) = AScalar (ADatatypeT id (map (compactAbstractType . toAbstractType) args))
toAbstractType (FunctionT _ tArg tRet) = AFunctionT (toAbstractFun tArg) (toAbstractType tRet)
toAbstractType AnyT = AScalar (ATypeVarT varName)
toAbstractType BotT = ABottom
toAbstractType x = error $ "unhandled case: " ++ show x

toAbstractFun :: SType -> AbstractSkeleton
toAbstractFun (FunctionT _ tArg tRes) = AScalar $ ADatatypeT "Fun" [toAbstractFun tArg, toAbstractFun tRes]
toAbstractFun t = toAbstractType t

compactAbstractType :: AbstractSkeleton -> AbstractSkeleton
compactAbstractType (AFunctionT tArg tRes) = AScalar $ ADatatypeT "Fun" [compactAbstractType tArg, compactAbstractType tRes]
compactAbstractType (AScalar (ADatatypeT dt args)) = AScalar (ADatatypeT dt $ map compactAbstractType args)
compactAbstractType t = t

-- this is not subtype relation!!!
isSubtypeOf :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Bool
isSubtypeOf bound t1 t2 = isJust unifier
  where
    unifier = getUnifier (bound ++ abstractTypeVars t1) [(t2, t1)]

isValidSubst :: Map Id AbstractSkeleton -> Bool
isValidSubst m = all (\(id, t) -> id `notElem` abstractTypeVars t) (Map.toList m)

checkUnification :: [Id] -> Map Id AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton -> Maybe (Map Id AbstractSkeleton)
checkUnification bound tass t1 t2 | t1 == t2 = Just tass
checkUnification bound tass (AScalar (ATypeVarT id)) t | id `Map.member` tass =
    -- keep the most informative substitution, eagerly substitute into the final result
    case checkUnification bound tass assigned t of
        Nothing -> Nothing
        Just u | isValidSubst u && isValidSubst (updatedTass u) -> Just (updatedTass u)
               | otherwise -> Nothing
  where
    substed m = abstractSubstitute m assigned
    assigned = fromJust (Map.lookup id tass)
    unboundTv = case assigned of
                  AScalar (ATypeVarT v) | v `notElem` bound -> Just v
                  _ -> Nothing
    substedTass u = Map.map (abstractSubstitute (Map.singleton id (substed u))) u
    updatedTass u = Map.insert id (substed u) (substedTass u)
checkUnification bound tass t (AScalar (ATypeVarT id)) | id `Map.member` tass =
    checkUnification bound tass (AScalar (ATypeVarT id)) t
checkUnification bound tass t@(AScalar (ATypeVarT id)) t'@(AScalar (ATypeVarT id')) 
  | id `elem` bound && id' `elem` bound = Nothing
  | id `elem` bound && id' `notElem` bound = checkUnification bound tass t' t
  | id `notElem` bound && id `elem` abstractTypeVars t' = Nothing
  | id `notElem` bound = Just (Map.insert id t' tass)
checkUnification bound tass (AScalar (ATypeVarT id)) t | id `elem` bound = Nothing
checkUnification bound tass t@(AScalar ADatatypeT {}) t'@(AScalar (ATypeVarT id)) = checkUnification bound tass t' t
checkUnification bound tass (AScalar (ATypeVarT id)) t | id `elem` abstractTypeVars t = Nothing
checkUnification bound tass (AScalar (ATypeVarT id)) t = let
    tass' = Map.map (abstractSubstitute (Map.singleton id t)) tass
    tass'' = Map.insert id t tass'
    in if isValidSubst tass'' then Just tass'' else Nothing
checkUnification bound tass (AScalar (ADatatypeT id tArgs)) (AScalar (ADatatypeT id' tArgs'))
  | id /= id' && 
      ((id == tyclassPrefix && tyclassPrefix `isPrefixOf` id') ||
          (id' == tyclassPrefix && tyclassPrefix `isPrefixOf` id)) = Just tass
  | id /= id' = Nothing
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
getUnifier bound = getUnifier' bound (Just Map.empty)

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
abstractSubstitute' id typ t@(AScalar ATypeVarT {}) = t
abstractSubstitute' id typ (AScalar (ADatatypeT id' args)) = AScalar (ADatatypeT id' (map (abstractSubstitute' id typ) args))
abstractSubstitute' id typ (AFunctionT tArg tRes) = AFunctionT tArg' tRes'
  where
    tArg' = abstractSubstitute' id typ tArg
    tRes' = abstractSubstitute' id typ tRes

abstractTypeVars :: AbstractSkeleton -> [Id]
abstractTypeVars (AScalar (ATypeVarT id)) = [id]
abstractTypeVars (AScalar (ADatatypeT id args)) = concatMap abstractTypeVars args 
abstractTypeVars (AFunctionT tArg tRes) = abstractTypeVars tArg ++ abstractTypeVars tRes
abstractTypeVars _ = []

equalAbstract :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Bool
equalAbstract tvs t1 t2 = isSubtypeOf tvs t1 t2 && isSubtypeOf tvs t2 t1

equalSplit :: [Id] -> SplitMsg -> SplitMsg -> Bool
equalSplit tvs s1 s2 = fst s1 == fst s2 && equalAbstract tvs (snd s1) (snd s2)

existAbstract :: [Id] -> AbstractCover -> AbstractSkeleton -> Bool
existAbstract tvs cover t = existAbstract' rootNode
  where
    existAbstract' paren | equalAbstract tvs paren t = True
    existAbstract' paren | isSubtypeOf tvs t paren = any existAbstract' (Set.toList $ HashMap.lookupDefault Set.empty paren cover)
    existAbstract' paren = False

abstractIntersect :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Maybe AbstractSkeleton
abstractIntersect bound t1 t2 = 
    case unifier of
      Nothing -> Nothing
      Just u -> Just $ abstractSubstitute u t1 
  where
    unifier = getUnifier bound [(t1, t2)]

-- | find the current most restrictive abstraction for a given type
currentAbst :: MonadIO m => [Id] -> AbstractCover -> AbstractSkeleton -> PNSolver m AbstractSkeleton
currentAbst tvs cover (AFunctionT tArg tRes) = do
    tArg' <- currentAbst tvs cover tArg
    tRes' <- currentAbst tvs cover tRes
    return $ AFunctionT tArg' tRes'
currentAbst tvs cover at = do
    freshAt <- freshAbstract tvs at
    case currentAbst' freshAt rootNode of
        Nothing -> error $ "cannot find current abstraction for type " ++ show at
        Just t -> return t
  where
    currentAbst' at paren | isSubtypeOf tvs at paren =
      let children = Set.toList $ HashMap.lookupDefault Set.empty paren cover
          inSubtree = any (isSubtypeOf tvs at) children
       in if inSubtree then head $ filter isJust $ map (currentAbst' at) children
                       else Just paren
    currentAbst' at paren = Nothing

applySemantic :: MonadIO m => [Id] -> AbstractSkeleton -> [AbstractSkeleton] -> PNSolver m AbstractSkeleton
applySemantic tvs fun args = do
    let cargs = init (decompose fun)
    let ret = last (decompose fun)
    let args' = map compactAbstractType args
    constraints <- zipWithM (typeConstraints tvs) cargs args'
    writeLog 3 "applySemantic" $ text "solving constraints" <+> pretty constraints
    let unifier = getUnifier tvs (concat constraints)
    case unifier of
        Nothing -> return ABottom
        Just m -> do
            writeLog 3 "applySemantic" $ text "get unifier" <+> pretty (Map.toList m)
            cover <- gets $ view (refineState . abstractionCover)
            let substRes = abstractSubstitute m ret
            writeLog 3 "applySemantic" $ text "current cover" <+> text (show cover)
            currentAbst tvs cover substRes

compareAbstract :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Ordering
compareAbstract tvs t1 t2 | isSubtypeOf tvs t1 t2 && isSubtypeOf tvs t2 t1 = EQ
                          | isSubtypeOf tvs t1 t2 = LT
                          | otherwise = GT
