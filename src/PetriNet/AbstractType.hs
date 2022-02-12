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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
abstractParamList (AFunctionT tArg tFun) =
    case tFun of
        AFunctionT {} -> tArg : abstractParamList tFun
        _ -> [tArg]
abstractParamList t = [t]

absFunArgs :: Id -> AbstractSkeleton -> [AbstractSkeleton]
absFunArgs id (AFunctionT tArg tRes) | id == "pair_match" = [tArg]
absFunArgs id (AFunctionT tArg tRes) = tArg : absFunArgs id tRes
absFunArgs _ t = []

decompose :: AbstractSkeleton -> [AbstractSkeleton]
decompose (AFunctionT tArg tRet) = decompose tArg ++ decompose tRet
decompose t = [t]

decomposeHo :: AbstractSkeleton -> [AbstractSkeleton]
decomposeHo (AFunctionT tArg tRet) = tArg : decomposeHo tRet
decomposeHo t = [t]

toAbstractType :: TypeSkeleton -> AbstractSkeleton
toAbstractType (TypeVarT id k) = ATypeVarT id k
toAbstractType (DatatypeT id k) = ADatatypeT id k
toAbstractType (TyAppT fun arg k) = ATyAppT fun' arg' k
    where
        fun' = compactAbstractType (toAbstractType fun)
        arg' = compactAbstractType (toAbstractType arg)
toAbstractType (TyFunT fun res) = ATyFunT fun' res'
    where
        fun' = compactAbstractType (toAbstractType fun)
        res' = compactAbstractType (toAbstractType res)
toAbstractType (FunctionT _ tArg tRes) = AFunctionT tArg' tRes'
    where
        tArg' = toAbstractFun tArg
        tRes' = toAbstractType tRes
toAbstractType AnyT = ATypeVarT varName KnStar
toAbstractType BotT = ABottom

toAbstractFun :: TypeSkeleton -> AbstractSkeleton
toAbstractFun (FunctionT _ tArg tRes) = ATyFunT tArg' tRes'
    where
        tArg' = toAbstractFun tArg
        tRes' = toAbstractFun tRes
toAbstractFun t = toAbstractType t

compactAbstractType :: AbstractSkeleton -> AbstractSkeleton
compactAbstractType (AFunctionT tArg tRes) = ATyFunT tArg' tRes'
    where
        tArg' = compactAbstractType tArg
        tRes' = compactAbstractType tRes
compactAbstractType (ATyAppT tFun tArg k) = ATyAppT tFun' tArg' k
    where
        tFun' = compactAbstractType tFun
        tArg' = compactAbstractType tArg
compactAbstractType t = t

-- this is not subtype relation, but subsumption relation
isSubtypeOf :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Bool
isSubtypeOf bound t1 t2 = isJust unifier
  where
    unifier = getUnifier (bound ++ abstractTypeVars t1) [(t2, t1)]

isValidSubst :: Map Id AbstractSkeleton -> Bool
isValidSubst m = all (\(id, t) -> id `notElem` abstractTypeVars t) (Map.toList m)

checkUnification :: [Id] 
                 -> Map Id AbstractSkeleton 
                 -> AbstractSkeleton 
                 -> AbstractSkeleton 
                 -> Maybe (Map Id AbstractSkeleton)
checkUnification bound tass t1 t2 | t1 == t2 = Just tass
checkUnification bound tass (ATypeVarT id _) t | id `Map.member` tass =
    -- keep the most informative substitution, eagerly substitute into the final result
    case checkUnification bound tass assigned t of
        Nothing -> Nothing
        Just u | isValidSubst u && isValidSubst (updatedTass u) -> Just (updatedTass u)
               | otherwise -> Nothing
  where
    substed m = abstractSubstitute m assigned
    assigned = fromJust (Map.lookup id tass)
    unboundTv = case assigned of
                  ATypeVarT v _ | v `notElem` bound -> Just v
                  _ -> Nothing
    substedTass u = Map.map (abstractSubstitute (Map.singleton id (substed u))) u
    updatedTass u = Map.insert id (substed u) (substedTass u)
checkUnification bound tass t@(ATypeVarT id k) t'@(ATypeVarT id' k')
  | id `elem` bound && id' `elem` bound = Nothing
  | not (compareKind k k') = Nothing
  | id `elem` bound && id' `notElem` bound = checkUnification bound tass t' t
  | id `notElem` bound && id `elem` abstractTypeVars t' = Nothing
  | id `notElem` bound = Just (Map.insert id t' tass)
checkUnification bound tass (ATypeVarT id _) t | id `elem` bound = Nothing
checkUnification bound tass (ATypeVarT id _) t | id `elem` abstractTypeVars t = Nothing
checkUnification bound tass (ATypeVarT _ k) (ADatatypeT _ k') | not (compareKind k k') = Nothing
checkUnification bound tass (ATypeVarT id _) t = let
    tass' = Map.map (abstractSubstitute (Map.singleton id t)) tass
    tass'' = Map.insert id t tass'
    in if isValidSubst tass'' then Just tass'' else Nothing
checkUnification bound tass t t'@(ATypeVarT {}) = checkUnification bound tass t' t
checkUnification bound tass (ADatatypeT id k) (ADatatypeT id' k') 
    | id /= id' || not (compareKind k k') = Nothing
    | id == id' && compareKind k k' = return tass
checkUnification bound tass (ATyAppT tFun tArg k) (ATyAppT tFun' tArg' k') 
    | compareKind k k' = case checkUnification bound tass tFun tFun' of
        Nothing -> Nothing
        Just tass' -> checkUnification bound tass' tArg tArg'
    | otherwise = Nothing
checkUnification bound tass (ATyFunT tArg tRes) (ATyFunT tArg' tRes') =
    case checkUnification bound tass tArg tArg' of
        Nothing -> Nothing
        Just tass' -> checkUnification bound tass' tRes tRes'
checkUnification bound tass (AFunctionT tArg tRes) (AFunctionT tArg' tRes') =
    case checkUnification bound tass tArg tArg' of
        Nothing -> Nothing
        Just tass' -> checkUnification bound tass' tRes tRes'
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
abstractSubstitute' id typ (ATypeVarT id' _) | id == id' = typ
abstractSubstitute' id typ t@(ATypeVarT {}) = t
abstractSubstitute' id typ t@(ADatatypeT {}) = t
abstractSubstitute' id typ (ATyAppT tFun tArg k) = ATyAppT tFun' tArg' k
    where
        tFun' = abstractSubstitute' id typ tFun
        tArg' = abstractSubstitute' id typ tArg
abstractSubstitute' id typ (ATyFunT tArg tRes) = ATyFunT tArg' tRes'
    where
        tArg' = abstractSubstitute' id typ tArg
        tRes' = abstractSubstitute' id typ tRes
abstractSubstitute' id typ (AFunctionT tArg tRes) = AFunctionT tArg' tRes'
    where
        tArg' = abstractSubstitute' id typ tArg
        tRes' = abstractSubstitute' id typ tRes

abstractTypeVars :: AbstractSkeleton -> [Id]
abstractTypeVars (ATypeVarT id _) = [id]
abstractTypeVars (ATyFunT tArg tRes) = abstractTypeVars tArg ++ abstractTypeVars tRes
abstractTypeVars (ATyAppT tFun tArg _) = abstractTypeVars tFun ++ abstractTypeVars tArg
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
        existAbstract' paren | isSubtypeOf tvs t paren = let
            children = Set.toList $ HashMap.lookupDefault Set.empty paren cover
            in any existAbstract' children
        existAbstract' paren = False

abstractIntersect :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Maybe AbstractSkeleton
abstractIntersect bound t1 t2 = 
    case unifier of
        Nothing -> Nothing
        Just u -> Just $ abstractSubstitute u t1 
    where
        unifier = getUnifier bound [(t1, t2)]

-- -- | find the current most restrictive abstraction for a given type
-- currentAbst :: MonadIO m => [Id] -> AbstractCover -> AbstractSkeleton -> PNSolver m [AbstractSkeleton]
-- currentAbst tvs cover (AFunctionT tArg tRes) = do
--     tArg' <- currentAbst tvs cover tArg
--     tRes' <- currentAbst tvs cover tRes
--     return $ [AFunctionT a r | a <- tArg', r <- tRes']
-- currentAbst tvs cover at = do
--     freshAt <- freshAbstract tvs at
--     case currentAbst' freshAt rootNode of
--         [] -> error $ "cannot find current abstraction for type " ++ show at
--         ts -> return ts
--     where
--         currentAbst' at paren | isSubtypeOf tvs at paren = let 
--             children = Set.toList $ HashMap.lookupDefault Set.empty paren cover
--             inSubtree = any (isSubtypeOf tvs at) children
--             in if inSubtree then concatMap (currentAbst' at) children
--                             else [paren]
--         currentAbst' at paren = []
-- | find the current most restrictive abstraction for a given type
currentAbst :: MonadIO m => [Id] -> AbstractCover -> AbstractSkeleton -> PNSolver m AbstractSkeleton
currentAbst tvs cover (AFunctionT tArg tRes) = do
    writeLog 3 "currentAbst" $ text "finding current abstraction for" <+> pretty (AFunctionT tArg tRes)
    tArg' <- currentAbst tvs cover tArg
    tRes' <- currentAbst tvs cover tRes
    return $ AFunctionT tArg' tRes'
currentAbst tvs cover at = do
    writeLog 3 "currentAbst" $ text "finding current abstraction for" <+> pretty at
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

-- | find the most general unifier between arguments
-- and apply this unifier onto the return type
-- a special case for type
applySemantic :: MonadIO m 
              => [Id] 
              -> AbstractSkeleton 
              -> [AbstractSkeleton] 
              -> PNSolver m AbstractSkeleton
applySemantic tvs fun args = do
    let cargs = init (decompose fun)
    let ret = last (decompose fun)
    let args' = map compactAbstractType args
    constraints <- zipWithM (typeConstraints tvs) cargs args'
    let constraints' = concat constraints
    -- writeLog 3 "applySemantic" $ text "solving constraints" <+> pretty constraints
    if matchTc constraints' then do
        let unifier = getUnifier tvs constraints'
        case unifier of
            Nothing -> return ABottom
            Just m -> do
                -- writeLog 3 "applySemantic" $ text "get unifier" <+> pretty (Map.toList m)
                cover <- gets (view abstractionCover)
                let substRes = abstractSubstitute m ret
                -- writeLog 3 "applySemantic" $ text "current cover" <+> text (show cover)
                currentAbst tvs cover substRes
        else return ABottom
  where
    matchTc [] = True
    matchTc (c:cs) = case c of
        (ADatatypeT id1 _, ADatatypeT id2 _) ->
            (((tyclassPrefix `isPrefixOf` id1) && (tyclassPrefix `isPrefixOf` id2)) ||
            (not (tyclassPrefix `isPrefixOf` id1) && not (tyclassPrefix `isPrefixOf` id2))) &&
            matchTc cs
        (ADatatypeT id _, _) -> (not (tyclassPrefix `isPrefixOf` id)) && matchTc cs
        (_, ADatatypeT id _) -> (not (tyclassPrefix `isPrefixOf` id)) && matchTc cs
        _ -> matchTc cs

compareAbstract :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> Ordering
compareAbstract tvs t1 t2 | isSubtypeOf tvs t1 t2 && isSubtypeOf tvs t2 t1 = EQ
                          | isSubtypeOf tvs t1 t2 = LT
                          | otherwise = GT
