{-# LANGUAGE FlexibleContexts #-}

module PetriNet.AbstractType where

import Database.Utils
import Encoder.ConstraintEncoder
import HooglePlus.TypeChecker
import PetriNet.Utils
import Synquid.Pretty
import Synquid.Type
import Synquid.Program
import Types.Common
import Types.Environment
import Types.Solver
import Types.Type
import Types.TypeChecker

import Control.Lens
import Control.Monad.Extra
import Control.Monad.State
import Data.Aeson
import Data.Either (isLeft)
import Data.List
import Data.List.Extra
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Debug.Trace
import GHC.Generics
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Printf

allTypesOf :: AbstractCover -> [AbstractSkeleton]
allTypesOf cover = nubOrd $ HashMap.keys cover ++ (Set.toList . Set.unions $ HashMap.elems cover)

coverSize :: AbstractCover -> Int
coverSize = length . allTypesOf

superTypeOf :: Environment -> AbstractCover -> AbstractSkeleton -> [AbstractSkeleton]
superTypeOf env cover at = superTypeOf' rootNode
  where
    superTypeOf' paren
        | isSubtypeOf env at paren =
            let children = Set.toList $ HashMap.lookupDefault Set.empty paren cover
             in paren : concatMap superTypeOf' children
        | otherwise = []

toAbstractType :: TypeSkeleton -> AbstractSkeleton
toAbstractType (TyAppT fun arg) = TyAppT fun' arg'
    where
        fun' = toAbstractFun fun
        arg' = toAbstractFun arg
toAbstractType (TyFunT fun res) = TyFunT fun' res'
    where
        fun' = toAbstractFun fun
        res' = toAbstractFun res
toAbstractType (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = toAbstractFun tArg
        tRes' = toAbstractType tRes
toAbstractType t = t

toAbstractFun :: TypeSkeleton -> AbstractSkeleton
toAbstractFun (FunctionT _ tArg tRes) = TyFunT tArg' tRes'
    where
        tArg' = toAbstractFun tArg
        tRes' = toAbstractFun tRes
toAbstractFun (TyAppT tFun tArg) = TyAppT tFun' tArg'
    where
        tFun' = toAbstractFun tFun
        tArg' = toAbstractFun tArg
toAbstractFun t = t

-- this is not subtype relation, but subsumption relation
isSubtypeOf :: Environment -> AbstractSkeleton -> AbstractSkeleton -> Bool
isSubtypeOf env t1 t2 = isJust mbUnifier
    where
        env' = Set.foldr addTypeVar env (typeVarsOf t1)
        mbUnifier = getUnifier env' t2 t1

isValidSubst :: Map Id AbstractSkeleton -> Bool
isValidSubst m = all (\(id, t) -> id `Set.member` typeVarsOf t) (Map.toList m)

getUnifier :: Environment -> AbstractSkeleton -> AbstractSkeleton -> Maybe (Map Id AbstractSkeleton)
getUnifier env t1 t2 =
    guard (checkResult ^. isChecked) >>
    Just (checkResult ^. typeAssignment)
    where
        checkResult = execState (solveTypeConstraint env t1 t2) emptyChecker

equalAbstract :: Environment -> AbstractSkeleton -> AbstractSkeleton -> Bool
equalAbstract env t1 t2 = sub && sup
    where
        sub = isSubtypeOf env t1 t2
        sup = isSubtypeOf env t2 t1

equalSplit :: Environment -> SplitMsg -> SplitMsg -> Bool
equalSplit env s1 s2 = fst s1 == fst s2 && equalAbstract env (snd s1) (snd s2)

existAbstract :: Environment -> AbstractCover -> AbstractSkeleton -> Bool
existAbstract env cover t = existAbstract' rootNode
    where
        existAbstract' paren =
            let eq = equalAbstract env paren t
                sub = isSubtypeOf env t paren
                children = Set.toList $ HashMap.lookupDefault Set.empty paren cover
             in eq || any existAbstract' children

abstractIntersect :: Environment -> AbstractSkeleton -> AbstractSkeleton -> Maybe AbstractSkeleton
abstractIntersect env t1 t2 = getUnifier env t1 t2 <&> (`typeSubstitute` t1)

-- | find the current most restrictive abstraction for a given type
currentAbst :: (ConstraintEncoder enc, MonadIO m) 
            => Environment 
            -> AbstractCover 
            -> AbstractSkeleton 
            -> PNSolver enc m AbstractSkeleton
currentAbst env cover (FunctionT x tArg tRes) = do
    tArg' <- currentAbst env cover tArg
    tRes' <- currentAbst env cover tRes
    return $ FunctionT x tArg' tRes'
currentAbst env cover at = do
    let bound = env ^. boundTypeVars
    freshAt <- freshType bound (toPolytype bound at)
    case currentAbst' freshAt rootNode of
        Nothing -> error $ "cannot find current abstraction for type " ++ show at
        Just t -> return t
    where
        currentAbst' at paren | isSubtypeOf env at paren =
            let children = Set.toList $ HashMap.lookupDefault Set.empty paren cover
                childSubs = map (currentAbst' at) children
            in if any (isSubtypeOf env at) children
                then head (filter isJust childSubs)
                else Just paren
        currentAbst' _ _ = Nothing

applySemantic :: (ConstraintEncoder enc, MonadIO m) 
              => Environment 
              -> AbstractSkeleton 
              -> [AbstractSkeleton]
              -> PNSolver enc m AbstractSkeleton
applySemantic env fun args = do
    let cargs = allArgTypes (toAbstractType fun)
    let ret = lastType fun
    constraints <- zipWithM (typeConstraint (env ^. boundTypeVars)) cargs args
    writeLog 3 "applySemantic" $ text "solving constraints" <+> pretty constraints
    let checkResult = execState (mapM (uncurry $ solveTypeConstraint env) constraints) emptyChecker
    if checkResult ^. isChecked
        then do
            let ass = checkResult ^. typeAssignment
            writeLog 3 "applySemantic" $ text "get unifier" <+> pretty (Map.toList ass)
            cover <- gets $ view (refineState . abstractionCover)
            let substRes = typeSubstitute ass ret
            writeLog 3 "applySemantic" $ text "current cover" <+> text (show cover)
            writeLog 3 "applySemantic" $ text "apply result" <+> pretty substRes
            currentAbst env cover substRes
        else return BottomT
    where
        typeConstraint bound t1 t2 = do
            t2' <- freshType bound (toPolytype bound t2)
            return (t1, t2')

compareAbstract :: Environment -> AbstractSkeleton -> AbstractSkeleton -> Ordering
compareAbstract env t1 t2
    | equalAbstract env t1 t2 = EQ
    | isSubtypeOf env t1 t2 = LT
    | otherwise = GT
