module Examples.InferenceDriver(
    parseExample,
    getExampleTypes
    ) where

import Database.Util
import Types.Type
import Types.IOFormat
import Types.InfConstraint
import Examples.Utils
import Synquid.Type
import PetriNet.Util
import Synquid.Pretty

import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC (exprType, typeToLHsType) hiding (Id)
import GHC.Paths
import TcRnDriver
import Outputable
import Control.Monad.State
import Data.List.Extra (groupOn)

parseExample :: [String] -> String -> IO (Either ConstrainedSchema ErrorMessage)
parseExample mdls mkFun = catch (do
    typ <- askGhc mdls $ exprType TM_Inst mkFun
    let hsType = typeToLHsType typ
    return (Left $ toInt $ resolveType hsType))
    (\(e :: SomeException) -> return (Right $ show e))
    where
        toInt (ForallT x t) = ForallT x (toInt t)
        toInt (Monotype t) = Monotype (integerToInt t)

getExampleTypes :: Environment -> [ConstrainedSchema] -> IO [ConstrainedType]
getExampleTypes env validSchemas = do
    let validTypes = map (shape . toMonotype) validSchemas
    t <- if not (null validTypes) then foldM antiUnification (head validTypes) (tail validTypes)
                                  else error "get example types error"
    let tvars = typeVarsOf t
    let generals = getGeneralizations t
    let reducedTypes = concatMap (reduceVars tvars) generals
    msgChan <- newChan
    checkRes <- mapM (\s -> checkTypes env msgChan (forall s) (forall t)) reducedTypes
    let checkedReduce = map snd $ filter (fst . fst) (zip checkRes reducedTypes)
    return $ t : generals ++ checkedReduce
    where
        forall t = let vars = typeVarsOf t
                    in foldr ForallT (Monotype $ addTrue t) vars

-- turn a GHC type into Hoogle+ type
resolveType :: LHsType GhcPs -> ConstrainedSchema
resolveType (L _ (HsForAllTy bs t)) = foldr ForallT (resolveType t) vs
    where
        vs = map vname bs
        vname (L _ (UserTyVar (L _ id))) = showSDocUnsafe (ppr id)
        vname (L _ (KindedTyVar (L _ id) _)) = showSDocUnsafe (ppr id)
resolveType (L _ (HsFunTy f _)) = Monotype (resolveType' f)
resolveType (L _ (HsQualTy ctx body)) = Monotype bodyWithTcArgs
    where
        unlocatedCtx = let L _ c = ctx in c
        tyConstraints = map resolveType' unlocatedCtx

        toClassConstraint tc@(ScalarT (DatatypeT name args rs) r) = 
            case head args of
              ScalarT (TypeVarT _ v) -> (v, name)
              _ -> error $ "Unsupported type class " ++ show tc
        toClassConstraint tc = error $ "Unsupported type class " ++ show tc

        tcConstraints = map toClassConstraint tyConstraints
        sortedTyclass = groupOn fst $ sortOn fst tcConstraints
        tcMap = Map.fromList $ map (\p -> over _1 head $ unzip p) sortedTyclass
        bodyWithTcArgs = addTcConstraint tcMap (resolveType' body)
resolveType t = error (showSDocUnsafe $ ppr t)

addTcConstraint :: Map Id (Set Id) -> TypeSkeleton r -> ConstrainedType
addTcConstraint tcMap (ScalarT (TypeVarT m v) r) =
    case Map.lookup m tcMap of
      Just s -> ScalarT (TypeVarT m v) s
      Nothing -> ScalarT (TypeVarT m v) r
addTcConstraint tcMap (ScalarT (DatatypeT name args rs) r) =
    -- propagate the tc constraints from inner types to outer data types
    -- if possible, this requires us to check the instances
    let argsWithTc = map (addTcConstraint tcMap) args
     in ScalarT (DatatypeT name argsWithTc rs) r
addTcConstraint tcMap (FunctionT x tArg tRes) =
    let tArgWithTc = addTcConstraint tcMap tArg
        tResWithTc = addTcConstraint tcMap tRes
     in FunctionT x tArgWithTc tResWithTc
addTcConstraint tcMap AnyT = AnyT

resolveType' :: LHsType GhcPs -> RType
resolveType' (L _ (HsFunTy f r)) = FunctionT "" (resolveType' f) (resolveType' r)
resolveType' (L _ (HsQualTy _ t)) = resolveType' t
resolveType' (L _ (HsTyVar _ (L _ v))) = 
    if isLower (head name)
       then ScalarT (TypeVarT Map.empty name) ftrue
       else ScalarT (DatatypeT name [] []) ftrue
    where
        name = showSDocUnsafe $ ppr v
resolveType' t@(L _ HsAppTy{}) = ScalarT (DatatypeT dtName dtArgs []) ftrue
    where
        dtName = case datatypeOf t of
                   "[]" -> "List"
                   "(,)" -> "Pair"
                   n -> n
        dtArgs = datatypeArgs t

        datatypeOf (L _ (HsAppTy f _)) = datatypeOf f
        datatypeOf (L _ (HsTyVar _ (L _ v))) = showSDocUnsafe (ppr v)

        datatypeArgs (L _ (HsAppTy (L _ HsTyVar {}) a)) = [resolveType' a]
        datatypeArgs (L _ (HsAppTy f a)) = datatypeArgs f ++ datatypeArgs a
        datatypeArgs t = [resolveType' t]

resolveType' (L _ (HsListTy t)) = ScalarT (DatatypeT "List" [resolveType' t] []) ftrue
resolveType' (L _ (HsTupleTy _ ts)) = foldr mkPair basePair otherTyps
    where
        mkPair acc t = ScalarT (DatatypeT "Pair" [acc, t] []) ftrue
        resolveTyps = map resolveType' ts
        (baseTyps, otherTyps) = splitAt (length ts - 2) resolveTyps
        basePair = ScalarT (DatatypeT "Pair" baseTyps []) ftrue
resolveType' (L _ (HsParTy t)) = resolveType' t
resolveType' t = error $ showSDocUnsafe (ppr t)

antiSubstitute :: SType -> Id -> SType -> SType
antiSubstitute pat name t | t == pat = vart_ name
antiSubstitute pat name (ScalarT (DatatypeT dt args _) _) = 
    ScalarT (DatatypeT dt (map (antiSubstitute pat name) args) []) ()
antiSubstitute pat name (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = antiSubstitute pat name tArg
        tRes' = antiSubstitute pat name tRes
antiSubstitute _ _ t = t

antiUnification :: ConstrainedType -> ConstrainedType -> IO ConstrainedType
antiUnification t1 t2 = evalStateT (antiUnification' t1 t2) emptyAntiUnifState

antiUnification' :: ConstrainedType -> ConstrainedType -> AntiUnifier IO ConstrainedType
antiUnification' AnyT t = return t
antiUnification' t AnyT = return t
antiUnification' (ScalarT (TypeVarT m id) s1) (ScalarT (TypeVarT _ _) s2) =
    let s = mergeTyclass s1 s2
     in ScalarT (TypeVarT m id) s
antiUnification' (ScalarT (TypeVarT m id) s) t =
    -- change the interface here, we have three options:
    -- all, partial, none
    case checkTyclass s t of
      AllSatisfy s' -> ScalarT (TypeVarT m id) s'
      PartialSatisfy s' -> ScalarT (TypeVarT m id) s'
      NoSatisfy -> do
          v <- freshId "a"
          modify $ over typeAssignment1 (Map.insertWith (++) t1 [v])
          modify $ over typeAssignment2 (Map.insertWith (++) t2 [v])
          return $ vart v (Set.empty)
antiUnification' t tv@(ScalarT (TypeVarT {}) _) = do
    swapTass
    result <- antiUnification' tv t
    swapTass
    return result
    where
        swapTass = do
            tass1 <- gets $ view typeAssignment1
            tass2 <- gets $ view typeAssignment2
            modify $ set typeAssignment1 tass2
            modify $ set typeAssignment2 tass1
antiUnification' t1@(ScalarT (DatatypeT dt1 args1 _) s1) t2@(ScalarT (DatatypeT dt2 args2 _) s2)
  | dt1 == dt2 = do
      args' <- mapM (uncurry antiUnification') (zip args1 args2)
      let s = mergeTyclass s1 s2
      return $ ScalarT (DatatypeT dt1 args' []) s
  | dt1 /= dt2 = do
      tass1 <- gets $ view typeAssignment1
      tass2 <- gets $ view typeAssignment2
      let overlap = (tass1 Map.! t1) `intersect` (tass2 Map.! t2)
      if t1 `Map.member` tass1 && t2 `Map.member` tass2 && not (null overlap)
         then if length overlap > 1 then error "antiUnficiation fails"
                                    else return $ vart (head overlap) Set.empty
         else do 
             v <- freshId "a"
             modify $ over typeAssignment1 (Map.insertWith (++) t1 [v])
             modify $ over typeAssignment2 (Map.insertWith (++) t2 [v])
             let s = mergeTyclass s1 s2
             return $ vart v s
antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
    tArg <- antiUnification' tArg1 tArg2
    tRes <- antiUnification' tRes1 tRes2
    return $ FunctionT x1 tArg tRes

getGeneralizations :: SType -> [SType]
getGeneralizations t =
    let subtypesEach = map subtypesOf (breakdown t)
        atLeast2 = filter ((>= 2) . length) $ subsequences subtypesEach
        commonSubtypes =  map intersections atLeast2
        validCommons = filter (not . Set.null) commonSubtypes
        freeVars = Set.toList $ typeVarsOf t
        validNames = foldr delete seqChars freeVars
        combineCommons = nub $ map Set.unions $ tail $ subsequences validCommons
        permutedCommons = concatMap (tail . subsequences . Set.toList) combineCommons
        namedCommons = map (flip zip validNames) (nub permutedCommons)
     in map (foldr (uncurry antiSubstitute) t) namedCommons
    where
        intersections [] = Set.empty
        intersections (x:xs) = foldr Set.intersection x xs

reduceVars :: Set Id -> SType -> [SType]
reduceVars vs t =
    let freeVars = Set.toList (typeVarsOf t)
        varSubsts = varMaps freeVars
     in map (foldr (uncurry antiSubstitute) t) varSubsts
    where
        listv = Set.toList vs
        varMaps [] = [[]]
        varMaps (v:vs) = [[l1] | l1 <- map (vart_ v,) vs] ++ 
            [l1 : l2 | l1 <- map (vart_ v,) vs, l2 <- varMaps vs]

mergeTyclass :: Set Id -> Set Id -> Set Id
mergeTyclass s1 s2 = undefined
    where
        commonTyclass = s1 `Set.intersection` s2
        diffTyclass1 = s1 `Set.difference` commonTyclass
        diffTyclass2 = s2 `Set.difference` commonTyclass
        leastCommonPredecessor = 

checkTyclass :: Set Id -> SType -> Maybe (Set Id)
checkTyclass = undefined
