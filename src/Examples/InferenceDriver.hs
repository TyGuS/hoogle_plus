{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.InferenceDriver(
    parseExample,
    getExampleTypes
    ) where

import Database.Utils
import Types.Common
import Types.Type
import Types.IOFormat
import Types.InfConstraint
import Types.Environment
import Examples.Utils
import Synquid.Type
import PetriNet.Utils
import Synquid.Pretty

import Control.Exception
import Control.Monad.State
import Control.Monad.Logic
import Control.Lens
import Data.Char
import Data.List
import Data.List.Extra (groupOn, nubOrdBy)
import Data.Either
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC hiding (Id)
import GHC.Paths
import TcRnDriver
import Outputable
import Text.Printf

-- pre: the mkFun is a tuple of arguments and return types
parseExample :: [String] -> String -> IO (Either ErrorMessage SchemaSkeleton)
parseExample mdls mkFun = catch (do
    typ <- askGhc mdls $ exprType TM_Default mkFun
    let hsType = typeToLHsType typ
    let hpType = toInt $ resolveType hsType
    return (Right hpType))
    (\(e :: SomeException) -> return (Left $ show e))
    where
        toInt (ForallT x t) = ForallT x (toInt t)
        toInt (Monotype t) = Monotype (integerToInt t)

getExampleTypes :: Environment -> [SchemaSkeleton] -> Int -> IO [String]
getExampleTypes env validSchemas num = do
    let validTypes = map toMonotype validSchemas
    let mdls = env ^. included_modules
    let initTyclassState = emptyTyclassState { _supportModules = Set.toList mdls }
    -- remove magic number later
    take num <$> evalStateT (do
        (mbFreeVar, st) <- if not (null validTypes)
                then foldM stepAntiUnification (head validTypes, emptyAntiUnifState) (tail validTypes)
                else error "get example types error"
        -- let t = defaultTypeVar mbFreeVar
        let t = mbFreeVar
        let tvars = typeVarsOf t
        let tcass = st ^. tyclassAssignment
        generalizeType validSchemas tcass t
        ) initTyclassState
    where
        stepAntiUnification (t1, st) t2 = antiUnification t1 t2 st
        forall t = let vars = typeVarsOf t
                    in foldr ForallT (Monotype t) vars

-- turn a GHC type into Hoogle+ type
resolveType :: LHsType GhcPs -> SchemaSkeleton
resolveType (L _ (HsForAllTy _ _ bs t)) = foldr ForallT (resolveType t) vs
    where
        vs = map vname bs
        vname (L _ (UserTyVar _ (L _ id))) = showSDocUnsafe (ppr id)
        vname (L _ (KindedTyVar _ (L _ id) _)) = showSDocUnsafe (ppr id)
resolveType t@(L _ (HsAppTy _ fun arg)) =
    let typs = tail $ map resolveType' $ breakApp t
        (args, [res]) = splitAt (length typs - 1) typs
     in Monotype $ foldr (FunctionT "") res args
    where
        breakApp (L _ (HsAppTy _ fun arg)) = breakApp fun ++ [arg]
        breakApp t = [t]
resolveType (L _ (HsQualTy _ ctx body)) = Monotype bodyWithTcArgs
    where
        unlocatedCtx = let L _ c = ctx in c
        tyConstraints = map (prefixTyclass . resolveType') unlocatedCtx

        prefixTyclass (DatatypeT name) = DatatypeT (tyclassPrefix ++ name)
        prefixTyclass tc = error $ "Unsupported type class " ++ show tc

        bodyWithTcArgs = foldr (FunctionT "") (toMonotype $ resolveType body) tyConstraints
resolveType t = Monotype $ resolveType' t

resolveType' :: LHsType GhcPs -> TypeSkeleton
resolveType' (L _ (HsFunTy _ f r)) = FunctionT "" (resolveType' f) (resolveType' r)
resolveType' (L _ (HsQualTy _ _ t)) = resolveType' t
resolveType' (L _ (HsTyVar _ _ (L _ v))) =
    if isLower (head name)
       then TypeVarT (existTypeVarPrefix:name)
       else DatatypeT name
    where
        name = showSDocUnsafe $ ppr v
resolveType' (L _ (HsAppTy _ f a)) = TyAppT dt a'
    where
        f' = resolveType' f
        a' = resolveType' a
        dt = case f' of
                DatatypeT "[]" -> DatatypeT "List"
                DatatypeT "(,)" -> DatatypeT "Pair"
                _ -> f'
resolveType' (L _ (HsListTy _ t)) = TyAppT (DatatypeT "List") (resolveType' t)
resolveType' (L _ (HsTupleTy _ _ ts)) = foldr TyAppT (DatatypeT "Pair") ts'
    where
        ts' = map resolveType' ts
resolveType' (L _ (HsParTy _ t)) = resolveType' t
resolveType' t = error $ showSDocUnsafe (ppr t)

antiSubstitute :: TypeSkeleton -> Id -> TypeSkeleton -> TypeSkeleton
antiSubstitute pat name t | t == pat = TypeVarT name
antiSubstitute pat name (TyAppT tFun tArg) = TyAppT tFun' tArg'
    where
        tFun' = antiSubstitute pat name tFun
        tArg' = antiSubstitute pat name tArg
antiSubstitute pat name (TyFunT tArg tRes) = TyFunT tArg' tRes'
    where
        tArg' = antiSubstitute pat name tArg
        tRes' = antiSubstitute pat name tRes
antiSubstitute pat name (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = antiSubstitute pat name tArg
        tRes' = antiSubstitute pat name tRes
antiSubstitute _ _ t = t

antiUnification :: TypeSkeleton -> TypeSkeleton -> AntiUnifState -> StateT TypeClassState IO (TypeSkeleton, AntiUnifState)
antiUnification t1 t2 = runStateT (antiUnification' t1 t2)

newAntiVariable :: TypeSkeleton -> TypeSkeleton -> Maybe TyclassConstraints -> AntiUnifier IO TypeSkeleton
newAntiVariable t1 t2 mbCons = do
    v <- freshId [] "t"
    modify $ over typeAssignment1 (Map.insertWith (++) t1 [v])
    modify $ over typeAssignment2 (Map.insertWith (++) t2 [v])
    when (isJust mbCons) (
        modify $ over tyclassAssignment (Map.insertWith Set.union v (fromJust mbCons))
        )
    return $ TypeVarT v

findWithDefaultAntiVariable :: TypeSkeleton -> TypeSkeleton -> AntiUnifier IO TypeSkeleton
findWithDefaultAntiVariable t1 t2 = do
    tass1 <- gets $ view typeAssignment1
    tass2 <- gets $ view typeAssignment2
    vs1 <- liftIO $ unifiableVars tass1 t1
    vs2 <- liftIO $ unifiableVars tass2 t2
    let overlap = vs1 `intersect` vs2
    if not (null overlap)
       then return $ TypeVarT (head overlap)
       else checkTyclass t1 t2
    where
        unifiableVars tass t = do
            results <- filterM (\(k, vs) -> do
                let vars = Set.toList $ typeVarsOf k `Set.union` typeVarsOf t
                let boundVars = filter (not . (==) existTypeVarPrefix . head) vars
                let env = emptyEnv { _boundTypeVars = boundVars }
                (isChecked, _) <- checkTypes env (mkPolyType t) (mkPolyType k)
                return isChecked) (Map.toList tass)
            return $ concatMap snd results

-- | this is a test document
antiUnification' :: TypeSkeleton -> TypeSkeleton -> AntiUnifier IO TypeSkeleton
antiUnification' t1 t2 | t1 == t2 = return t1
antiUnification' AnyT t = return t
antiUnification' t AnyT = return t
antiUnification' t1@(TypeVarT id1) t2@(TypeVarT id2)
  | existTypeVarPrefix == head id1 = return $ TypeVarT id2
  | existTypeVarPrefix == head id2 = return $ TypeVarT id1
  ---- | otherwise = findWithDefaultAntiVariable t1 t2
antiUnification' t1@(TypeVarT id) t
  | existTypeVarPrefix == head id = return t
  ---- | otherwise = findWithDefaultAntiVariable t1 t
antiUnification' t tv@(TypeVarT _) = do
    swapAssignments
    result <- antiUnification' tv t
    swapAssignments
    return result
antiUnification' t1@(TyAppT tFun1 tArg1) t2@(TyAppT tFun2 tArg2) = do
    let (dt1, args1) = collectArgs t1
    let (dt2, args2) = collectArgs t2
    if dt1 == dt2
        then do args' <- zipWithM antiUnification' args1 args2
                return $ foldl' TyAppT (DatatypeT dt1) args'
        else findWithDefaultAntiVariable t1 t2
antiUnification' t1@(TyFunT tArg1 tRes1) t2@(TyFunT tArg2 tRes2) = do
    tArg <- antiUnification' tArg1 tArg2
    tRes <- antiUnification' tRes1 tRes2
    return (TyFunT tArg tRes)
antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
    tArg <- antiUnification' tArg1 tArg2
    tRes <- antiUnification' tRes1 tRes2
    return $ FunctionT x1 tArg tRes
antiUnification' t1 t2 = findWithDefaultAntiVariable t1 t2

generalizeType :: [SchemaSkeleton] -> TyclassAssignment -> TypeSkeleton -> StateT TypeClassState IO [String]
generalizeType exTyps tcass t = do
    -- do the filtering inside observeAll
    liftIO $ print t
    liftIO $ print tcass
    typs <- observeAllT (do
        let (_, vars) = partition ((==) existTypeVarPrefix . head) $ Set.toList $ typeVarsOf t
        (ass, gt) <- evalStateT (generalizeSType tcass t) (TypeNaming Map.empty Set.empty (Set.fromList vars))
        let (freeVars, vars) = partition ((==) existTypeVarPrefix . head) $ Set.toList $ typeVarsOf gt
        -- simplify computation here
        let mkSubst t = Map.fromList $ map (,t) freeVars
        if null vars && null freeVars
           then {- liftIO (checkUnify gt) >>= guard >> -} return (ass, gt)
           else msum $ map (\v -> do
                let t = typeSubstitute (mkSubst (TypeVarT v)) gt
                -- liftIO (checkUnify t) >>= guard
                guard (isInhabited ass t)
                return $ (ass, t)) vars
        )
    return $ map addTyclasses $
             sortOn (uncurry $ scoreSignature t) $
             nubOrdBy dedupArgs typs
    where
        checkUnify typ = do
            let vars = typeVarsOf typ
            let sch = foldr ForallT (Monotype typ) vars
            checkResults <- mapM (\s -> checkTypes emptyEnv s sch) exTyps
            return $ all fst checkResults
        toConstraint (id, s) = intercalate ", " $ map (unwords . (:[id])) (Set.toList s)
        addTyclasses (constraints, t) =
            let tyclasses = intercalate ", " $
                            filter (not . null) $
                            map toConstraint $
                            Map.toList constraints
             in if null tyclasses then show t
                                  else printf "(%s) => %s" tyclasses (show t)

generalizeSType :: TyclassAssignment -> TypeSkeleton -> TypeGeneralizer IO (TyclassAssignment, TypeSkeleton)
generalizeSType tcass t@(TypeVarT id) = (do
    -- either add type class or not 
    let tc = maybe [] Set.toList (Map.lookup id tcass)
    let mkResult tc = {- guard (checkImplies tcs) >> -} return (Map.singleton id (Set.singleton tc), t)
    prevVars <- gets $ view prevTypeVars
    if id `Set.member` prevVars
       then return (Map.empty, t)
       else do
            modify $ over prevTypeVars (Set.insert id)
            msum (map mkResult tc) `mplus` return (Map.empty, t)
    ) `mplus` (do
        -- or replace the variable with previous names
        -- type classes constraints lost
        prevVars <- gets $ view beginTypeVars
        let prevVars' = Set.delete id prevVars
        msum $ map (return . (Map.empty,) . TypeVarT) (Set.toList prevVars')
        )
generalizeSType tcass t@(FunctionT x tArg tRes) = do
    (argTyclass, tArg') <- generalizeSType tcass tArg
    (resTyclass, tRes') <- generalizeSType tcass tRes
    let tyclassAssignment = Map.foldrWithKey (Map.insertWith Set.union) resTyclass argTyclass
    return (tyclassAssignment, FunctionT x tArg' tRes')
generalizeSType tcass t@(DatatypeT _) = datatypeToVar tcass t `mplus` return (tcass, t)
generalizeSType tcass t@(TyAppT tFun tArg) = (do
        let (dt, args) = collectArgs t
        generalizedArgs <- mapM (generalizeSType tcass) args
        let (tyclassMaps, args') = unzip generalizedArgs
        let argTyclasses = foldr (Map.foldrWithKey (Map.insertWith Set.union)) Map.empty tyclassMaps
        return (argTyclasses, foldl' TyAppT (DatatypeT dt) args')
    ) `mplus`
    if (show t == "[Char]") then datatypeToVar tcass t else mzero
generalizeSType _ t = error $ "unsupported type " ++ show t

-- | TODO: this does not seem to be correct, please double check it later
datatypeToVar :: TyclassAssignment -> TypeSkeleton -> TypeGeneralizer IO (TyclassAssignment, TypeSkeleton)
datatypeToVar tcass t = do
    -- generalize the data type into a fresh type variable
    -- or reuse previous variables if tyclass checks
    tccache <- lift $ lift $ gets $ view tyclassCache
    typeCounting <- gets $ view substCounter
    prevVars <- gets $ view prevTypeVars
    beginVars <- gets $ view beginTypeVars
    dtclasses <- case Map.lookup t tccache of
                    Just tc -> return tc
                    Nothing -> lift . lift $ Set.toList <$> getDtTyclasses tcass t
    -- TODO: what is the good option to post-filter this cases
    -- because it is not always correct
    let matchedPrevVars = filter (\v -> case Map.lookup v tcass of
            Just tc -> not (Set.null (tc `Set.intersection` Set.fromList dtclasses))
            Nothing -> False) (Set.toList beginVars)
    let (v, startIdx) = case Map.lookup t typeCounting of
                            Just (v, i) -> (v, i)
                            Nothing -> let vars = Set.toList prevVars ++ map fst (Map.elems typeCounting)
                                           currMaxOrd = maximum (ord 'a' : map (ord . head) vars)
                                        in ([chr (currMaxOrd + 1)], 0)
    -- choose between incr the index or not
    -- if reusing some previous var, do not backtrack over type classes
    msum (map (\v -> return (Map.empty, TypeVarT v)) matchedPrevVars) `mplus`
      msum (map (\idx -> return (Map.empty, TypeVarT (v ++ show idx))) [1..startIdx]) `mplus`
        -- if start a new index
        if startIdx < 2 -- optimization for eval only
            then do
                let varName = v ++ show (startIdx + 1)
                modify $ over substCounter $ Map.insert t (v, startIdx + 1)
                return (Map.empty, TypeVarT varName) `mplus`
                    (msum $ map (\tc -> return (Map.singleton varName (Set.singleton tc), TypeVarT varName)) dtclasses)
            else mzero

swapAssignments :: AntiUnifier IO ()
swapAssignments = do
    tass1 <- gets $ view typeAssignment1
    tass2 <- gets $ view typeAssignment2
    modify $ set typeAssignment1 tass2
    modify $ set typeAssignment2 tass1

-- check the type class constraints for @t1@ and @t2@
-- return a new type variable with the calculated type classes
checkTyclass :: TypeSkeleton -> TypeSkeleton -> AntiUnifier IO TypeSkeleton
checkTyclass t1@(TypeVarT id1) t2@(TypeVarT id2) = do
    -- assume both type variables cannot be freely unified
    tcass <- gets $ view tyclassAssignment
    let tc1 = Map.findWithDefault Set.empty id1 tcass
    let tc2 = Map.findWithDefault Set.empty id2 tcass
    let tc = tc1 `Set.intersection` tc2
    let mbTyclass = if Set.null tc then Nothing else Just tc
    newAntiVariable t1 t2 mbTyclass
checkTyclass t1@(TypeVarT id) t2 = do
    -- check the type class for the data type
    tcass <- gets $ view tyclassAssignment
    tccache <- lift $ gets $ view tyclassCache
    let tc1 = Map.findWithDefault Set.empty id tcass
    tc <- case Map.lookup t2 tccache of
        Just tc2 -> return $ tc1 `Set.intersection` Set.fromList tc2
        Nothing -> do
            tc2 <- lift $ getDtTyclasses tcass t2
            return $ tc1 `Set.intersection` tc2
    let mbTyclass = if Set.null tc then Nothing else Just tc
    newAntiVariable t1 t2 mbTyclass
checkTyclass t1 t2@(TypeVarT _) = do
    swapAssignments
    result <- checkTyclass t2 t1
    swapAssignments
    return result
checkTyclass t1 t2 = do
    tccache <- lift $ gets $ view tyclassCache
    tcass <- gets $ view tyclassAssignment
    tc1 <- maybe (lift $ getDtTyclasses tcass t1) (return . Set.fromList) (Map.lookup t1 tccache)
    tc2 <- maybe (lift $ getDtTyclasses tcass t2) (return . Set.fromList) (Map.lookup t2 tccache)
    let tc = tc1 `Set.intersection` tc2
    let mbTyclass = if Set.null tc then Nothing else Just tc
    newAntiVariable t1 t2 mbTyclass

getDtTyclasses :: TyclassAssignment -> TypeSkeleton -> StateT TypeClassState IO TyclassConstraints
getDtTyclasses tcass typ = do
    mbCandidates <- mapM (mkTyclassQuery tcass typ) supportedTyclasses
    let tc = map fromJust $ filter isJust mbCandidates
    modify $ over tyclassCache (Map.insert typ tc)
    return $ Set.fromList tc

mkTyclassQuery :: TyclassAssignment -> TypeSkeleton -> String -> StateT TypeClassState IO (Maybe String)
mkTyclassQuery tcass typ tyclass = do
    mdls <- gets $ view supportModules
    let vars = Set.toList $ typeVarsOf typ
    -- this is unuseful now, consider removing it
    let varTcs = concatMap (\v -> case Map.lookup v tcass of
                                    Just tc -> map (v,) $ Set.toList tc
                                    Nothing -> []) vars
    let varTcStrs = map (\(v, tc) -> unwords [tc, v]) varTcs
    let allTcs = intercalate ", " $ varTcStrs ++ [printf "%s (%s)" tyclass (show typ)]
    let query = printf "undefined :: (%s) => ()" allTcs
    liftIO $
        catch (askGhc mdls (exprType TM_Default query) >> return (Just tyclass))
              (\(e :: SomeException) -> liftIO (print e) >> return Nothing)

defaultTypeVar :: TypeSkeleton -> TypeSkeleton -> TypeSkeleton
defaultTypeVar to (TypeVarT v) | existTypeVarPrefix == head v = to
defaultTypeVar to (TyAppT tFun tArg) = TyAppT tFun' tArg'
    where
        tFun' = defaultTypeVar to tFun
        tArg' = defaultTypeVar to tArg
defaultTypeVar to (TyFunT tArg tRes) = TyAppT tArg' tRes'
    where
        tArg' = defaultTypeVar to tArg
        tRes' = defaultTypeVar to tRes
defaultTypeVar to (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = defaultTypeVar to tArg
        tRes' = defaultTypeVar to tRes
defaultTypeVar _ t = t

-- partition all the type variables in the given type
-- into negative set and positive set
reversePolarity reversed t = if isFunctionType t then not reversed else reversed

typeVarPolarity :: Bool -> TypeSkeleton -> (Set Id, Set Id)
typeVarPolarity reversed (TypeVarT v) =
    if reversed then (Set.empty, Set.singleton v)
                else (Set.singleton v, Set.empty)
typeVarPolarity reversed (DatatypeT _) = (Set.empty, Set.empty)
typeVarPolarity reversed (TyAppT tFun tArg) = (neg `Set.union` neg', pos `Set.union` pos')
    where
        (neg, pos) = typeVarPolarity (reversePolarity reversed tFun) tFun
        (neg', pos') = typeVarPolarity (reversePolarity reversed tArg) tArg
typeVarPolarity reversed (TyFunT tArg tRes) = (neg `Set.union` neg', pos `Set.union` pos')
    where
        (neg, pos) = typeVarPolarity (reversePolarity reversed tArg) tArg
        (neg', pos') = typeVarPolarity (reversePolarity reversed tRes) tRes
typeVarPolarity reversed (FunctionT x tArg tRes) = (neg `Set.union` neg', pos `Set.union` pos')
    where
        (neg, pos) = typeVarPolarity (reversePolarity reversed tArg) tArg
        (neg', pos') = typeVarPolarity (not $ reversePolarity reversed tRes) tRes

notOnlyPositive :: TypeSkeleton -> Bool
notOnlyPositive t = let (neg, pos) = typeVarPolarity False t
                     in Set.null (pos `Set.difference` neg)

isInhabited :: TyclassAssignment -> TypeSkeleton -> Bool
isInhabited tyclass t =
    let tcArgs = concatMap tyclassesToArgs $ Map.toList tyclass
        args = map snd (argsWithName t)
        res = lastType t
     in isReachable args res && all (isRelevant (tcArgs ++ args) res) args
    where
        tyclassToArg v tc
            | tc == "Eq" || tc == "Ord" = FunctionT "" (TypeVarT v) (FunctionT "" (TypeVarT v) (DatatypeT "Bool"))
            | tc == "Num" = FunctionT "" (TypeVarT v) (FunctionT "" (TypeVarT v) (TypeVarT v))
        tyclassesToArgs (v, tcs) = map (uncurry tyclassToArg . (v,)) $ Set.toList tcs

isReachable :: [TypeSkeleton] -> TypeSkeleton -> Bool
isReachable args res = typeVarsOf res `Set.isSubsetOf` Set.unions (map typeVarsOf args)

isRelevant :: [TypeSkeleton] -> TypeSkeleton -> TypeSkeleton -> Bool
isRelevant args res (TypeVarT id) = (id `Set.member` (typeVarsOf res)) || usedInHigherOrder
    where
        hoArgs = filter isFunctionType args ++ concatMap hoArgsOf args
        usedInHigherOrder = any relevantToHigherOrder hoArgs
        relevantToHigherOrder hoArg = let argsOfHoArg = map snd (argsWithName hoArg)
                                          tvInArgs = Set.unions $ map typeVarsOf argsOfHoArg
                                       in (id `Set.member` tvInArgs) && isRelevant args res hoArg
isRelevant args res t@FunctionT {} =
    let argsOfHoArg = map snd (argsWithName t)
        resOfHoArg = lastType t
        containedArgs = containsType t args
        remainingArgs = args \\ containedArgs
        argsReachable = all (isReachable remainingArgs) argsOfHoArg
        resRelevant = isRelevant (resOfHoArg : remainingArgs) res resOfHoArg
     in argsReachable && resRelevant
isRelevant _ _ _ = True

checkImplies :: [Id] -> Bool
checkImplies tcs = "Ord" `notElem`tcs || "Eq" `notElem` tcs

dedupArgs :: (TyclassAssignment, TypeSkeleton) -> (TyclassAssignment, TypeSkeleton) -> Ordering
dedupArgs (tc1, t1) (tc2, t2) = let args1 = map snd (argsWithName t1)
                                    args2 = map snd (argsWithName t2)
                                    ret1 = lastType t1
                                    ret2 = lastType t2
                                 in compare (sort args1, ret1, tc1) (sort args2, ret2, tc2)

reverseSubstitution :: TypeSkeleton -> TypeSkeleton -> Map TypeSkeleton (Set TypeSkeleton) -> Map TypeSkeleton (Set TypeSkeleton)
reverseSubstitution t1@(TypeVarT _) t2 tass = Map.insertWith Set.union t1 (Set.singleton t2) tass
reverseSubstitution t1 t2@(TypeVarT _) tass = Map.insertWith Set.union t1 (Set.singleton t2) tass
reverseSubstitution t1@(TyAppT tFun1 tArg1) t2@(TyAppT tFun2 tArg2) tass
    | null args1 && null args2 = Map.insertWith Set.union t1 (Set.singleton t2) tass
    | dt1 == dt2 = foldr (uncurry reverseSubstitution) tass (zip args1 args2)
    | dt1 /= dt2 = error "datatype was lost during generalization"
    where
        (dt1, args1) = collectArgs t1
        (dt2, args2) = collectArgs t2
reverseSubstitution t1@(TyFunT tArg1 tRes1) t2@(TyFunT tArg2 tRes2) tass =
    reverseSubstitution tRes1 tRes2 tass'
    where
        tass' = reverseSubstitution tArg1 tArg2 tass
reverseSubstitution (FunctionT _ tArg1 tRes1) (FunctionT _ tArg2 tRes2) tass =
    reverseSubstitution tRes1 tRes2 tass'
    where
        tass' = reverseSubstitution tArg1 tArg2 tass

scoreSignature :: TypeSkeleton -> TyclassAssignment -> TypeSkeleton -> Double
scoreSignature auType tyclass t =
    let subst = reverseSubstitution auType t Map.empty
        tcCount = 0.5 * fromIntegral (Map.foldr ((+) . Set.size) 0 tyclass)
        varsCount = Set.size $ typeVarsOf t
        substCount k = Set.size
     in fromIntegral (Map.foldrWithKey (\k v -> (+) $ substCount k v) 0 subst) + -- penalty for more than one non-identity subst
         tcCount - 0.1 * fromIntegral varsCount
