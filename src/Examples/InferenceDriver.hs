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
import Types.CheckMonad
import qualified Types.TypeChecker
import Examples.Utils
import Synquid.Type
import PetriNet.Utils
import Synquid.Pretty
import Synquid.Program

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

getExampleTypes :: Environment -> [Id] -> [SchemaSkeleton] -> Int -> IO ([String], InfStats)
getExampleTypes env argNames validSchemas num = do
    let mdls = env ^. included_modules
    let initTyclassState = emptyTyclassState { _supportModules = Set.toList mdls }
    -- remove magic number later
    (typs, st) <- runStateT (do
        mbFreeVars <- observeAllT go 
        mapM (\(t, st) -> zip (repeat t) <$> getGeneralizations t st) mbFreeVars
        ) initTyclassState
    let sortedTyps = sortOn (\(t, (tc, gt)) -> scoreSignature t tc gt) (concat typs)
    let tcTyps = map (addTyclasses . over _2 (addArgNames argNames) . snd) sortedTyps
    return (take num tcTyps, st ^. infStats)
    where
        stepAntiUnification (t1, st) t2 = antiUnification t1 t2 st
        go = do
            -- liftIO $ print validSchemas
            -- create types with all fresh type variables
            freshTypes <- mapM (freshType []) validSchemas
            foldM stepAntiUnification (head freshTypes, emptyAntiUnifState) (tail freshTypes)
        forall t = let vars = typeVarsOf t
                    in foldr ForallT (Monotype t) vars
        getGeneralizations t st = do
            let tvars = typeVarsOf t
            let tcass = st ^. tyclassAssignment
            -- either contains wildcards or is inhabited
            if hasWildcard t || isInhabited tcass t
               then generalizeType validSchemas tcass t
               else return []
        toConstraint (id, s) = intercalate ", " $ map (unwords . (:[id])) (Set.toList s)
        
        addTyclasses (constraints, t) =
            let tyclasses = intercalate ", " $
                            filter (not . null) $
                            map toConstraint renamedConstraints
                vars = Set.toList (typeVarsOf t)
                letters = map (:[]) ['a'..'z']
                varCandidates = filter (`notElem` vars) letters
                varSubst = Map.fromList (filter (uncurry (/=)) $ zip vars varCandidates)
                renamedTyp = typeSubstitute (Map.map vart_ varSubst) t
                substInConstraint = \(id, s) -> (Map.findWithDefault id id varSubst, s)
                renamedConstraints = map substInConstraint (Map.toList constraints)
                strTyp = showTypeWithName renamedTyp
             in if null tyclasses then strTyp
                                  else printf "(%s) => %s" tyclasses strTyp

        addArgNames [] t = t
        addArgNames (arg:args) (FunctionT _ tArg tRes) = 
            FunctionT arg tArg (addArgNames args tRes)
        addArgNames _ _ = error "unmatched number of arguments between argNames and query type"

        padding t xs = zip (repeat (fst t)) xs
        flattenResult (antiUnifs, generals) = zipWith padding antiUnifs generals

-- turn a GHC type into Hoogle+ type
resolveType :: LHsType GhcPs -> SchemaSkeleton
resolveType (L _ (HsForAllTy _ _ bs t)) = foldr ForallT (resolveType t) vs
    where
        vs = map ((wildcardPrefix:) . vname) bs
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
       then TypeVarT (wildcardPrefix:name)
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

antiUnification :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifState -> LogicT (StateT TypeClassState m) (TypeSkeleton, AntiUnifState)
antiUnification t1 t2 = runStateT (antiUnification' t1 t2)

newAntiVariable :: MonadIO m => TypeSkeleton -> TypeSkeleton -> Maybe TyclassConstraints -> AntiUnifier m TypeSkeleton
newAntiVariable t1 t2 mbCons = do
    -- liftIO $ print "newAntiVariable start"
    -- liftIO $ print (t1, t2)
    -- liftIO $ print "newAntiVariable end"
    -- check whether the disunification is satisfiable
    -- liftIO $ print $ "Adding disunification constraint " ++ show (t1, t2)
    modify $ over disunifConstraints (DisunifConstraint t1 t2 :)
    checkConstraints
    -- if the disunification is satisfied, assign a new binding
    v <- freshId [] "t"
    modify $ over typeAssignment (Map.insert (t1, t2) v)
    when (isJust mbCons) (
        modify $ over tyclassAssignment (Map.insertWith Set.union v (fromJust mbCons))
        )
    return $ TypeVarT v

-- there are two cases for create anti-unification variables
-- 1) consider reuse an existing binding if the sub unifies, add unification constraints
-- 2) consider create a new binding, add non-unification constraints
findWithDefaultAntiVariable :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m TypeSkeleton
findWithDefaultAntiVariable t1 t2 = do
    tass <- gets $ view typeAssignment
    -- liftIO $ print $ "current anti unification assignment " ++ show tass
    msum (map (unifiableVar (t1, t2)) (Map.toList tass)) `mplus` -- reuse type variables
        (checkTyclass t1 t2 >>= newAntiVariable t1 t2) -- create new variables
    where
        -- we only allow unification between wildcards, not for anti-unification variables
        unifyAndCheck t1 t2 = do
            checkUnification t1 t2
            -- liftIO $ print $ "Adding unification constraint " ++ show (t1, t2)
            modify $ over unifConstraints (UnifConstraint t1 t2 :)
            checkConstraints
        unifiableVar (t1, t2) ((k1, k2), v) = do
            unifyAndCheck t1 k1
            unifyAndCheck t2 k2
            return (vart_ v)

-- there are two cases for antiUnification
-- 1) one of the type is a wildcard, return the other type
-- 2) either find an existing binding or create a new one
antiUnification' :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m TypeSkeleton
antiUnification' t1 t2 | t1 == t2 = return t1
antiUnification' AnyT t = return t
antiUnification' t AnyT = return t
antiUnification' t1@(TypeVarT id1) t2@(TypeVarT id2)
  | wildcardPrefix == head id1 = return (vart_ id2) `mplus` findWithDefaultAntiVariable t1 t2
  | wildcardPrefix == head id2 = return (vart_ id1) `mplus` findWithDefaultAntiVariable t1 t2
antiUnification' t1@(TypeVarT id) t
  | wildcardPrefix == head id = return t `mplus` findWithDefaultAntiVariable t1 t
antiUnification' t tv@(TypeVarT id)
  | wildcardPrefix == head id = return t `mplus` findWithDefaultAntiVariable t tv
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

generalizeType :: MonadIO m => [SchemaSkeleton] -> TyclassAssignment -> TypeSkeleton -> StateT TypeClassState m [(TyclassAssignment, TypeSkeleton)]
generalizeType exTyps tcass t = do
    -- do the filtering inside observeAll
    liftIO $ print t
    -- liftIO $ print tcass
    -- collect stats
    typs <- observeAllT (do
        let (_, vars) = partition ((==) wildcardPrefix . head) $ Set.toList $ typeVarsOf t
        (ass, gt) <- evalStateT (generalizeSType tcass t) (TypeNaming Map.empty Map.empty Set.empty (Set.fromList vars))
        let (freeVars, vars) = partition ((==) wildcardPrefix . head) $ Set.toList $ typeVarsOf gt
        -- simplify computation here
        let mkSubst t = Map.fromList $ map (,t) freeVars
        if null vars && null freeVars
           then lift (modify $ over (infStats . prefilterCounts) (+ 1)) >>
                lift (modify $ over (infStats . postfilterCounts) (+ 1)) >>
                return (ass, gt)
           else msum $ map (\v -> do
                let t = typeSubstitute (mkSubst (vart_ v)) gt
                lift $ modify $ over (infStats . prefilterCounts) (+ 1)
                -- liftIO $ print $ "Generalize into " ++ show t
                guard (isInhabited ass t)
                -- liftIO $ print "this is accepted"
                lift $ modify $ over (infStats . postfilterCounts) (+ 1)
                return (ass, t)) vars
        )
    return $ nubOrdBy dedupArgs typs

generalizeSType :: MonadIO m => TyclassAssignment -> TypeSkeleton -> TypeGeneralizer m (TyclassAssignment, TypeSkeleton)
generalizeSType tcass t@(TypeVarT id) = do
    -- either add type class or not 
    let tc = maybe [] Set.toList (Map.lookup id tcass)
    let mkResult tc = return (Map.singleton id (Set.singleton tc), t)
    prevVars <- gets $ view prevTypeVars
    if id `Set.member` prevVars
       then return (Map.empty, t)
       else do
            modify $ over prevTypeVars (Set.insert id)
            (msum $ map mkResult tc) `mplus` return (Map.empty, t)
generalizeSType tcass t@(FunctionT x tArg tRes) = do
    (argTyclass, tArg') <- generalizeSType tcass tArg
    (resTyclass, tRes') <- generalizeSType tcass tRes
    let tyclassAssignment = Map.foldrWithKey (Map.insertWith Set.union) resTyclass argTyclass
    return (tyclassAssignment, FunctionT x tArg' tRes')
generalizeSType tcass t@(TyFunT tArg tRes) = do
    (argTyclass, tArg') <- generalizeSType tcass tArg
    (resTyclass, tRes') <- generalizeSType tcass tRes
    let tyclassAssignment = Map.foldrWithKey (Map.insertWith Set.union) resTyclass argTyclass
    return (tyclassAssignment, TyFunT tArg' tRes')
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

datatypeToVar :: MonadIO m => TyclassAssignment -> TypeSkeleton -> TypeGeneralizer m (TyclassAssignment, TypeSkeleton)
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
    msum (map (\v -> do
            modify $ over prevTypeVars (Set.insert v)
            return (Map.empty, vart_ v)) matchedPrevVars) `mplus`
      msum (map (\idx -> return (Map.empty, vart_ (v ++ show idx))) [1..startIdx]) `mplus`
        -- if start a new index
        if startIdx < 3 -- optimization for eval only
            then
            do
                let varName = v ++ show (startIdx + 1)
                -- liftIO $ print varName
                modify $ over substCounter $ Map.insert t (v, startIdx + 1)
                modify $ over nameCounter $ Map.insert v (startIdx + 1)
                return (Map.empty, vart_ varName) `mplus`
                    (msum $ map (\tc -> return (Map.singleton varName (Set.singleton tc), vart_ varName)) dtclasses)
            else mzero

-- check the type class constraints for @t1@ and @t2@
-- return a new type variable with the calculated type classes
checkTyclass :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m (Maybe (Set Id))
checkTyclass t1@(TypeVarT id1) t2@(TypeVarT id2) = do
    -- assume both type variables cannot be freely unified
    tcass <- gets $ view tyclassAssignment
    let tc1 = Map.findWithDefault Set.empty id1 tcass
    let tc2 = Map.findWithDefault Set.empty id2 tcass
    let tc = tc1 `Set.intersection` tc2
    return (if Set.null tc then Nothing else Just tc)
checkTyclass t1@(TypeVarT id) t2 = do
    -- check the type class for the data type
    tcass <- gets $ view tyclassAssignment
    tccache <- lift $ gets $ view tyclassCache
    let tc1 = Map.findWithDefault Set.empty id tcass
    tc <- case Map.lookup t2 tccache of
        Just tc2 -> return $ tc1 `Set.intersection` Set.fromList tc2
        Nothing -> do
            tc2 <- lift . lift $ getDtTyclasses tcass t2
            return $ tc1 `Set.intersection` tc2
    return (if Set.null tc then Nothing else Just tc)
checkTyclass t1 t2@(TypeVarT {}) = checkTyclass t2 t1
checkTyclass t1 t2 = do
    tccache <- lift $ gets $ view tyclassCache
    tcass <- gets $ view tyclassAssignment
    tc1 <- maybe (lift . lift $ getDtTyclasses tcass t1) (return . Set.fromList) (Map.lookup t1 tccache)
    tc2 <- maybe (lift . lift $ getDtTyclasses tcass t2) (return . Set.fromList) (Map.lookup t2 tccache)
    let tc = tc1 `Set.intersection` tc2
    return (if Set.null tc then Nothing else Just tc)

getDtTyclasses :: MonadIO m => TyclassAssignment -> TypeSkeleton -> StateT TypeClassState m TyclassConstraints
getDtTyclasses tcass typ = do
    mbCandidates <- mapM (mkTyclassQuery tcass typ) supportedTyclasses
    let tc = map fromJust $ filter isJust mbCandidates
    modify $ over tyclassCache (Map.insert typ tc)
    return $ Set.fromList tc

mkTyclassQuery :: MonadIO m => TyclassAssignment -> TypeSkeleton -> String -> StateT TypeClassState m (Maybe String)
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
defaultTypeVar to (TypeVarT v) | wildcardPrefix == head v = to
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

checkConstraints :: MonadIO m => AntiUnifier m ()
checkConstraints = do
    -- first call type checker for unification constraints
    -- liftIO $ print "start unification"
    unifs <- gets $ view unifConstraints
    mapM_ checkConstraint unifs
    -- then call type checker for disunification constraints, with the previous unifier
    -- liftIO $ print "start disunification"
    disunifs <- gets $ view disunifConstraints
    mapM_ checkConstraint disunifs

checkConstraint :: MonadIO m => AntiUnifConstraint -> AntiUnifier m ()
checkConstraint (UnifConstraint t1 t2) = do
    -- liftIO (print "checkUnif")
    -- liftIO (print (t1, t2))
    checkUnification t1 t2
checkConstraint (DisunifConstraint t1 t2) = do
    -- liftIO $ print "checkDisunif"
    tass <- gets $ view tmpAssignment
    -- liftIO $ print tass
    let t1' = typeSubstitute tass t1
    let t2' = typeSubstitute tass t2
    -- liftIO $ print (t1', t2')
    checkDisunification t1' t2'
    -- liftIO $ print "end checkConstraint"

checkUnification :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m ()
checkUnification t1 t2 = do
    -- liftIO $ print "checkUnification"
    -- liftIO $ print (t1, t2)
    let vars = Set.toList $ typeVarsOf t1 `Set.union` typeVarsOf t2
    let boundVars = filter ((/=) wildcardPrefix . head) vars
    let env = emptyEnv { _boundTypeVars = boundVars }
    tmpAss <- gets $ view tmpAssignment
    names <- getNameCounter
    -- liftIO $ print tmpAss
    -- liftIO $ print (p1, p2)
    (isChecked, tass) <- liftIO $ do
        let initChecker = Types.TypeChecker.emptyChecker {
                            Types.TypeChecker._typeAssignment = tmpAss
                          , Types.TypeChecker._nameCounter = names
                          }
        checkTypes env initChecker vars t1 t2
    guard isChecked
    modify $ set tmpAssignment tass
    -- liftIO $ print "checkUnification end"

-- whether there exists a substitution such that 
-- the two types does not unify with each other
checkDisunification :: MonadIO m => TypeSkeleton -> TypeSkeleton -> AntiUnifier m ()
checkDisunification t1 t2 | t1 == t2 = mzero
checkDisunification (TypeVarT id) _ | wildcardPrefix == head id = return ()
checkDisunification _ (TypeVarT id) | wildcardPrefix == head id = return ()
checkDisunification (TyAppT tFun1 tArg1) (TyAppT tFun2 tArg2) = do
    -- we cannot answer "NO" when only looking at the "fun" part
    ifte (checkDisunification tFun1 tFun2)
         (const (return ()))
         (checkDisunification tArg1 tArg2)
checkDisunification (TyFunT tArg1 tRes1) (TyFunT tArg2 tRes2) = do
    ifte (checkDisunification tArg1 tArg2)
         (const (return ()))
         (checkDisunification tRes1 tRes2)
checkDisunification (FunctionT _ tArg1 tRes1) (FunctionT _ tArg2 tRes2) = do
    ifte (checkDisunification tArg1 tArg2)
         (const (return ()))
         (checkDisunification tRes1 tRes2)
checkDisunification _ _ = return ()

hasWildcard :: TypeSkeleton -> Bool
hasWildcard (TypeVarT v) = wildcardPrefix == head v
hasWildcard (TyAppT tFun tArg) = hasWildcard tFun || hasWildcard tArg
hasWildcard (TyFunT tArg tRes) = hasWildcard tArg || hasWildcard tRes
hasWildcard (FunctionT _ tArg tRes) = hasWildcard tArg || hasWildcard tRes
hasWildcard _ = False
