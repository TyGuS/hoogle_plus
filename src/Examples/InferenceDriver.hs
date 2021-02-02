{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.InferenceDriver(
    parseExample,
    getExampleTypes
    ) where

import Database.Convert (addTrue)
import Database.Util
import Types.Common
import Types.Type
import Types.IOFormat
import Types.InfConstraint
import Types.Environment
import Types.CheckMonad
import qualified Types.TypeChecker
import Examples.Utils
import Synquid.Type
import PetriNet.Util
import Synquid.Pretty
import Synquid.Logic (ftrue)

import Control.Concurrent.Chan
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
parseExample :: [String] -> String -> IO (Either ErrorMessage RSchema)
parseExample mdls mkFun = catch (do
    typ <- askGhc mdls $ exprType TM_Default mkFun
    let hsType = typeToLHsType typ
    let hpType = toInt $ resolveType hsType
    return (Right hpType))
    (\(e :: SomeException) -> return (Left $ show e))
    where
        toInt (ForallT x t) = ForallT x (toInt t)
        toInt (Monotype t) = Monotype (integerToInt t)

getExampleTypes :: Environment -> [Id] -> [RSchema] -> Int -> IO ([String], InfStats)
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
            let validTypes = map shape freshTypes
            foldM stepAntiUnification (head validTypes, emptyAntiUnifState) (tail validTypes)
        forall t = let vars = typeVarsOf t
                    in foldr ForallT (Monotype $ addTrue t) vars
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
                renamedTyp = stypeSubstitute (Map.map vart_ varSubst) t
                substInConstraint = \(id, s) -> (Map.findWithDefault id id varSubst, s)
                renamedConstraints = map substInConstraint (Map.toList constraints)
                strTyp = showSTypeWithName renamedTyp
             in if null tyclasses then strTyp
                                  else printf "(%s) => %s" tyclasses strTyp

        addArgNames [] t = t
        addArgNames (arg:args) (FunctionT _ tArg tRes) = 
            FunctionT arg tArg (addArgNames args tRes)
        addArgNames _ _ = error "unmatched number of arguments between argNames and query type"

        padding t xs = zip (repeat (fst t)) xs
        flattenResult (antiUnifs, generals) = zipWith padding antiUnifs generals

-- turn a GHC type into Hoogle+ type
resolveType :: LHsType GhcPs -> RSchema
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

        prefixTyclass tc@(ScalarT (DatatypeT name args rs) r) =
            ScalarT (DatatypeT (tyclassPrefix ++ name) args rs) r
        prefixTyclass tc = error $ "Unsupported type class " ++ show tc

        bodyWithTcArgs = foldr (FunctionT "") (toMonotype $ resolveType body) tyConstraints
resolveType t = Monotype $ resolveType' t

resolveType' :: LHsType GhcPs -> RType
resolveType' (L _ (HsFunTy _ f r)) = FunctionT "" (resolveType' f) (resolveType' r)
resolveType' (L _ (HsQualTy _ _ t)) = resolveType' t
resolveType' (L _ (HsTyVar _ _ (L _ v))) =
    if isLower (head name)
       then ScalarT (TypeVarT Map.empty (wildcardPrefix:name)) ftrue
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

        datatypeOf (L _ (HsAppTy _ f _)) = datatypeOf f
        datatypeOf (L _ (HsTyVar _ _ (L _ v))) = showSDocUnsafe (ppr v)

        datatypeArgs (L _ (HsAppTy _ (L _ HsTyVar {}) a)) = [resolveType' a]
        datatypeArgs (L _ (HsAppTy _ f a)) = datatypeArgs f ++ datatypeArgs a
        datatypeArgs t = [resolveType' t]

resolveType' (L _ (HsListTy _ t)) = ScalarT (DatatypeT "List" [resolveType' t] []) ftrue
resolveType' (L _ (HsTupleTy _ _ ts)) = foldr mkPair basePair otherTyps
    where
        mkPair acc t = ScalarT (DatatypeT "Pair" [acc, t] []) ftrue
        resolveTyps = map resolveType' ts
        (baseTyps, otherTyps) = splitAt (length ts - 2) resolveTyps
        basePair = ScalarT (DatatypeT "Pair" baseTyps []) ftrue
resolveType' (L _ (HsParTy _ t)) = resolveType' t
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

antiUnification :: MonadIO m => SType -> SType -> AntiUnifState -> LogicT (StateT TypeClassState m) (SType, AntiUnifState)
antiUnification t1 t2 st = do
    -- liftIO $ print (t1, t2)
    (t, st) <- runStateT (antiUnification' t1 t2) st
    -- liftIO $ print t
    return (t, st)

newAntiVariable :: MonadIO m => SType -> SType -> Maybe TyclassConstraints -> AntiUnifier m SType
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
    return $ vart_ v

-- there are two cases for create anti-unification variables
-- 1) consider reuse an existing binding if the sub unifies, add unification constraints
-- 2) consider create a new binding, add non-unification constraints
findWithDefaultAntiVariable :: MonadIO m => SType -> SType -> AntiUnifier m SType
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
antiUnification' :: MonadIO m => SType -> SType -> AntiUnifier m SType
antiUnification' t1 t2 | t1 == t2 = return t1
antiUnification' AnyT t = return t
antiUnification' t AnyT = return t
antiUnification' t1@(ScalarT (TypeVarT _ id1) _) t2@(ScalarT (TypeVarT _ id2) _)
  | wildcardPrefix == head id1 = return (vart_ id2) `mplus` findWithDefaultAntiVariable t1 t2
  | wildcardPrefix == head id2 = return (vart_ id1) `mplus` findWithDefaultAntiVariable t1 t2
antiUnification' t1@(ScalarT (TypeVarT _ id) _) t
  | wildcardPrefix == head id = return t `mplus` findWithDefaultAntiVariable t1 t
antiUnification' t tv@(ScalarT (TypeVarT _ id) _)
  | wildcardPrefix == head id = return t `mplus` findWithDefaultAntiVariable t tv
antiUnification' t1@(ScalarT (DatatypeT dt1 args1 _) _) t2@(ScalarT (DatatypeT dt2 args2 _) _)
  | dt1 == dt2 = do
      -- liftIO $ print "*************************************************"
      -- liftIO $ print (args1, args2)
      args' <- mapM (uncurry antiUnification') (zip args1 args2)
      return (ScalarT (DatatypeT dt1 args' []) ())
antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
    tArg <- antiUnification' tArg1 tArg2
    tRes <- antiUnification' tRes1 tRes2
    return $ FunctionT x1 tArg tRes
antiUnification' t1 t2 = findWithDefaultAntiVariable t1 t2

generalizeType :: MonadIO m => [RSchema] -> TyclassAssignment -> SType -> StateT TypeClassState m [(TyclassAssignment, SType)]
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
                let t = stypeSubstitute (mkSubst (vart_ v)) gt
                lift $ modify $ over (infStats . prefilterCounts) (+ 1)
                -- liftIO $ print $ "Generalize into " ++ show t
                guard (isInhabited ass t)
                -- liftIO $ print "this is accepted"
                lift $ modify $ over (infStats . postfilterCounts) (+ 1)
                return (ass, t)) vars
        )
    return $ nubOrdBy dedupArgs typs

generalizeSType :: MonadIO m => TyclassAssignment -> SType -> TypeGeneralizer m (TyclassAssignment, SType)
generalizeSType tcass t@(ScalarT (TypeVarT _ id) _) = do
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
generalizeSType tcass t@(ScalarT (DatatypeT name args _) _) = (do
    generalizedArgs <- mapM (generalizeSType tcass) args
    let (tyclassMaps, args') = unzip generalizedArgs
    let argTyclasses = foldr (Map.foldrWithKey (Map.insertWith Set.union)) Map.empty tyclassMaps
    return (argTyclasses, ScalarT (DatatypeT name args' []) ())
    ) `mplus`
    if (null args) || (show t == "[Char]") then datatypeToVar tcass t else mzero
generalizeSType _ t = error $ "unsupported type " ++ show t

datatypeToVar :: MonadIO m => TyclassAssignment -> SType -> TypeGeneralizer m (TyclassAssignment, SType)
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
checkTyclass :: MonadIO m => SType -> SType -> AntiUnifier m (Maybe (Set Id))
checkTyclass t1@(ScalarT (TypeVarT _ id1) _) t2@(ScalarT (TypeVarT _ id2) _) = do
    -- assume both type variables cannot be freely unified
    tcass <- gets $ view tyclassAssignment
    let tc1 = Map.findWithDefault Set.empty id1 tcass
    let tc2 = Map.findWithDefault Set.empty id2 tcass
    let tc = tc1 `Set.intersection` tc2
    return (if Set.null tc then Nothing else Just tc)
checkTyclass t1@(ScalarT (TypeVarT _ id) _) t2 = do
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
checkTyclass t1 t2@(ScalarT (TypeVarT {}) _) = checkTyclass t2 t1
checkTyclass t1 t2 = do
    tccache <- lift $ gets $ view tyclassCache
    tcass <- gets $ view tyclassAssignment
    tc1 <- maybe (lift . lift $ getDtTyclasses tcass t1) (return . Set.fromList) (Map.lookup t1 tccache)
    tc2 <- maybe (lift . lift $ getDtTyclasses tcass t2) (return . Set.fromList) (Map.lookup t2 tccache)
    let tc = tc1 `Set.intersection` tc2
    return (if Set.null tc then Nothing else Just tc)

getDtTyclasses :: MonadIO m => TyclassAssignment -> SType -> StateT TypeClassState m TyclassConstraints
getDtTyclasses tcass typ = do
    mbCandidates <- mapM (mkTyclassQuery tcass typ) supportedTyclasses
    let tc = map fromJust $ filter isJust mbCandidates
    modify $ over tyclassCache (Map.insert typ tc)
    return $ Set.fromList tc

mkTyclassQuery :: MonadIO m => TyclassAssignment -> SType -> String -> StateT TypeClassState m (Maybe String)
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

defaultTypeVar :: SType -> SType -> SType
defaultTypeVar to (ScalarT (TypeVarT _ v) _) | wildcardPrefix == head v = to
defaultTypeVar to (ScalarT (DatatypeT name args _) _) =
    let args' = map (defaultTypeVar to) args
     in ScalarT (DatatypeT name args' []) ()
defaultTypeVar to (FunctionT x tArg tRes) =
    let tArg' = defaultTypeVar to tArg
        tRes' = defaultTypeVar to tRes
     in FunctionT x tArg' tRes'
defaultTypeVar _ t = t

-- partition all the type variables in the given type
-- into negative set and positive set
reversePolarity reversed t = if isFunctionType t then not reversed else reversed

typeVarPolarity :: Bool -> SType -> (Set Id, Set Id)
typeVarPolarity reversed (ScalarT (TypeVarT _ v) _) =
    if reversed then (Set.empty, Set.singleton v)
                else (Set.singleton v, Set.empty)
typeVarPolarity reversed (ScalarT (DatatypeT _ args _) _) =
    let argPolarities = map (\t -> typeVarPolarity (reversePolarity reversed t) t) args
        (neg, pos) = unzip argPolarities
     in (Set.unions neg, Set.unions pos)
typeVarPolarity reversed (FunctionT x tArg tRes) =
    let (neg, pos) = typeVarPolarity (reversePolarity reversed tArg) tArg
        (neg', pos') = typeVarPolarity (not $ reversePolarity reversed tRes) tRes
     in (neg `Set.union` neg', pos `Set.union` pos')

notOnlyPositive :: SType -> Bool
notOnlyPositive t = let (neg, pos) = typeVarPolarity False t
                     in Set.null (pos `Set.difference` neg)

isInhabited :: TyclassAssignment -> SType -> Bool
isInhabited tyclass t =
    let tcArgs = concatMap tyclassesToArgs $ Map.toList tyclass
        args = snd $ unzip $ argsWithName t
        res = lastType t
     in isReachable args res && all (isRelevant (tcArgs ++ args) res) args
    where
        tyclassToArg v tc
            | tc == "Eq" || tc == "Ord" = FunctionT "" (vart_ v) (FunctionT "" (vart_ v) (ScalarT (DatatypeT "Bool" [] []) ()))
            | tc == "Num" = FunctionT "" (vart_ v) (FunctionT "" (vart_ v) (vart_ v))
        tyclassesToArgs (v, tcs) = map (uncurry tyclassToArg . (v,)) $ Set.toList tcs

isReachable :: [SType] -> SType -> Bool
isReachable args res = typeVarsOf res `Set.isSubsetOf` Set.unions (map typeVarsOf args)

isRelevant :: [SType] -> SType -> SType -> Bool
isRelevant args res (ScalarT (TypeVarT _ id) _) = (id `Set.member` typeVarsOf res) || usedInHigherOrder
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
checkImplies tcs = "Ord" `notElem` tcs || "Eq" `notElem` tcs

dedupArgs :: (TyclassAssignment, SType) -> (TyclassAssignment, SType) -> Ordering
dedupArgs (tc1, t1) (tc2, t2) = let args1 = map snd (argsWithName t1)
                                    args2 = map snd (argsWithName t2)
                                    ret1 = lastType t1
                                    ret2 = lastType t2
                                 in compare (sort args1, ret1, tc1) (sort args2, ret2, tc2)

reverseSubstitution :: SType -> SType -> Map SType (Set SType) -> Map SType (Set SType)
reverseSubstitution t1@(ScalarT (TypeVarT {}) _) t2 tass = Map.insertWith Set.union t1 (Set.singleton t2) tass
reverseSubstitution t1 t2@(ScalarT (TypeVarT {}) _) tass = Map.insertWith Set.union t1 (Set.singleton t2) tass
reverseSubstitution t1@(ScalarT (DatatypeT name1 args1 _) _) t2@(ScalarT (DatatypeT name2 args2 _) _) tass
  | null args1 && null args2 = Map.insertWith Set.union t1 (Set.singleton t2) tass
  | name1 == name2 = foldr (uncurry reverseSubstitution) tass (zip args1 args2)
  | name1 /= name2 = error "datatype was lost during generalization"
reverseSubstitution (FunctionT _ tArg1 tRes1) (FunctionT _ tArg2 tRes2) tass =
    let tass' = reverseSubstitution tArg1 tArg2 tass
     in reverseSubstitution tRes1 tRes2 tass'

scoreSignature :: SType -> TyclassAssignment -> SType -> Double
scoreSignature auType tyclass t =
    let subst = reverseSubstitution auType t Map.empty
        tcCount = 0.5 * fromIntegral (Map.foldr ((+) . Set.size) 0 tyclass)
        varsCount = Set.size $ typeVarsOf t
        substCount k s = Set.size s
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
    let t1' = stypeSubstitute tass t1
    let t2' = stypeSubstitute tass t2
    -- liftIO $ print (t1', t2')
    checkDisunification t1' t2'
    -- liftIO $ print "end checkConstraint"

checkUnification :: MonadIO m => SType -> SType -> AntiUnifier m ()
checkUnification t1 t2 = do
    -- liftIO $ print "checkUnification"
    -- liftIO $ print (t1, t2)
    let vars = Set.toList $ typeVarsOf t1 `Set.union` typeVarsOf t2
    let boundVars = filter ((/=) wildcardPrefix . head) vars
    let env = emptyEnv { _boundTypeVars = boundVars }
    let p1 = addTrue t1
    let p2 = addTrue t2
    tmpAss <- gets $ view tmpAssignment
    names <- getNameCounter
    -- liftIO $ print tmpAss
    -- liftIO $ print (p1, p2)
    (isChecked, tass) <- liftIO $ do
        messageChan <- newChan
        let initChecker = Types.TypeChecker.emptyChecker { 
                            Types.TypeChecker._checkerChan = messageChan
                          , Types.TypeChecker._typeAssignment = tmpAss
                          , Types.TypeChecker._nameCounter = names
                          }
        checkTypes env initChecker vars p1 p2
    guard isChecked
    modify $ set tmpAssignment tass
    -- liftIO $ print "checkUnification end"

checkDisunification :: MonadIO m => SType -> SType -> AntiUnifier m ()
checkDisunification t1 t2 | t1 == t2 = mzero
checkDisunification (ScalarT (TypeVarT _ id) _) _ | wildcardPrefix == head id = return ()
checkDisunification _ (ScalarT (TypeVarT _ id) _) | wildcardPrefix == head id = return ()
checkDisunification (ScalarT (DatatypeT dt1 args1 _) _) (ScalarT (DatatypeT dt2 args2 _) _)
  | dt1 /= dt2 = return ()
  | dt1 == dt2 && null args1 && null args2 = mzero
  | otherwise = checkArgs args1 args2
  where
      checkArgs [] [] = mzero
      checkArgs (arg1:args1) (arg2:args2) = ifte (checkDisunification arg1 arg2) 
                                                 (const (return ()))
                                                 (checkArgs args1 args2)
checkDisunification (FunctionT _ tArg1 tRes1) (FunctionT _ tArg2 tRes2) = do
    checkDisunification tArg1 tArg2
    checkDisunification tRes1 tRes2
checkDisunification _ _ = return ()

hasWildcard :: SType -> Bool
hasWildcard (ScalarT (DatatypeT _ args _) _) = any hasWildcard args
hasWildcard (ScalarT (TypeVarT _ name) _) = wildcardPrefix == head name
hasWildcard (FunctionT _ tArg tRes) = hasWildcard tArg || hasWildcard tRes
hasWildcard _ = False
