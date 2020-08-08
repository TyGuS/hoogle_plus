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
parseExample :: [String] -> String -> IO (Either RSchema ErrorMessage)
parseExample mdls mkFun = catch (do
    typ <- askGhc mdls $ exprType TM_Default mkFun
    let hsType = typeToLHsType typ
    let hpType = toInt $ resolveType hsType
    return (Left hpType))
    (\(e :: SomeException) -> return (Right $ show e))
    where
        toInt (ForallT x t) = ForallT x (toInt t)
        toInt (Monotype t) = Monotype (integerToInt t)

getExampleTypes :: Environment -> [RSchema] -> Int -> IO ([String], InfStats)
getExampleTypes env validSchemas num = do
    let validTypes = map (shape . toMonotype) validSchemas
    let mdls = env ^. included_modules
    let initTyclassState = emptyTyclassState { _supportModules = Set.toList mdls }
    -- remove magic number later
    (typs, st) <- runStateT (do
        (mbFreeVar, st) <- if not (null validTypes)
                then foldM stepAntiUnification (head validTypes, emptyAntiUnifState) (tail validTypes)
                else error "get example types error"
        -- let t = defaultTypeVar mbFreeVar
        let t = mbFreeVar
        let tvars = typeVarsOf t
        let tcass = st ^. tyclassAssignment
        generalizeType validSchemas tcass t
        ) initTyclassState
    return (take num typs, st ^. infStats)
    where
        stepAntiUnification (t1, st) t2 = antiUnification t1 t2 st
        forall t = let vars = typeVarsOf t
                    in foldr ForallT (Monotype $ addTrue t) vars

-- turn a GHC type into Hoogle+ type
resolveType :: LHsType GhcPs -> RSchema
resolveType (L _ (HsForAllTy bs t)) = foldr ForallT (resolveType t) vs
    where
        vs = map vname bs
        vname (L _ (UserTyVar (L _ id))) = showSDocUnsafe (ppr id)
        vname (L _ (KindedTyVar (L _ id) _)) = showSDocUnsafe (ppr id)
resolveType t@(L _ (HsAppTy fun arg)) =
    let typs = tail $ map resolveType' $ breakApp t
        (args, [res]) = splitAt (length typs - 1) typs
     in Monotype $ foldr (FunctionT "") res args
    where
        breakApp (L _ (HsAppTy fun arg)) = breakApp fun ++ [arg]
        breakApp t = [t]
resolveType (L _ (HsQualTy ctx body)) = Monotype bodyWithTcArgs
    where
        unlocatedCtx = let L _ c = ctx in c
        tyConstraints = map (prefixTyclass . resolveType') unlocatedCtx

        prefixTyclass tc@(ScalarT (DatatypeT name args rs) r) =
            ScalarT (DatatypeT (tyclassPrefix ++ name) args rs) r
        prefixTyclass tc = error $ "Unsupported type class " ++ show tc

        bodyWithTcArgs = foldr (FunctionT "") (toMonotype $ resolveType body) tyConstraints
resolveType t = Monotype $ resolveType' t

resolveType' :: LHsType GhcPs -> RType
resolveType' (L _ (HsFunTy f r)) = FunctionT "" (resolveType' f) (resolveType' r)
resolveType' (L _ (HsQualTy _ t)) = resolveType' t
resolveType' (L _ (HsTyVar _ (L _ v))) =
    if isLower (head name)
       then ScalarT (TypeVarT Map.empty (univTypeVarPrefix:name)) ftrue
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

antiUnification :: SType -> SType -> AntiUnifState -> StateT TypeClassState IO (SType, AntiUnifState)
antiUnification t1 t2 st = runStateT (antiUnification' t1 t2) st

newAntiVariable :: SType -> SType -> Maybe TyclassConstraints -> AntiUnifier IO SType
newAntiVariable t1 t2 mbCons = do
    v <- freshId [] "t"
    modify $ over typeAssignment1 (Map.insertWith (++) t1 [v])
    modify $ over typeAssignment2 (Map.insertWith (++) t2 [v])
    when (isJust mbCons) (
        modify $ over tyclassAssignment (Map.insertWith Set.union v (fromJust mbCons))
        )
    return $ vart_ v

findWithDefaultAntiVariable :: SType -> SType -> AntiUnifier IO SType
findWithDefaultAntiVariable t1 t2 = do
    tass1 <- gets $ view typeAssignment1
    tass2 <- gets $ view typeAssignment2
    vs1 <- liftIO $ unifiableVars tass1 t1
    vs2 <- liftIO $ unifiableVars tass2 t2
    let overlap = vs1 `intersect` vs2
    if not (null overlap)
       then return $ vart_ (head overlap)
       else checkTyclass t1 t2
    where
        unifiableVars tass t = do
            messageChan <- newChan
            results <- filterM (\(k, vs) -> do
                let vars = Set.toList $ typeVarsOf k `Set.union` typeVarsOf t
                -- let boundVars = filter (not . (==) univTypeVarPrefix . head) vars
                let env = emptyEnv -- { _boundTypeVars = boundVars }
                (isChecked, newt) <- checkTypes env messageChan (mkPolyType $ addTrue t) (mkPolyType $ addTrue k)
                let notBothVars = case (t, newt) of
                                (ScalarT (TypeVarT _ id1) _, ScalarT (TypeVarT _ id2) _)
                                    | id1 /= id2 -> head id1 == 't' && head id2 == 't'
                                _ -> True
                return (isChecked && notBothVars)) (Map.toList tass)
            return $ concat $ snd $ unzip results

-- antiUnification' :: SType -> SType -> AntiUnifier IO SType
-- antiUnification' t1 t2 | t1 == t2 = return t1
-- antiUnification' AnyT t = return t
-- antiUnification' t AnyT = return t
-- antiUnification' t1@(ScalarT (TypeVarT _ id1) _) t2@(ScalarT (TypeVarT _ id2) _)
--   | univTypeVarPrefix == head id1 = do
--     tass <- gets $ view typeAssignment2
--     if t2 `Map.member` tass
--         then return $ vart_ (head $ tass Map.! t2)
--         else return $ vart_ id2
--   | univTypeVarPrefix == head id2 = do
--     tass <- gets $ view typeAssignment1
--     if t1 `Map.member` tass
--         then return $ vart_ (head $ tass Map.! t1)
--         else return $ vart_ id1
--   -- | otherwise = findWithDefaultAntiVariable t1 t2
-- antiUnification' t1@(ScalarT (TypeVarT _ id) _) t
--   | univTypeVarPrefix == head id = do
--     tass <- gets $ view typeAssignment2
--     if t `Map.member` tass
--         then return $ vart_ (head $ tass Map.! t)
--         else return t
-- antiUnification' t tv@(ScalarT (TypeVarT {}) _) = do
--     swapAssignments
--     result <- antiUnification' tv t
--     swapAssignments
--     return result
-- antiUnification' t1@(ScalarT (DatatypeT dt1 args1 _) _) t2@(ScalarT (DatatypeT dt2 args2 _) _)
--   | dt1 == dt2 = do
--       args' <- mapM (uncurry antiUnification') (zip args1 args2)
--       return $ ScalarT (DatatypeT dt1 args' []) ()
-- antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
--     tArg <- antiUnification' tArg1 tArg2
--     tRes <- antiUnification' tRes1 tRes2
--     return $ FunctionT x1 tArg tRes
-- antiUnification' t1 t2 = findWithDefaultAntiVariable t1 t2
antiUnification' :: SType -> SType -> AntiUnifier IO SType
antiUnification' t1 t2 | t1 == t2 = return t1
antiUnification' AnyT t = return t
antiUnification' t AnyT = return t
antiUnification' t1@(ScalarT (TypeVarT _ id1) _) t2@(ScalarT (TypeVarT _ id2) _)
  | univTypeVarPrefix == head id1 = return $ vart_ id2
  | univTypeVarPrefix == head id2 = return $ vart_ id1
  -- | otherwise = findWithDefaultAntiVariable t1 t2
antiUnification' t1@(ScalarT (TypeVarT _ id) _) t
  | univTypeVarPrefix == head id = return t
  -- | otherwise = findWithDefaultAntiVariable t1 t
antiUnification' t tv@(ScalarT (TypeVarT {}) _) = do
    swapAssignments
    result <- antiUnification' tv t
    swapAssignments
    return result
antiUnification' t1@(ScalarT (DatatypeT dt1 args1 _) _) t2@(ScalarT (DatatypeT dt2 args2 _) _)
  | dt1 == dt2 = do
      args' <- mapM (uncurry antiUnification') (zip args1 args2)
      return $ ScalarT (DatatypeT dt1 args' []) ()
antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
    tArg <- antiUnification' tArg1 tArg2
    tRes <- antiUnification' tRes1 tRes2
    return $ FunctionT x1 tArg tRes
antiUnification' t1 t2 = findWithDefaultAntiVariable t1 t2

generalizeType :: [RSchema] -> TyclassAssignment -> SType -> StateT TypeClassState IO [String]
generalizeType exTyps tcass t = do
    -- do the filtering inside observeAll
    liftIO $ print t
    liftIO $ print tcass
    -- collect stats
    typs <- observeAllT (do
        let (_, vars) = partition ((==) univTypeVarPrefix . head) $ Set.toList $ typeVarsOf t
        (ass, gt) <- evalStateT (generalizeSType tcass t) (TypeNaming Map.empty Map.empty Set.empty (Set.fromList vars))
        -- liftIO $ print (ass, gt)
        let (freeVars, vars) = partition ((==) univTypeVarPrefix . head) $ Set.toList $ typeVarsOf gt
        -- simplify computation here
        let mkSubst t = Map.fromList $ map (,t) freeVars
        if null vars && null freeVars
           then lift (modify $ over (infStats . prefilterCounts) (+ 1)) >>
                lift (modify $ over (infStats . postfilterCounts) (+ 1)) >>
                -- liftIO (print (ass, gt)) >>
                return (ass, gt)
           else msum $ map (\v -> do
                let t = stypeSubstitute (mkSubst (vart_ v)) gt
                lift $ modify $ over (infStats . prefilterCounts) (+ 1)
                -- liftIO (checkUnify t) >>= guard
                -- liftIO $ print (ass, t)
                guard (isInhabited ass t)
                lift $ modify $ over (infStats . postfilterCounts) (+ 1)
                return $ (ass, t)) vars
        )
    return $ map addTyclasses $
             sortOn (uncurry $ scoreSignature t) $
             nubOrdBy dedupArgs typs
    where
        checkUnify typ = do
            let vars = typeVarsOf typ
            let sch = foldr ForallT (Monotype (addTrue typ)) vars
            messageChan <- newChan
            checkResults <- mapM (\s -> checkTypes emptyEnv messageChan s sch) exTyps
            return $ all fst checkResults
        toConstraint (id, s) = intercalate ", " $ map (unwords . (:[id])) (Set.toList s)
        addTyclasses (constraints, t) =
            let tyclasses = intercalate ", " $
                            filter (not . null) $
                            map toConstraint $
                            Map.toList constraints
             in if null tyclasses then show t
                                  else printf "(%s) => %s" tyclasses (show t)

generalizeSType :: TyclassAssignment -> SType -> TypeGeneralizer IO (TyclassAssignment, SType)
-- generalizeSType tcass t@(ScalarT (TypeVarT _ id) _) =
--         enumTyclasses `mplus` enumDupVars
--     where
--         enumTyclasses = do
--             -- either add type class or not 
--             let tc = maybe [] Set.toList (Map.lookup id tcass)
--             let mkResult t tc = {- guard (checkImplies tcs) >> -} return (Map.singleton id (Set.singleton tc), t)
--             prevVars <- gets $ view prevTypeVars
--             if id `Set.member` prevVars -- either enumerate type classes or create a new one
--                then do
--                    return (Map.empty, t)
--                else do
--                    modify $ over prevTypeVars (Set.insert id)
--                    if null tc then return (Map.empty, t)
--                               else (msum $ map (mkResult t) tc) `mplus` return (Map.empty, t)
--         enumDupVars = do
--             beginVars <- gets $ view beginTypeVars
--             prevVars <- gets $ view prevTypeVars
--             -- if id `Set.member` prevVars
--             --     then do
--                     -- or replace the variable with previous names
--                     -- type classes constraints lost
--                     let vars = Set.toList beginVars ++ Set.toList prevVars
--                     v <- freshId vars "t"
--                     modify $ over prevTypeVars (Set.insert v)
--                     let prevVars' = delete id (Set.toList beginVars)
--                     msum $ map (return . (Map.empty,) . vart_) (v:prevVars')
--                 -- else mzero
generalizeSType tcass t@(ScalarT (TypeVarT _ id) _) = (do
    -- either add type class or not 
    let tc = maybe [] Set.toList (Map.lookup id tcass)
    let mkResult tc = {- guard (checkImplies tcs) >> -} return (Map.singleton id (Set.singleton tc), t)
    prevVars <- gets $ view prevTypeVars
    if id `Set.member` prevVars
       then return (Map.empty, t)
       else do
            modify $ over prevTypeVars (Set.insert id)
            (msum $ map mkResult tc) `mplus` return (Map.empty, t)
    ) `mplus` (do
        -- or replace the variable with previous names
        -- type classes constraints lost
        prevVars <- gets $ view beginTypeVars
        let prevVars' = Set.delete id prevVars
        msum $ map (return . (Map.empty,) . vart_) (Set.toList prevVars')
        )
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

datatypeToVar :: TyclassAssignment -> SType -> TypeGeneralizer IO (TyclassAssignment, SType)
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

swapAssignments :: AntiUnifier IO ()
swapAssignments = do
    tass1 <- gets $ view typeAssignment1
    tass2 <- gets $ view typeAssignment2
    modify $ set typeAssignment1 tass2
    modify $ set typeAssignment2 tass1

-- check the type class constraints for @t1@ and @t2@
-- return a new type variable with the calculated type classes
checkTyclass :: SType -> SType -> AntiUnifier IO SType
checkTyclass t1@(ScalarT (TypeVarT _ id1) _) t2@(ScalarT (TypeVarT _ id2) _) = do
    -- assume both type variables cannot be freely unified
    tcass <- gets $ view tyclassAssignment
    let tc1 = Map.findWithDefault Set.empty id1 tcass
    let tc2 = Map.findWithDefault Set.empty id2 tcass
    let tc = tc1 `Set.intersection` tc2
    let mbTyclass = if Set.null tc then Nothing else Just tc
    newAntiVariable t1 t2 mbTyclass
checkTyclass t1@(ScalarT (TypeVarT _ id) _) t2 = do
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
checkTyclass t1 t2@(ScalarT (TypeVarT {}) _) = do
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

getDtTyclasses :: TyclassAssignment -> SType -> StateT TypeClassState IO TyclassConstraints
getDtTyclasses tcass typ = do
    mbCandidates <- mapM (mkTyclassQuery tcass typ) supportedTyclasses
    let tc = map fromJust $ filter isJust mbCandidates
    modify $ over tyclassCache (Map.insert typ tc)
    return $ Set.fromList tc

mkTyclassQuery :: TyclassAssignment -> SType -> String -> StateT TypeClassState IO (Maybe String)
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
defaultTypeVar to (ScalarT (TypeVarT _ v) _) | univTypeVarPrefix == head v = to
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
