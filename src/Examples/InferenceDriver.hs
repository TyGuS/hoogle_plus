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

import Control.Exception
import Control.Monad.State
import Control.Monad.Logic
import Control.Lens
import Data.Char
import Data.List
import Data.List.Extra (groupOn)
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

parseExample :: [String] -> String -> IO (Either RSchema ErrorMessage)
parseExample mdls mkFun = catch (do
    typ <- askGhc mdls $ exprType TM_Default mkFun
    let hsType = typeToLHsType typ
    return (Left $ toInt $ resolveType hsType))
    (\(e :: SomeException) -> return (Right $ show e))
    where
        toInt (ForallT x t) = ForallT x (toInt t)
        toInt (Monotype t) = Monotype (integerToInt t)

getExampleTypes :: Environment -> [RSchema] -> IO [String]
getExampleTypes env validSchemas = do
    let validTypes = map (shape . toMonotype) validSchemas
    let mdls = env ^. included_modules
    let initTyclassState = emptyTyclassState { _supportModules = Set.toList mdls }
    evalStateT (do
        (mbFreeVar, st) <- if not (null validTypes) 
                then foldM stepAntiUnification (head validTypes, emptyAntiUnifState) (tail validTypes)
                else error "get example types error"
        let t = defaultTypeVar mbFreeVar
        let tvars = typeVarsOf t
        let tcass = st ^. tyclassAssignment
        generalizeType tcass t
        ) initTyclassState
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
resolveType (L _ (HsFunTy f _)) = Monotype (resolveType' f)
resolveType (L _ (HsQualTy ctx body)) = Monotype bodyWithTcArgs
    where
        unlocatedCtx = let L _ c = ctx in c
        tyConstraints = map (prefixTyclass . resolveType') unlocatedCtx

        prefixTyclass tc@(ScalarT (DatatypeT name args rs) r) = 
            ScalarT (DatatypeT (tyclassPrefix ++ name) args rs) r
        prefixTyclass tc = error $ "Unsupported type class " ++ show tc

        bodyWithTcArgs = foldr (FunctionT "") (resolveType' body) tyConstraints
resolveType t = error (showSDocUnsafe $ ppr t)

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
    v <- freshId "a"
    modify $ over typeAssignment1 (Map.insertWith (++) t1 [v])
    modify $ over typeAssignment2 (Map.insertWith (++) t2 [v])
    when (isJust mbCons) (
        modify $ over tyclassAssignment (Map.insertWith Set.union v (fromJust mbCons))
        )
    return $ vart_ v

antiUnification' :: SType -> SType -> AntiUnifier IO SType
antiUnification' AnyT t = return t
antiUnification' t AnyT = return t
antiUnification' t1@(ScalarT (TypeVarT _ id1) _) t2@(ScalarT (TypeVarT _ id2) _)
  | univTypeVarPrefix == head id1 = return $ vart_ id2
  | univTypeVarPrefix == head id2 = return $ vart_ id1
  | otherwise = checkTyclass t1 t2
antiUnification' t1@(ScalarT (TypeVarT _ id) _) t
  | univTypeVarPrefix == head id = return t
  | otherwise = checkTyclass t1 t
antiUnification' t tv@(ScalarT (TypeVarT {}) _) = do
    swapAssignments
    result <- antiUnification' tv t
    swapAssignments
    return result
antiUnification' t1@(ScalarT (DatatypeT dt1 args1 _) _) t2@(ScalarT (DatatypeT dt2 args2 _) _)
  | dt1 == dt2 = do
      args' <- mapM (uncurry antiUnification') (zip args1 args2)
      return $ ScalarT (DatatypeT dt1 args' []) ()
  | dt1 /= dt2 = do
      tass1 <- gets $ view typeAssignment1
      tass2 <- gets $ view typeAssignment2
      let overlap = (tass1 Map.! t1) `intersect` (tass2 Map.! t2)
      if t1 `Map.member` tass1 && t2 `Map.member` tass2 && not (null overlap)
         then if length overlap > 1 then error "antiUnficiation fails"
                                    else return $ vart_ (head overlap)
         else checkTyclass t1 t2 
antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
    tArg <- antiUnification' tArg1 tArg2
    tRes <- antiUnification' tRes1 tRes2
    return $ FunctionT x1 tArg tRes
antiUnification' t1 t2 = error $ "unhandled " ++ show (t1, t2)

generalizeType :: TyclassAssignment -> SType -> StateT TypeClassState IO [String]
generalizeType tcass t = do
    -- do the filtering inside observeAll
    liftIO $ print t
    liftIO $ print tcass
    typs <- observeAllT (do
        (ass, gt) <- evalStateT (generalizeSType tcass t) Map.empty
        guard (notOnlyPositive gt)
        return (ass, gt)
        )
    return $ map addTyclasses typs
    where
        toConstraint (id, s) = intercalate ", " $ map (unwords . (:[id])) (Set.toList s)
        addTyclasses (constraints, t) = 
            let tyclasses = intercalate ", " $
                            filter (not . null) $
                            map toConstraint $
                            Map.toList constraints
             in if null tyclasses then show t
                                  else printf "(%s) => %s" tyclasses (show t)

generalizeSType :: TyclassAssignment -> SType -> TypeGeneralizer IO (TyclassAssignment, SType)
generalizeSType tcass t@(ScalarT (TypeVarT _ id) _) = do
    -- either add type class or not
    let tc = maybe [] Set.toList (Map.lookup id tcass)
    let mkResult tcs = guard (checkImplies tcs) >> return (Map.singleton id (Set.fromList tcs), t)
    msum $ map mkResult (subsequences tc)
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
    ) `mplus` if null args then datatypeToVar tcass t else mzero
generalizeSType _ t = error $ "unsupported type " ++ show t

datatypeToVar :: TyclassAssignment -> SType -> TypeGeneralizer IO (TyclassAssignment, SType)
datatypeToVar tcass t = do
    -- generalize the data type into a fresh type variable
    tccache <- lift $ lift $ gets $ view tyclassCache
    typeCounting <- get
    dtclasses <- case Map.lookup t tccache of
                    Just tc -> return tc
                    Nothing -> lift . lift $ Set.toList <$> getDtTyclasses tcass t
    let (v, startIdx) = case Map.lookup t typeCounting of
                            Just (v, i) -> (v, i)
                            Nothing -> let currMaxOrd = maximum (map (ord . head . fst) (Map.elems typeCounting))
                                        in if Map.null typeCounting then ("a", 0)
                                                                    else ([chr (currMaxOrd + 1)], 0)
    -- choose between incr the index or not
    -- if reusing some previous var, do not backtrack over type classes
    msum (map (\idx -> return (Map.empty, vart_ (v ++ show idx))) [1..startIdx]) `mplus`
        -- if start a new index
        (msum $ map (\tcs -> do
            -- "Ord" implies "Eq"
            guard (checkImplies tcs)
            -- this should be reverted if we backtrack
            modify $ Map.insert t (v, startIdx + 1)
            let varName = v ++ show (startIdx + 1)
            return (Map.singleton varName (Set.fromList tcs), vart_ varName)
            ) (subsequences dtclasses))

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
    let varTcs = concatMap (\v -> case Map.lookup v tcass of
                                    Just tc -> map (v,) $ Set.toList tc
                                    Nothing -> []) vars
    let varTcStrs = map (\(v, tc) -> unwords [tc, v]) varTcs
    let allTcs = intercalate ", " $ varTcStrs ++ [unwords [tyclass, (show typ)]]
    let query = printf "undefined :: (%s) => ()" allTcs
    liftIO $ print query
    liftIO $
        catch (askGhc mdls (exprType TM_Default query) >> return (Just tyclass))
              (\(e :: SomeException) -> liftIO (print e) >> return Nothing)

defaultTypeVar :: SType -> SType
defaultTypeVar (ScalarT (TypeVarT _ v) _) | univTypeVarPrefix == head v = ScalarT (DatatypeT "Int" [] []) ()
defaultTypeVar (ScalarT (DatatypeT name args _) _) = 
    let args' = map defaultTypeVar args
     in ScalarT (DatatypeT name args' []) ()
defaultTypeVar (FunctionT x tArg tRes) =
    let tArg' = defaultTypeVar tArg
        tRes' = defaultTypeVar tRes
     in FunctionT x tArg' tRes'
defaultTypeVar t = t

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

checkImplies :: [Id] -> Bool
checkImplies tcs = if "Ord" `elem` tcs then "Eq" `notElem` tcs else True
