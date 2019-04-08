{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module PetriNet.PNSolver (runPNSolver) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics
import Control.Lens
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Data (Data)
import qualified Data.Char as Char
import Data.Either hiding (fromLeft, fromRight)
import Data.List.Extra
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Data.Serialize (Serialize)
import Data.Aeson (ToJSON, genericToEncoding, defaultOptions)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import Debug.Trace
import Language.Haskell.Exts.Parser (parseExp, ParseResult(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Z3.Monad as Z3
import System.CPUTime
import Text.Printf

import Types.Common
import Types.Type
import Types.Environment
import Types.Abstract
import Types.Solver
import Types.Program
import Types.Experiments
import Types.Encoder
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Program
import Synquid.Type
import Synquid.Logic
import Synquid.Util
import Synquid.Error
import Synquid.Pretty
import PetriNet.AbstractType
import PetriNet.PNEncoder
import PetriNet.GHCChecker
import HooglePlus.Stats
import HooglePlus.CodeFormer
import HooglePlus.Abstraction
import HooglePlus.Refinement
import Database.Convert
import Database.Generate

encodeFunction :: Id -> AbstractSkeleton -> FunctionCode
encodeFunction id t@(AFunctionT tArg tRet) = FunctionCode id hoParams params [show $ lastAbstractType t]
  where
    base = (0, [])
    hoFun x = encodeFunction (show x) x
    hoParams = map hoFun $ filter isAFunctionT (abstractParamList t)
    params = map show (abstractParamList t)
encodeFunction id t = FunctionCode id [] [] [show t]

freshId :: (MonadIO m) => Id -> PNSolver m Id
freshId prefix = do
    indices <- flip (^.) nameCounter <$> get
    let idx = Map.findWithDefault 0 prefix indices
    modify (over nameCounter $ Map.insert prefix (idx+1))
    return $ prefix ++ show idx

-- | Replace all bound type variables with fresh free variables
freshType :: (MonadIO m) => RSchema -> PNSolver m RType
freshType sch = freshType' Map.empty [] sch
  where
    freshType' subst constraints (ForallT a sch) = do
        a' <- freshId "A"
        freshType' (Map.insert a (vart a' ftrue) subst) (a':constraints) sch
    freshType' subst constraints (Monotype t) = return (typeSubstitute subst t)

instantiate :: (MonadIO m) => Environment -> [(Id, AbstractSkeleton)] -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = do
    semantic <- view abstractionTree <$> get
    Map.fromList <$> (nonPolyAbstracts semantic >>= instantiate' sigs)
  where
    nonPolySigs = filter (not . hasAbstractVar (env ^. boundTypeVars). snd) sigs
    nonPolyAbstracts semantic = mapM (\(id, t) -> do
        name <- if Map.member id (env ^. arguments) then freshId (id++"_") else freshId "f"
        modify $ over nameMapping (Map.insert name id)
        return (name, head (cutoff env semantic t))) nonPolySigs
    removeSuffix id ty = (removeLast '_' id, ty)

    instantiate' sigs sigsAcc = do
        st <- get
        -- to test first level abstraction, please disable the complement type here
        let typs = case st ^. refineStrategy of
                     AbstractRefinement -> leafTypes (st ^. abstractionTree)
                     NoRefine -> filter notEx (leafTypes (st ^. abstractionTree))
                     Combination -> filter notEx (leafTypes (st ^. abstractionTree))
                     QueryRefinement -> filter notEx (leafTypes (st ^. abstractionTree))
        writeLog 3 $ text "Current abstract types:" <+> pretty typs
        sigs' <- foldM (\acc -> (<$>) (acc ++) . uncurry (instantiateWith env typs)) [] sigs
        return $ nubOrdOn (uncurry removeSuffix) (sigsAcc ++ sigs')

instantiateWith :: (MonadIO m) => Environment -> [AbstractSkeleton] -> Id -> AbstractSkeleton -> PNSolver m [(Id, AbstractSkeleton)]
instantiateWith env typs id sk = do
    let vars = Set.toList (allAbstractVar sk) \\ (env ^. boundTypeVars)
    let multiSubsts = map (zip vars) $ multiPermutation (length vars) typs
    let substedSymbols = map (foldr (\(id, t) acc -> abstractSubstitute (env ^. boundTypeVars) id t acc) sk) multiSubsts
    st <- get
    let refinedSymbols = nubOrd (concatMap (cutoff env (st ^. abstractionTree)) substedSymbols)
    foldrM (\t accMap -> do
        newId <- if "Pair" `isPrefixOf` id then freshId (id++"_") else freshId "f"
        modify $ over nameMapping (Map.insert newId id)
        return $ (newId, t):accMap) [] refinedSymbols

splitTransition :: MonadIO m => Environment -> Id -> SplitInfo -> PNSolver m SplitInfo
splitTransition env tid info | "|clone" `isSuffixOf` tid = do
    let newTyps = snd (head (splitedPlaces info))
    let mkFc t = FunctionCode (show t ++ "|clone") [] [show t] [show t, show t]
    let unifiedTyps = map (\t -> (show t ++ "|clone", mkFc t)) newTyps
    -- step 1: add new transitions into the environment
    mapM_ (\(id, ty) -> modify $ over functionMap (HashMap.insert id ty)) unifiedTyps
    -- step 2: pack the information into SplitInfo for incremental encoding
    let newIds = fst (unzip unifiedTyps)
    -- step 3: remove splited transition from the type2transition mapping
    modify $ over type2transition (Map.map (delete tid))
    modify $ over functionMap (HashMap.delete tid)
    -- step 4: return the new split information with splited transitions
    return (info { splitedGroup = (tid, newIds) : splitedGroup info })
splitTransition env tid info | "Pair_match" `isPrefixOf` tid || 
                               "|color" `isSuffixOf` tid || 
                               "|uncolor" `isSuffixOf` tid = do
    -- step 1: remove splited transition from the type2transition mapping
    modify $ over type2transition (Map.map (delete tid))
    modify $ over functionMap (HashMap.delete tid)
    return info
splitTransition env tid info = do
    writeLog 3 $ pretty tid
    sigs <- view currentSigs <$> get
    let splitedTyp = fst (head (splitedPlaces info))
    let newTyps = snd (head (splitedPlaces info))
    let typ = transitionSig sigs
    -- step 1: unify the type with refined abstract type and get new signatures
    unifyRes <- unifyNewType typ newTyps
    let unifiedTyps = nubOrdOn snd unifyRes
    -- step 2: add new transitions into the environment
    mapM_ (\(id, ty) -> modify $ over currentSigs (Map.insert id ty)) unifiedTyps
    mapM_ (\(id, ty) -> modify $ over functionMap (HashMap.insert id (encodeFunction id ty))) unifiedTyps
    mapM_ (uncurry typeMap) unifiedTyps
    -- step 3: pack the information into SplitInfo for incremental encoding
    let newIds = fst (unzip unifiedTyps)
    -- step 4: remove splited transition from the type2transition mapping
    modify $ over type2transition (Map.map (delete tid))
    modify $ over functionMap (HashMap.delete tid)
    -- return the new split information with splited transitions
    -- step 5: update the detailed signature ids
    modify $ over detailedSigs (Set.union (Set.fromList newIds) . Set.delete tid)
    if null newIds
       then return info
       else do
           info <- addPairMatch unifiedTyps
           idPairs <- mapM (addColorTrans typ) unifiedTyps
           let pgs = groupOn fst (sortOn fst (concat idPairs))
           let newPairs = map (\xs -> (show (fst (head xs)), map show (snd (unzip xs)))) pgs
           return (info { splitedGroup = (tid, newIds) : newPairs ++ splitedGroup info })
  where
    addPairMatch unifiedTyps | "Pair" `isPrefixOf` tid = do
        -- step 6: add pair pattern matching when we are splitting some Pair constructors, they share the same number with constructor
        let tid' = Text.unpack (Text.replace "Pair" "Pair_match" (Text.pack tid))
        let pairs = map (\(id, ty) -> mkPairMatch (encodeFunction id ty)) unifiedTyps
        mapM_ (\(id, ty) -> do
                                let AFunctionT arg0 (AFunctionT arg1 ret) = ty
                                modify $ over currentSigs (Map.insert (replaceId "Pair" "fst" id) (AFunctionT ret arg0))
                                modify $ over currentSigs (Map.insert (replaceId "Pair" "snd" id) (AFunctionT ret arg1))) unifiedTyps
        mapM_ (\ef -> modify $ over functionMap (HashMap.insert (funName ef) ef)) pairs
        mapM_ (\(id, ty) -> typeMap (replaceId "Pair" "Pair_match" id) ty) unifiedTyps
        let newIds' = map funName pairs
        return (info { splitedGroup = (tid', newIds') : splitedGroup info })
    addPairMatch unfiedTyps | otherwise = return info

    addColorTrans typ (_, unifiedTyp) | isAHigherOrder typ = do
        let hops = filter isAFunctionT (abstractParamList unifiedTyp)
        let oldHops = filter isAFunctionT (abstractParamList typ)
        mapM_ addHoParam hops
        return (zip oldHops hops)
    addColorTrans _ _ = return []

    getHoParams (AFunctionT tArg tRet) = init (decompose tArg) ++ getHoParams tRet
    getHoParams _ = []

    typeMap id ty = do
        let tys = decompose ty
        mapM_ (\t -> modify $ over type2transition (Map.insertWith union t [id])) tys

    transitionSig sigs = fromMaybe
                         (error $ printf "cannot find transition %s in sig map" tid)
                         (Map.lookup tid sigs)

    matchTyps target (AFunctionT tArg tRet) (AFunctionT tArg' tRet') =
        matchTyps target tArg tArg' ++ matchTyps target tRet tRet'
    matchTyps target absTyp polyTyp
        | target == absTyp = [polyTyp]
        | otherwise = []

    genTypes pat newTyps (AFunctionT tArg tRet) = 
            [ AFunctionT arg ret | arg <- if null args then [tArg] else args
                                 , ret <- if null rets then [tRet] else rets ]
        where
            args = genTypes pat newTyps tArg
            rets = genTypes pat newTyps tRet
    genTypes pat newTyps t = if t == pat then newTyps else []

    unifyNewType typ newTyps = do
        let id' = removeLast '_' tid
        polyTyp <- toAbstractType . shape <$> findSymbol env id'
        -- pass the current abstract hierarchy into the unifier
        abstraction <- view abstractionTree <$> get
        -- get all the constraints
        let typs = genTypes (fst (head (splitedPlaces info))) newTyps typ
        let constraints = map (`typeConstraints` polyTyp) typs
        writeLog 3 $ text "trying to solve constraints" <+> pretty constraints
        let unifiers = map (getUnifier abstraction (env ^. boundTypeVars) (Just Map.empty)) constraints
        writeLog 3 $ text "unify result is" <+> pretty (show unifiers)
        let checkSuccess = filter isJust unifiers
        if not (null checkSuccess)
           then do
               let ts = snd (unzip (filter (isJust . fst) (zip unifiers typs)))
               writeLog 3 $ text "get signatures" <+> pretty ts
               ids <- mapM (\_ -> if Map.member id' (env ^. arguments) || "Pair" `isPrefixOf` id' then freshId (id'++"_") else freshId "f") ts
               mapping <- view nameMapping <$> get
               let actualName = fromMaybe 
                                (error $ "cannot find name " ++ tid ++ " in the function name mapping")
                                (Map.lookup tid mapping)
               mapM_ (\name -> modify $ over nameMapping (Map.insert name actualName)) ids
               return (zip ids ts)
           else return []

distinguish :: MonadIO m => Environment -> SType -> SType -> PNSolver m (Maybe SplitMsg)
distinguish env (FunctionT _ tArg tRes) (FunctionT _ tArg' tRes') = do
    diff <- distinguish env tArg tArg'
    case diff of
      Nothing  -> distinguish env tRes tRes'
      res -> return res
distinguish env AnyT _ = return Nothing
distinguish env _ AnyT = return Nothing
distinguish env t1 t2 = do
    tass <- view typeAssignment <$> get
    semantic <- view abstractionTree <$> get
    writeLog 2 $ text "type assignments" <+> text (show tass)
    let t1' = var2any env (stypeSubstitute tass t1)
    let t2' = var2any env (stypeSubstitute tass t2)
    let ats1 = cutoff env semantic (toAbstractType t1')
    let ats2 = cutoff env semantic (toAbstractType t2')
    writeLog 3 $ text "trying to distinguish" <+> pretty t1' <+> text "==>" <+> pretty ats1 <+> text "and" <+> pretty t2' <+> text "==>" <+> pretty ats2
    -- only try to get split information when the two types have
    -- same abstract representations in the current abstraction level
    let diff = ats1 `intersect` ats2
    let pTyp = head diff
    let aTyp1 = toAbstractType t1'
    let aTyp2 = toAbstractType t2'
    if null diff || t1' == t2'
       then return Nothing
       else case distinguish' (env ^. boundTypeVars) pTyp aTyp1 aTyp2 of
              Nothing -> return Nothing
              Just t -> return (Just (pTyp, t))


findSymbol :: MonadIO m => Environment -> Id -> PNSolver m RType
findSymbol env sym = do
    nameMap <- view nameMapping <$> get
    let name = fromMaybe sym (Map.lookup sym nameMap)
    case lookupSymbol name 0 env of
        Nothing -> do
            case lookupSymbol ("(" ++ name ++ ")") 0 env of
                Nothing -> do
                    modify $ set isChecked False
                    writeLog 2 $ text "cannot find symbol" <+> text name <+> text "in the current environment"
                    return AnyT
                Just sch -> freshType sch
        Just sch -> freshType sch

strengthenRoot :: MonadIO m => Environment -> AbstractSkeleton -> SType -> SType -> PNSolver m AbstractSkeleton
strengthenRoot env dfault expected real = do
    diff <- distinguish env expected real
    writeLog 3 $ text "strengthen root with expected" <+> pretty expected <+> text "and real" <+> pretty real <+> text "and get" <+> pretty diff
    tass <- view typeAssignment <$> get
    semantic <- view abstractionTree <$> get
    let expected' = var2any env (stypeSubstitute tass expected)
    let real' = var2any env (stypeSubstitute tass real)
    case diff of
      Nothing -> do -- they already have different abstract types, we need to distinguish the concrete type from its abstract one
          return (head (cutoff env semantic (toAbstractType expected')))
      Just (t1, t2) -> do -- they have the same abstract type, we need to assign new abstraction for them
          let semantic' = updateSemantic env semantic t2
          return (head (cutoff env semantic' (toAbstractType expected')))

strengthenArgs :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m (Maybe AbstractSkeleton)
strengthenArgs env (Program (PSymbol sym) typ) at = do
    semantic <- view abstractionTree <$> get
    sigs <- view currentSigs <$> get
    let at' = case Map.lookup sym sigs of
                     Nothing -> head (cutoff env semantic (toAbstractType (shape typ)))
                     Just t -> lastAbstract t
    let at'' = fromMaybe at (abstractIntersection at' at)
    when (isJust (abstractIntersection at' at))
         (do
             writeLog 3 $ text "add pair of split types" <+> text (show (at', at''))
             modify $ over splitTypes ((:) (at', at'')))
    -- unify the return type with the abstract type and get the type assignment
    t <- findSymbol env (removeLast '_' sym)
    let maybeTass = checkUnification semantic (env ^. boundTypeVars) Map.empty at (toAbstractType (shape (lastType t)))
    case maybeTass of
      Nothing -> return Nothing -- we find a type conflict
      Just tass -> do
                let vars = typeVarsOf t
                let tass' = foldr (\v m -> if Map.member v m then m else Map.insert v (AExclusion Set.empty) m) tass vars
                let t' = foldr (uncurry (abstractSubstitute (env ^. boundTypeVars))) (toAbstractType (shape t)) (Map.toList tass')
                return (Just t')
strengthenArgs env (Program (PApp pFun pArg) typ) at = do
    maybeFun <- strengthenArgs env pFun at
    case maybeFun of
      Nothing -> return Nothing
      Just (AFunctionT tArg tRet) -> do
          maybeArg <- strengthenArgs env pArg tArg
          case maybeArg of
            Nothing -> return Nothing
            Just t -> return (Just tRet)
      t -> error $ "unexpected type pattern " ++ show t
strengthenArgs env (Program (PFun x body) (FunctionT _ tArg tRet)) (AFunctionT atArg atRet) = do
    maybeRet <- strengthenArgs (addVariable x (addTrue tArg) env) body atRet
    case maybeRet of
      Nothing -> return Nothing
      Just atRet' -> return (Just (AFunctionT atArg atRet'))
strengthenArgs env (Program (PFun x body) t) (AFunctionT atArg atRet) = do
    id <- freshId "A"
    let tArg = addTrue (ScalarT (TypeVarT Map.empty id) ())
    maybeRet <- strengthenArgs (addVariable x (addTrue tArg) env) body atRet
    case maybeRet of
      Nothing -> return Nothing
      Just atRet' -> return (Just (AFunctionT atArg atRet'))
strengthenArgs _ prog t = return Nothing

-- bottom up check a program on the concrete type system
-- at the same time, keep track of the abstract type for each node
bottomUpCheck :: (MonadIO m) => Environment -> RProgram -> PNSolver m (RProgram, AbstractSkeleton)
bottomUpCheck env p@(Program (PSymbol sym) typ) = do
    -- lookup the symbol type in current scope
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    t <- findSymbol env (removeLast '_' sym)
    sigs <- view currentSigs <$> get
    params <- view paramNames <$> get
    srcs <- view sourceTypes <$> get
    -- add arguments into this map
    let sigs' = foldr (uncurry Map.insert) sigs (zip params srcs)
    let n = fromMaybe (error $ "cannot find symbol " ++ sym ++ " in signatures")
                      (Map.lookup sym sigs')
    return (Program (PSymbol sym) t, n)
bottomUpCheck env (Program (PApp pFun pArg) typ) = do
    (arg, ta) <- bottomUpCheck env pArg
    ifM (view isChecked <$> get)
        (do
            (fun, tf) <- bottomUpCheck env pFun
            ifM (view isChecked <$> get)
                (do
                    let FunctionT _ tArg tRet = typeOf fun
                    let AFunctionT atArg atRet = tf
                    writeLog 3 $ text "Solving constraint for" <+> pretty arg <+> text "::" <+> pretty (shape $ typeOf arg) <+> text "==" <+> pretty (shape tArg)
                    solveTypeConstraint env (shape $ typeOf arg) (shape tArg)
                    ifM (view isChecked <$> get)
                        (return (Program (PApp fun arg) tRet, atRet))
                        (do
                            atArg' <- strengthenRoot env atArg (shape tArg) (shape (typeOf arg))
                            return (arg, atArg')))
                (return (fun, tf)))
        (return (arg, ta))
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    (body', at) <- bottomUpCheck (addVariable x (addTrue tArg) env) body
    ifM (view isChecked <$> get)
        (return (Program (PFun x body') (FunctionT x tArg (typeOf body')), AFunctionT (toAbstractType (shape tArg)) at))
        (return (body', at))
bottomUpCheck env p@(Program (PFun x body) _) = do
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    id <- freshId "A"
    let tArg = addTrue (ScalarT (TypeVarT Map.empty id) ())
    (body', at) <- bottomUpCheck (addVariable x tArg env) body
    ifM (view isChecked <$> get)
        (return (Program (PFun x body') (FunctionT x tArg (typeOf body')), AFunctionT (toAbstractType (shape tArg)) at))
        (return (body', at))
bottomUpCheck _ p = error ("unhandled case for checking " ++ show p ++ "::" ++ show (typeOf p))

solveTypeConstraint :: (MonadIO m) => Environment -> SType -> SType -> PNSolver m ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | id == id' = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | isBound env id && isBound env id' =
    modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | isBound env id = do
    st <- get
    if id' `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv
            solveTypeConstraint env tv typ
        else unify env id' tv
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv'
            solveTypeConstraint env typ tv'
        else if id' `Map.member` (st ^. typeAssignment)
            then do
                let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
                writeLog 3 $ text "Solving constraint" <+> pretty tv <+> "==" <+> pretty typ
                solveTypeConstraint env tv typ
            else do
                unify env id tv'
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty t
            solveTypeConstraint env typ t
        else do
            unify env id t
solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 $ text "Solving constraint" <+> pretty tArg <+> "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    st <- get
    when (st ^. isChecked) (do
        writeLog 3 $ text "Solving constraint" <+> pretty tRet <+> "==" <+> pretty tRet'
        solveTypeConstraint env tRet tRet')
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id /= id' = do
    modify $ set isChecked False
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id == id' = do
    solveTypeConstraint' env tArgs tArgs'
  where
    solveTypeConstraint' _ []  [] = return ()
    solveTypeConstraint' env (ty:tys) (ty':tys') = do
        writeLog 3 $ text "Solving constraint" <+> pretty ty <+> "==" <+> pretty ty'
        solveTypeConstraint env ty ty'
        checked <- view isChecked <$> get
        -- if the checking between ty and ty' succeeds, proceed to others
        when (checked) (solveTypeConstraint' env tys tys')
solveTypeConstraint env t1 t2 = error $ "unknown types " ++ show t1 ++ " or " ++ show t2

-- | unify the type variable with some given type
-- add the type assignment to our state
unify :: (MonadIO m) => Environment -> Id -> SType -> PNSolver m ()
unify env v t = do
    modify $ over typeAssignment (Map.map (stypeSubstitute (Map.singleton v t)))
    tass <- view typeAssignment <$> get
    modify $ over typeAssignment (Map.insert v (stypeSubstitute tass t))

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m SplitInfo
refineSemantic env prog at = do
    -- back propagation of the error types to get all split information
    strengthenArgs env prog at
    -- get the split pairs
    splits <- view splitTypes <$> get
    -- the only problem is to deal with split one type more than once, let's assume it would not happen first
    splitInfos <- filterM (uncurry hasNewSplit) (nubOrdOn fst splits) >>= mapM (uncurry transSplit)
    let SplitInfo pls trs = combineInfo splitInfos
    return (SplitInfo pls (flattenInfo trs))
  where
    -- if any of the transitions is splitted again, merge the results
    combineInfo [] = SplitInfo [] []
    combineInfo (x:xs) = let SplitInfo ts trs = combineInfo xs
                             SplitInfo [(t, ts')] trs' = x
                          in SplitInfo ((t, ts'):ts) (trs ++ trs')

    replaceTrans tr trs [] = (False, [])
    replaceTrans tr trs ((x,xs):remains) = if tr `elem` xs then (True, (x, trs ++ delete tr xs):remains)
                                                           else let (res, remains') = replaceTrans tr trs remains
                                                                 in (res, (x, xs):remains')

    flattenInfo [] = []
    flattenInfo ((tr, trs):infos) = let (res, infos') = replaceTrans tr trs infos
                                     in if res then flattenInfo infos'
                                               else (tr, trs) : flattenInfo infos

    hasNewSplit t1 t2 = do
        semantic <- view abstractionTree <$> get
        writeLog 2 $ text "check add type" <+> pretty t2 <+> text "into" <+> text (show semantic)
        let semantic' = updateSemantic env semantic t2
        return (semantic /= semantic')

    transSplit t1 t2 = do
        semantic <- view abstractionTree <$> get
        writeLog 2 $ text "add type" <+> pretty t2 <+> text "into" <+> text (show semantic)
        let semantic' = updateSemantic env semantic t2
        modify $ set abstractionTree semantic'
        let t2' = head (cutoff env semantic' t2)
        writeLog 2 $ text $ printf "%s is splited into %s" (show t1) (show t2')
        writeLog 2 $ text $ printf "new semantic is %s" (show semantic')
        t2tr <- view type2transition <$> get
        let tids = Map.findWithDefault [] t1 t2tr
        let nts = leafTypes semantic' \\ leafTypes semantic
        if null tids
           then do
               -- find one of the leaf node to split at
               let t1' = head (filter (isSubtypeOf t2) (leafTypes semantic))
               let splitNode = SplitInfo [(t1', nts)] []
               let tids' = Map.findWithDefault [] t1' t2tr
               SplitInfo _ trs <- foldrM (splitTransition env) splitNode tids'
               return (SplitInfo [(t1, nts)] trs)
           else do
               let splitNode = SplitInfo [(t1, nts)] []
               foldrM (splitTransition env) splitNode tids

initNet :: MonadIO m => Environment -> PNSolver m ()
initNet env = withTime "construction time" $ do
    -- reset the solver state
    modify $ set functionMap HashMap.empty
    modify $ set currentSigs Map.empty
    modify $ set type2transition Map.empty

    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    absSymbols <- mapM (uncurry abstractSymbol) (Map.toList (allSymbols env))
    -- first order arguments are tokens but not transitions in petri net
    let usefulSymbols = filter (flip notElem ("fst" : "snd" : Map.keys foArgs) . fst) absSymbols
    sigs <- instantiate env usefulSymbols
    modify $ set detailedSigs (Map.keysSet sigs)
    writeLog 3 $ text "instantiated sigs" <+> pretty (Map.toList sigs)
    mapM_ addEncodedFunction (Map.toList sigs)
    -- add clone functions for each type
    allTy <- gets (Map.keys . view type2transition)
    mapM_ addCloneFunction allTy   
  where
    abstractSymbol id sch = do
        t <- freshType sch
        let absTy = toAbstractType (shape t)
        return (id, absTy)

    addEncodedFunction (id, f) | "Pair" `isPrefixOf` id = do
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert (replaceId "Pair" "Pair_match" id) (mkPairMatch ef))
        modify $ over functionMap (HashMap.insert id ef)
        modify $ over currentSigs (Map.insert id f)
        let AFunctionT arg0 (AFunctionT arg1 ret) = f
        -- add fst and snd sigs
        modify $ over currentSigs (Map.insert (replaceId "Pair" "fst" id) (AFunctionT ret arg0))
        modify $ over currentSigs (Map.insert (replaceId "Pair" "snd" id) (AFunctionT ret arg1))
        -- store the used abstract types and their groups into mapping
        let addTransition k tid = Map.insertWith union k [tid]
        let includedTyps = decompose f
        mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps
        mapM_ (\t -> modify $ over type2transition (addTransition t (replaceId "Pair" "Pair_match" id))) includedTyps
    addEncodedFunction (id, f) | isAHigherOrder f = do
        -- for higher order functions, we add coloring and uncoloring transitions
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert id ef)
        modify $ over currentSigs (Map.insert id f)
        -- add transitions to color and uncolor tokens
        let hops = filter isAFunctionT (abstractParamList f)
        mapM_ addHoParam hops
    addEncodedFunction (id, f) = do
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert id ef)
        modify $ over currentSigs (Map.insert id f)
        -- store the used abstract types and their groups into mapping
        let addTransition k tid = Map.insertWith union k [tid]
        let includedTyps = decompose f
        mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps

resetEncoder :: (MonadIO m) => Environment -> RType -> PNSolver m EncodeState
resetEncoder env dst = do
    -- reset source and destination types
    let binds = env ^. boundTypeVars
    abstraction <- gets (view abstractionTree)
    let tgt = (head (cutoff env abstraction (toAbstractType (shape dst))))
    modify $ set targetType tgt
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    let srcTypes = map ( head
                       . cutoff env abstraction
                       . toAbstractType
                       . shape
                       . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    modify $ set paramNames $ Map.keys foArgs
    srcTypes <- gets (view sourceTypes)
    writeLog 2 $ text "parameter types are" <+> pretty srcTypes
    writeLog 2 $ text "return type is" <+> pretty tgt

    loc <- gets (view currentLoc)
    funcs <- gets (view functionMap)
    t2tr <- gets (view type2transition)
    let tid2tr = Map.foldrWithKey (\k v -> Map.insert (show k) v) Map.empty t2tr
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    liftIO $ encoderInit loc hoArgs (map show srcTypes) (show tgt) (HashMap.elems funcs) tid2tr

findPath :: (MonadIO m) => Environment -> RType -> EncodeState -> PNSolver m (CodePieces, EncodeState)
findPath env dst st = do
    (res, st') <- withTime "solver time" (liftIO (encoderSolve st))
    case res of
        [] -> do
            currSt <- get
            maxDepth <- view maxApplicationDepth <$> get
            when (currSt ^. currentLoc >= maxDepth) (error "cannot find a path")
            modify $ set currentLoc ((currSt ^. currentLoc) + 1)
            st'' <- withTime "encoding time" (resetEncoder env dst)
            findPath env dst st''
        _  -> withTime "code former time" $ do
            fm <- view functionMap <$> get
            src <- view sourceTypes <$> get
            args <- view paramNames <$> get
            let sortedRes = sortOn snd res
            let transNames = map fst sortedRes
            writeLog 2 $ text "found path" <+> pretty transNames
            let usefulTrans = filter (\n -> skipUncolor n
                                         && skipClone n
                                         && skipDiscard n) transNames
            let sigNames = map removeSuffix usefulTrans
            dsigs <- view detailedSigs <$> get
            let sigNames' = filter (\name -> Set.member name dsigs || "Pair_match" `isPrefixOf` name) sigNames
            let sigs = substPair (map (findFunction fm) sigNames')
            writeLog 2 $ text "found filtered sigs" <+> text (show sigs)
            let initialFormer = FormerState 0 HashMap.empty [] []
            code <- generateCode initialFormer (map show src) args sigs
            return (code, st')
  where
    findFunctions fm groups name =
        case Map.lookup name groups of
            Just g -> findFunction fm (head g)
            Nothing -> error $ "cannot find function group " ++ name

    findFunction fm name = fromMaybe (error $ "cannot find function name " ++ name) 
                                     (HashMap.lookup name fm)

    combinations []    = []
    combinations [x]   = [x]
    combinations (h:t) = [ hh:tt | hh <- h, tt <- combinations t ]

    generateCode initialFormer src args sigs = do
        tgt <- view targetType <$> get
        liftIO (evalStateT (generateProgram sigs src args (show tgt) True) initialFormer)

    skipUncolor = not . isInfixOf "|uncolor"
    skipClone = not . isInfixOf "|clone"
    skipDiscard = not . isInfixOf "|discard"
    removeSuffix = removeLast '|'

    substPair [] = []
    substPair (x:xs) = if "Pair_match" `isPrefixOf` funName x
                          then   ( x { funName = replaceId "Pair_match" "fst" (funName x), funReturn = [head (funReturn x)] } )
                               : ( x { funName = replaceId "Pair_match" "snd" (funName x), funReturn = [funReturn x !! 1] } )
                               : substPair xs
                          else x : substPair xs

fixEncoder :: MonadIO m => Environment -> RType -> EncodeState -> SplitInfo -> PNSolver m EncodeState
fixEncoder env dst st info = do
    let binds = env ^. boundTypeVars
    abstraction <- gets (view abstractionTree)
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    let srcTypes = map ( head
                       . cutoff env abstraction
                       . toAbstractType
                       . shape
                       . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    let tgt = head (cutoff env abstraction (toAbstractType (shape dst)))
    modify $ set targetType tgt
    writeLog 2 $ text "fixed parameter types are" <+> pretty srcTypes
    writeLog 2 $ text "fixed return type is" <+> pretty tgt
    loc <- gets (view currentLoc)
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    writeLog 2 $ text ("get split information " ++ show info)
    let newTyps = concat (snd (unzip (splitedPlaces info)))
    mapM_ addCloneFunction newTyps
    modify $ over type2transition (Map.filter (not . null))
    funcs <- gets (view functionMap)
    t2tr <- gets (view type2transition)
    let tid2tr = Map.foldrWithKey (\k v -> Map.insert (show k) v) Map.empty t2tr
    liftIO $ execStateT (encoderRefine info (HashMap.elems funcs) tid2tr (map show srcTypes) (show tgt)) st

findProgram :: MonadIO m => Environment -> RType -> EncodeState -> PNSolver m (RProgram, EncodeState)
findProgram env dst st = do
    modify $ set splitTypes []
    modify $ set typeAssignment Map.empty
    writeLog 2 $ text "calling findProgram"
    (codeResult, st') <- findPath env dst st
    oldSemantic <- view abstractionTree <$> get
    writeLog 2 $ pretty (Set.toList codeResult)
    checkResult <- withTime "type checking time" (firstCheckedOrError $ sortOn length (Set.toList codeResult))
    if isLeft checkResult
       then let Left code = checkResult in checkSolution st' code
       else let Right err = checkResult in findNextSolution st' oldSemantic err
  where
    firstCheckedOrError [] = return (Right (uHole, AExclusion Set.empty))
    firstCheckedOrError (x:xs) = do
        res <- parseAndCheck x
        case res of
          Left prog -> return (Left prog)
          Right err -> do
              res' <- firstCheckedOrError xs
              case res' of
                Left prog -> return (Left prog)
                Right _   -> return (Right err)

    parseAndCheck code = do
        let prog = case parseExp code of
                       ParseOk exp -> toSynquidProgram exp
                       ParseFailed loc err -> error err
        mapping <- view nameMapping <$> get
        writeLog 1 $ text "Find program" <+> pretty (recoverNames mapping prog)
        modify $ set isChecked True
        (btm, at) <- bottomUpCheck env prog
        mapping <- view nameMapping <$> get
        writeLog 3 $ text "bottom up checking get program" <+> pretty (recoverNames mapping btm)
        ifM (view isChecked <$> get)
            (do
                solveTypeConstraint env (shape (typeOf btm)) (shape dst)
                at' <- strengthenRoot env at (shape dst) (shape (typeOf btm))
                ifM (view isChecked <$> get)
                    (return (Left btm))
                    (return (Right (btm, at'))))
            (return (Right (btm, at)))

    findNextSolution st oldSemantic (prog, at) = do
        rs <- view refineStrategy <$> get
        case rs of
            NoRefine -> findProgram env dst st
            Combination -> do
                splitInfo <- withTime "refinement time" (refineSemantic env prog at)
                -- add new places and transitions into the petri net
                newSemantic <- view abstractionTree <$> get
                refine st oldSemantic newSemantic splitInfo
            AbstractRefinement -> do
                splitInfo <- withTime "refinement time" (refineSemantic env prog at)
                -- add new places and transitions into the petri net
                newSemantic <- view abstractionTree <$> get
                refine st oldSemantic newSemantic splitInfo
            QueryRefinement -> do
                splitInfo <- withTime "refinement time" (refineSemantic env prog at)
                -- add new places and transitions into the petri net
                newSemantic <- view abstractionTree <$> get
                refine st oldSemantic newSemantic splitInfo

    refine st oldSemantic newSemantic info | oldSemantic == newSemantic = do
        let st' = st { prevChecked = True }
        findProgram env dst st'
    refine st oldSemantic newSemantic info = do
        modify $ over solverStats (\s -> s {
            iterations = iterations s + 1
        })
        st' <- withTime "encoding time" (fixEncoder env dst st info)
        sigs <- view currentSigs <$> get
        dsigs <- view detailedSigs <$> get
        findProgram env dst st'

    checkSolution st code = do
        let st' = st { prevChecked = True }
        solutions <- view currentSolutions <$> get
        mapping <- view nameMapping <$> get
        let code' = recoverNames mapping code
        checkedSols <- withTime "type checking time" (filterM (liftIO . haskellTypeChecks env dst) [code'])
        if (code' `elem` solutions) || (null checkedSols)
           then do
               findProgram env dst st'
           else do
               modify $ over currentSolutions ((:) code')
               return $ (code', st')

printSolution solution = do
    liftIO $ putStrLn "*******************SOLUTION*********************"
    liftIO $ putStrLn $ "SOLUTION: " ++ (mkOneLine $ show solution)
    liftIO $ putStrLn "************************************************"


findFirstN :: (MonadIO m) => Environment -> RType -> EncodeState -> Int -> PNSolver m RProgram
findFirstN env dst st cnt | cnt == 1  = do
    (res, _) <- withTime "total search time" $ findProgram env dst st
    printSolution res
    printStats
    return res
findFirstN env dst st cnt | otherwise = do
    (res, st') <- withTime "total search time" $ findProgram env dst st
    printSolution res
    printStats
    resetTiming
    findFirstN env dst st' (cnt-1)

runPNSolver :: MonadIO m => Environment -> Int -> RType -> PNSolver m RProgram
runPNSolver env cnt t = do
    initNet env
    st <- withTime "encoding time" (resetEncoder env t)
    findFirstN env t st cnt

recoverNames :: Map Id Id -> RProgram -> RProgram
recoverNames mapping (Program (PSymbol sym) t) =
    case Map.lookup sym mapping of
      Nothing -> Program (PSymbol (removeLast '_' sym)) t
      Just name -> Program (PSymbol (removeLast '_' name)) t
recoverNames mapping (Program (PApp pFun pArg) t) = Program (PApp pFun' pArg') t
  where
    pFun' = recoverNames mapping pFun
    pArg' = recoverNames mapping pArg
recoverNames mapping (Program (PFun x body) t) = Program (PFun x body') t
  where
    body' = recoverNames mapping body

-------------------------------------------------------------------------------
-- | helper functions
-------------------------------------------------------------------------------

writeLog level msg = do
    st <- get
    if level <= st ^. logLevel then traceShow (plain msg) $ return () else return ()

multiPermutation len elmts | len == 0 = []
multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
multiPermutation len elmts            = nubOrd $ [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]


replaceId a b = Text.unpack . Text.replace a b . Text.pack
mkPairMatch (FunctionCode name _ params ret) = FunctionCode (replaceId "Pair" "Pair_match" name) [] ret params 

var2any env t@(ScalarT (TypeVarT _ id) _) | isBound env id = t
var2any env t@(ScalarT (TypeVarT _ id) _) | otherwise = AnyT
var2any env (ScalarT (DatatypeT id args l) r) = ScalarT (DatatypeT id (map (var2any env) args) l) r
var2any env (FunctionT x tArg tRet) = FunctionT x (var2any env tArg) (var2any env tRet)

addCloneFunction ty = do
    let tyStr = show ty
    let fname = tyStr ++ "|clone"
    let fc = FunctionCode fname [] [tyStr] [tyStr, tyStr]
    let addTransition k tid = Map.insertWith union k [tid]
    modify $ over functionMap (HashMap.insert fname fc)
    -- modify $ over currentSigs (Map.insert fname fc)
    modify $ over type2transition (addTransition ty fname)

addHoParam t = do
    let addTransition k tid = Map.insertWith union k [tid]
    let params = init (decompose t)
    let ret = last (decompose t)
    let colorTr = show t ++ "|color"
    let uncolorTr = show t ++ "|uncolor"
    mapM_ (\k -> modify $ over type2transition (addTransition k colorTr)) params
    modify $ over type2transition (addTransition ret uncolorTr)

