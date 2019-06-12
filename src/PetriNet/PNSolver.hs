{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Text.Pretty.Simple
import Control.Concurrent.Chan

import Types.Common
import Types.Type
import Types.Environment
import Types.Abstract
import Types.Solver
import Types.Program
import Types.PetriNet
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
import PetriNet.PNBuilder
import PetriNet.PNEncoder
import PetriNet.PNEncoder
import PetriNet.GHCChecker
import PetriNet.Utils
import HooglePlus.Stats
import HooglePlus.CodeFormer
import HooglePlus.Abstraction
import HooglePlus.Refinement
import Database.Convert
import Database.Generate

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
        let typs = case st ^. (searchParams . refineStrategy) of
                     AbstractRefinement -> leafTypes (st ^. abstractionTree)
                     NoRefine -> filter notEx (leafTypes (st ^. abstractionTree))
                     Combination -> filter notEx (leafTypes (st ^. abstractionTree))
                     QueryRefinement -> leafTypes (st ^. abstractionTree)
        writeLog 3 "instantiate" $ text "Current abstract types:" <+> pretty typs
        sigs' <- foldM (\acc -> (<$>) ((++) acc) . uncurry (instantiateWith env typs)) [] sigs
        return $ nubOrdOn (uncurry removeSuffix) (sigsAcc ++ sigs')

instantiateWith :: (MonadIO m) => Environment -> [AbstractSkeleton] -> Id -> AbstractSkeleton -> PNSolver m [(Id, AbstractSkeleton)]
instantiateWith env typs id sk = do
    let vars = (Set.toList $ allAbstractVar sk) \\ (env ^. boundTypeVars)
    let multiSubsts = map (zip vars) $ multiPermutation (length vars) typs
    let substedSymbols = map (foldr (\(id, t) acc -> abstractSubstitute (env ^. boundTypeVars) id t acc) sk) multiSubsts
    st <- get
    let refinedSymbols = nubOrd (concatMap (cutoff env (st ^. abstractionTree)) substedSymbols)
    foldrM (\t accMap -> do
        newId <- if "Pair" `isPrefixOf` id then freshId (id++"_") else freshId "f"
        modify $ over nameMapping (Map.insert newId id)
        return $ (newId, t):accMap) [] refinedSymbols

splitTransition :: MonadIO m => Environment -> Id -> SplitInfo -> PNSolver m SplitInfo
splitTransition env tid info = do
    writeLog 3 "splitTransition" $ pretty tid
    sigs <- view currentSigs <$> get
    let splitedTyp = fst (head (splitedPlaces info))
    let newTyps = snd (head (splitedPlaces info))
    let typ = transitionSig sigs
    -- step 1: unify the type with refined abstract type and get new signatures
    unifyRes <- unifyNewType typ newTyps
    writeLog 3 "splitTransition - unifyRes" $ pretty unifyRes
    let unifiedTyps = nubOrdOn snd unifyRes
    -- step 2: add new transitions into the environment
    mapM_ (\(id, ty) -> do
        let add x = (modify $ over currentSigs (Map.insert id x))
        ifM (shouldDedupe)
            (do (error "not implemented")
                sigs <- view currentSigs <$> get
                let reversedSigs = groupByMap sigs
                if ty `Map.member` reversedSigs then
                    (writeLog 3 "splitTransition" $ text "not including sig" <+> pretty (id, ty))
                    else add ty)
            (add ty)) unifiedTyps
    mapM_ (\(id, ty) -> do
        let add x = (modify $ over functionMap (HashMap.insert id x))
        ifM (shouldDedupe)
            (do (error "not implemented")
                fm <- view functionMap <$> get
                let reversedFm = groupByMap (Map.fromList $ HashMap.toList fm)
                let encodedFunc = encodeFunction id ty
                if encodedFunc `Map.member` reversedFm then
                    (writeLog 3 "splitTransition" $ text "not including function" <+> pretty (id, ty))
                    else add encodedFunc)
            (add (encodeFunction id ty))) unifiedTyps
    mapM_ (uncurry typeMap) unifiedTyps
    -- step 3: pack the information into SplitInfo for incremental encoding
    let newIds = fst (unzip unifiedTyps)
    -- step 4: remove splited transition from the type2transition mapping
    modify $ over type2transition (Map.map (delete tid))
    -- return the new split information with splited transitions
    -- step 5: update the detailed signature ids
    modify $ over detailedSigs (Set.union (Set.fromList newIds) . Set.delete tid)
    if null newIds
       then return info
       else if "Pair" `isPrefixOf` tid
                then do
                    -- step 6: add pair pattern matching when we are splitting some Pair constructors, they share the same number with constructor
                    let tid' = Text.unpack (Text.replace "Pair" "Pair_match" (Text.pack tid))
                    let pairs = map (\(id, ty) -> mkPairMatch (encodeFunction id ty)) unifiedTyps
                    mapM_ (\(id, ty) -> do
                                            let AFunctionT arg0 (AFunctionT arg1 ret) = ty
                                            modify $ over currentSigs (Map.insert (replaceId "Pair" "fst" id) (AFunctionT ret arg0))
                                            modify $ over currentSigs (Map.insert (replaceId "Pair" "snd" id) (AFunctionT ret arg1))) unifiedTyps
                    mapM_ (\ef -> modify $ over functionMap (HashMap.insert (funName ef) ef)) pairs
                    let newIds' = map funName pairs
                    return (info { splitedGroup = (tid', newIds'):(tid, newIds):(splitedGroup info) })
                else return (info { splitedGroup = (tid, newIds):(splitedGroup info) })
  where

    getHoParams (AFunctionT tArg tRet) = init (decompose tArg) ++ getHoParams tRet
    getHoParams _ = []

    typeMap id ty = do
        let tys = decompose ty
        mapM_ (\t -> modify $ over type2transition (Map.insertWith union t [id])) tys

    transitionSig sigs = case Map.lookup tid sigs of
                                Just sig -> sig
                                Nothing -> error $ printf "cannot find transition %s in sig map" tid

    matchTyps target (AFunctionT tArg tRet) (AFunctionT tArg' tRet') =
        matchTyps target tArg tArg' ++ matchTyps target tRet tRet'
    matchTyps target absTyp polyTyp
        | target == absTyp = [polyTyp]
        | otherwise = []

    genTypes pattern newTyps (AFunctionT tArg tRet) =
            [ AFunctionT arg ret | arg <- if null args then [tArg] else args
                                 , ret <- if null rets then [tRet] else rets ]
        where
            args = genTypes pattern newTyps tArg
            rets = genTypes pattern newTyps tRet
    genTypes pattern newTyps t = if t == pattern then newTyps else []

    unifyNewType typ newTyps = do
        let id' = removeLast '_' tid
        polyTyp <- toAbstractType . shape <$> findSymbol env id'
        -- pass the current abstract hierarchy into the unifier
        abstraction <- view abstractionTree <$> get
        -- get all the constraints
        let typs = genTypes (fst (head (splitedPlaces info))) newTyps typ
        let constraints = map (\t -> typeConstraints t polyTyp) typs
        writeLog 3 "unifyNewType" $ text "trying to solve constraints" <+> pretty constraints
        let unifiers = map (getUnifier abstraction (env ^. boundTypeVars) (Just Map.empty)) constraints
        writeLog 3 "unifyNewType" $ text "unify result is" <+> pretty (map (Map.toList <$>) unifiers)
        let checkSuccess = filter isJust unifiers
        if not (null checkSuccess)
           then do
               let ts = snd (unzip (filter (isJust . fst) (zip unifiers typs)))
               writeLog 3 "unifyNewType" $ text "get signatures" <+> pretty ts
               ids <- mapM (\_ -> if Map.member id' (env ^. arguments) || "Pair" `isPrefixOf` id' then freshId (id'++"_") else freshId "f") ts
               mapping <- view nameMapping <$> get
               let actualName = case Map.lookup tid mapping of
                                  Nothing -> error $ "cannot find name " ++ tid ++ " in the function name mapping"
                                  Just n -> n
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
    writeLog 2 "distinguish" $ text "type assignments" <+> text (show tass)
    let t1' = var2any env (stypeSubstitute tass t1)
    let t2' = var2any env (stypeSubstitute tass t2)
    let ats1 = cutoff env semantic (toAbstractType t1')
    let ats2 = cutoff env semantic (toAbstractType t2')
    writeLog 3 "distinguish" $ text "trying to distinguish" <+> pretty t1' <+> text "==>" <+> pretty ats1 <+> text "and" <+> pretty t2' <+> text "==>" <+> pretty ats2
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
    let name = case Map.lookup sym nameMap of
                 Nothing -> sym -- error $ "cannot find function name " ++ sym ++ " in the name mapping"
                 Just n -> n
    case lookupSymbol name 0 env of
        Nothing -> do
            case lookupSymbol ("(" ++ name ++ ")") 0 env of
                Nothing -> do
                    modify $ set isChecked False
                    writeLog 2 "findSymbol" $ text "cannot find symbol" <+> text name <+> text "in the current environment"
                    return AnyT
                Just sch -> freshType sch
        Just sch -> freshType sch

strengthenRoot :: MonadIO m => Environment -> AbstractSkeleton -> SType -> SType -> PNSolver m AbstractSkeleton
strengthenRoot env dfault expected real = do
    diff <- distinguish env expected real
    writeLog 3 "strengthenRoot" $ text "strengthen root with expected" <+> pretty expected <+> text "and real" <+> pretty real <+> text "and get" <+> pretty diff
    tass <- view typeAssignment <$> get
    semantic <- view abstractionTree <$> get
    let expected' = var2any env (stypeSubstitute tass expected)
    let real' = var2any env (stypeSubstitute tass real)
    case diff of
      Nothing -> do -- they already have different abstract types, we need to distinguish the concrete type from its abstract one
          {- let realAt = toAbstractType real'
          let realAt' = head (cutoff env semantic realAt)
          let Just (t1, t2) = distinguish' (env ^. boundTypeVars) realAt realAt realAt'
          let semantic' = updateSemantic env semantic t2
          return (head (cutoff env semantic' (toAbstractType expected'))) -}
          return (head (cutoff env semantic (toAbstractType expected')))
      Just (t1, t2) -> do -- they have the same abstract type, we need to assign new abstraction for them
          let semantic' = updateSemantic env semantic t2
          -- modify $ over splitTypes ((:) (t1, t2))
          return (head (cutoff env semantic' (toAbstractType expected')))

strengthenArgs :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m (Maybe AbstractSkeleton)
strengthenArgs env (Program (PSymbol sym) typ) at = do
    semantic <- view abstractionTree <$> get
    sigs <- view currentSigs <$> get
    let at' = case Map.lookup sym sigs of
                     Nothing -> head (cutoff env semantic (toAbstractType (shape typ))) -- error $ "cannot find symbol " ++ sym ++ " in currentSigs"
                     Just t -> lastAbstract t
    -- let at' = head (cutoff env semantic (toAbstractType (shape (lastType typ))))
    when (isJust (abstractIntersection at' at))
         (do
             let at'' = if isSubtypeOf at at' then at else fromJust (abstractIntersection at' at)
             writeLog 3 "strengthArgs" $ text "add pair of split types" <+> text (show (at', at''))
             modify $ over splitTypes ((:) (at', at'')))
    -- unify the return type with the abstract type and get the type assignment
    t <- findSymbol env (removeLast '_' sym)
    let maybeTass = checkUnification semantic (env ^. boundTypeVars) Map.empty (at) (toAbstractType (shape (lastType t)))
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
strengthenArgs _ prog t = return Nothing -- error $ "unhandled pattern for " ++ show prog ++ " with abstract type " ++ show t

-- bottom up check a program on the concrete type system
-- at the same time, keep track of the abstract type for each node
bottomUpCheck :: (MonadIO m) => Environment -> RProgram -> PNSolver m (RProgram, AbstractSkeleton)
bottomUpCheck env p@(Program (PSymbol sym) typ) = do
    -- lookup the symbol type in current scope
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    t <- findSymbol env (removeLast '_' sym)
    sigs <- view currentSigs <$> get
    params <- view paramNames <$> get
    srcs <- view sourceTypes <$> get
    -- add arguments into this map
    let sigs' = foldr (uncurry Map.insert) sigs (zip params srcs)
    return (Program (PSymbol sym) t, case Map.lookup sym sigs' of
                                       Nothing -> error $ "cannot find symbol " ++ sym ++ " in signatures"
                                       Just n -> n)
bottomUpCheck env (Program (PApp pFun pArg) typ) = do
    (arg, ta) <- bottomUpCheck env pArg
    ifM (view isChecked <$> get)
        (do
            (fun, tf) <- bottomUpCheck env pFun
            ifM (view isChecked <$> get)
                (do
                    let FunctionT _ tArg tRet = typeOf fun
                    let AFunctionT atArg atRet = tf
                    writeLog 3 "bottomUpCheck" $ text "Solving constraint for" <+> pretty arg <+> text "::" <+> pretty (shape $ typeOf arg) <+> text "==" <+> pretty (shape tArg)
                    solveTypeConstraint env (shape $ typeOf arg) (shape tArg)
                    ifM (view isChecked <$> get)
                        (return (Program (PApp fun arg) tRet, atRet))
                        (do
                            atArg' <- strengthenRoot env atArg (shape tArg) (shape (typeOf arg))
                            return (arg, atArg')))
                (return (fun, tf)))
        (return (arg, ta))
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    (body', at) <- bottomUpCheck (addVariable x (addTrue tArg) env) body
    ifM (view isChecked <$> get)
        (return (Program (PFun x body') (FunctionT x tArg (typeOf body')), AFunctionT (toAbstractType (shape tArg)) at))
        (return (body', at))
bottomUpCheck env p@(Program (PFun x body) _) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
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
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv
            solveTypeConstraint env tv typ
        else unify env id' tv
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv'
            solveTypeConstraint env typ tv'
        else if id' `Map.member` (st ^. typeAssignment)
            then do
                let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
                writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> "==" <+> pretty typ
                solveTypeConstraint env tv typ
            else do
                unify env id tv'
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty t
            solveTypeConstraint env typ t
        else do
            unify env id t
solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tArg <+> "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    st <- get
    when (st ^. isChecked) (do
        writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tRet <+> "==" <+> pretty tRet'
        solveTypeConstraint env tRet tRet')
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id /= id' = do
    modify $ set isChecked False
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id == id' = do
    solveTypeConstraint' env tArgs tArgs'
  where
    solveTypeConstraint' _ []  [] = return ()
    solveTypeConstraint' env (ty:tys) (ty':tys') = do
        writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty ty <+> "==" <+> pretty ty'
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
    let groups = flattenInfo trs
    return (SplitInfo pls groups)
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
                                               else (tr, trs):(flattenInfo infos)

    hasNewSplit t1 t2 = do
        semantic <- view abstractionTree <$> get
        writeLog 3 "refineSemantic" $ text "check add type" <+> pretty t2 <+> text "into" </> pretty semantic
        let semantic' = updateSemantic env semantic t2
        return (semantic /= semantic')

    transSplit t1 t2 = do
        semantic <- view abstractionTree <$> get
        writeLog 3 "refineSemantic" $ text "add type" <+> pretty t2 <+> text "into" </> pretty semantic
        let semantic' = updateSemantic env semantic t2
        modify $ set abstractionTree semantic'
        let t2' = head (cutoff env semantic' t2)
        writeLog 3 "refineSemantic" $ pretty t1 <+> text "is splited into" <+> pretty t2'
        writeLog 3 "refineSemantic" $ text "new semantic is:" </> pretty semantic'
        t2tr <- view type2transition <$> get
        let tids = Map.findWithDefault [] t1 t2tr
        let nts = (leafTypes semantic') \\ (leafTypes semantic)
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
initNet env = withTime ConstructionTime $ do
    -- reset the solver state
    modify $ set functionMap HashMap.empty
    modify $ set currentSigs Map.empty
    modify $ set type2transition Map.empty

    let binds = env ^. boundTypeVars
    abstraction <- view abstractionTree <$> get
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    let srcTypes = map ( head
                       . cutoff env abstraction
                       . toAbstractType
                       . shape
                       . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    absSymbols <- mapM (uncurry abstractSymbol) (Map.toList (allSymbols env))
    -- first order arguments are tokens but not transitions in petri net
    let usefulSymbols = filter (flip notElem ("fst" : "snd" : Map.keys foArgs) . fst) absSymbols
    sigs <- instantiate env usefulSymbols
    modify $ set detailedSigs (Map.keysSet sigs)
    writeLog 3 "initNet" $ text "instantiated sigs" <+> pretty (Map.toList sigs)
    symbols <- mapM addEncodedFunction (Map.toList sigs)
    let symbols' = concat symbols
    -- Count the symbols with the SAME signature being put in the net.
    dupes <- countDuplicates symbols'
    symbols'' <- ifM (shouldDedupe) (
        do
            error "not implemented"
            (s, toBeRemoved) <- partitionDuplicateFunctions symbols'
            removeDuplicates toBeRemoved
            return s)
        (return symbols')
    gm <- view groupMap <$> get
    names <- view nameMapping <$> get
    let realNames = map (\x -> x ++ " → " ++ (fromJust $ Map.lookup x names)) (Map.keys sigs)
    writeLog 3 "initNet" $ text "duplicate funcs" <+> text dupes
    let net = (buildPetriNet symbols'' (map show srcTypes))
    modify $ set solverNet net
  where
    abstractSymbol id sch = do
        t <- freshType sch
        let absTy = toAbstractType (shape t) -- abstract binds abstraction (shape t)
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
        return [ef, mkPairMatch ef]
    addEncodedFunction (id, f) = do
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert id ef)
        modify $ over currentSigs (Map.insert id f)
        -- store the used abstract types and their groups into mapping
        let addTransition k tid = Map.insertWith union k [tid]
        let includedTyps = decompose f
        mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps
        return [ef]


resetEncoder :: (MonadIO m) => Environment -> RType -> PNSolver m EncodeState
resetEncoder env dst = do
    -- reset source and destination types
    let binds = env ^. boundTypeVars
    abstraction <- view abstractionTree <$> get
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
    srcTypes <- view sourceTypes <$> get
    writeLog 2 "resetEncoder" $ text "parameter types are" <+> pretty srcTypes
    writeLog 2 "resetEncoder" $ text "return type is" <+> pretty tgt

    -- reset the petri net
    net <- view solverNet <$> get
    -- filter the net with only the detailed signatures
    dsigs <- view detailedSigs <$> get
    sigs <- view currentSigs <$> get
    let removedIds = Map.keysSet sigs `Set.difference` dsigs
    writeLog 3 "resetEncoder" $ text "removing transitions" <+> (text (show removedIds))
    let net' = Set.foldr removeTransition net removedIds
    modify $ set solverNet net'
    loc <- view currentLoc <$> get
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    liftIO $ encoderInit net' loc hoArgs (map show srcTypes) (show tgt)

findPath :: (MonadIO m) => Environment -> RType -> EncodeState -> PNSolver m (CodePieces, EncodeState)
findPath env dst st = do
    (res, st') <- withTime SolverTime (liftIO (encoderSolve st))
    case res of
        [] -> do
            currSt <- get
            maxDepth <- view (searchParams . eGuessDepth) <$> get
            when (currSt ^. currentLoc >= maxDepth) (
              do
                mesgChan <- view messageChan <$> get
                liftIO $ writeChan mesgChan (MesgClose CSNoSolution)
                error "cannot find a path")
            modify $ set currentLoc ((currSt ^. currentLoc) + 1)
            -- initNet env
            st'' <- withTime EncodingTime (resetEncoder env dst)
            findPath env dst st''
        _  -> withTime FormerTime $ do
            fm <- view functionMap <$> get
            src <- view sourceTypes <$> get
            args <- view paramNames <$> get
            let sortedRes = sortOn snd res
            let transNames = map fst sortedRes
            writeLog 2 "findPath" $ text "found path" <+> pretty transNames
            let usefulTrans = filter (\n -> skipEntry n
                                         && skipClone n
                                         && skipDiscard n) transNames
            let sigNames = map removeSuffix usefulTrans
            dsigs <- view detailedSigs <$> get
            let sigNames' = filter (\name -> Set.member name dsigs || "Pair_match" `isPrefixOf` name) sigNames
            let sigs = substPair (map (findFunction fm) sigNames')
            writeLog 2 "findPath" $ text "found filtered sigs" <+> text (show sigs)
            -- let sigs = combinations (map (findFunctions fm groups) sigNames)
            let initialFormer = FormerState 0 HashMap.empty [] []
            code <- generateCode initialFormer (map show src) args sigs
            return (code, st')
  where
    findFunctions fm groups name =
        case Map.lookup name groups of
            Just g -> findFunction fm (head g) -- map (findFunction fm) (head g)
            Nothing -> error $ "cannot find function group " ++ name

    findFunction fm name =
        case HashMap.lookup name fm of
            Just fc -> fc
            Nothing -> error $ "cannot find function name " ++ name

    combinations []    = []
    combinations [x]   = [x]
    combinations (h:t) = [ hh:tt | hh <- h, tt <- combinations t ]


    generateCode initialFormer src args sigs = do
        tgt <- view targetType <$> get
        liftIO (evalStateT (generateProgram sigs src args (show tgt) True) initialFormer)

    skipEntry = not . isInfixOf "|entry"
    skipClone = not . isInfixOf "|clone"
    skipDiscard = not . isInfixOf "|discard"
    removeSuffix = removeLast '|'

    substPair [] = []
    substPair (x:xs) = if "Pair_match" `isPrefixOf` (funName x)
                          then   ( x { funName = replaceId "Pair_match" "fst" (funName x), funReturn = [(funReturn x) !! 0] } )
                               : ( x { funName = replaceId "Pair_match" "snd" (funName x), funReturn = [(funReturn x) !! 1] } )
                               : (substPair xs)
                          else x:(substPair xs)

fixNet :: MonadIO m => Environment -> SplitInfo -> PNSolver m ()
fixNet env (SplitInfo splitedTys splitedGps) = do
    -- reset the src types with the new abstraction semantic
    abstraction <- view abstractionTree <$> get
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    let srcTypes = map ( head
                       . cutoff env abstraction
                       . toAbstractType
                       . shape
                       . toMonotype) $ Map.elems foArgs
    oldSource <- view sourceTypes <$> get
    modify $ set sourceTypes srcTypes
    -- add new function into the petri net
    let newGroups = concat (snd (unzip splitedGps))
    writeLog 3 "fixNet" $ text "splitGroups" <+> pretty splitedGps
    writeLog 3 "fixNet" $ text "newGroups" <+> pretty newGroups
    sigs <- view currentSigs <$> get
    net <- view solverNet <$> get
    let sigs' = Map.filterWithKey (\k _ -> k `elem` newGroups) sigs
    writeLog 3 "fixNet" $ text "sigs to be added include" <+> pretty (Map.toList sigs')
    fm <- view functionMap <$> get
    let encodedFuncs = map (\name -> case HashMap.lookup name fm of
                                       Nothing -> error $ "cannot find function name " ++ name ++ " in functionMap"
                                       Just n -> n) newGroups
    let allFuncs = (HashMap.elems fm)
    dupes <- countDuplicates encodedFuncs
    writeLog 2 "fixNet" $ text "duplicate sigs" <+> text (countDuplicateSigs sigs)
    writeLog 1 "fixNet" $ text "duplicate funcs" <+> text dupes
    let newPlaces = (map show (concat (snd (unzip splitedTys))))
    functionedNet <- ifM (shouldDedupe) (
        do
            (error "not implemented")
            (deduplicatedFuncs, toBeRemoved) <- partitionDuplicateFunctions encodedFuncs
            removeDuplicates toBeRemoved
            writeLog 3 "fixNet" $ text "deduplicated funcs" <+> pretty (map funName deduplicatedFuncs)
            writeLog 3 "fixNet" $ text "to be removed funcs" <+> pretty (map funName toBeRemoved)
            return $ foldr addArgClone (foldr addFunction net deduplicatedFuncs) newPlaces)
            -- (return $ foldr addArgClone (foldr addFunction net encodedFuncs) newPlaces))
        (return $ foldr addArgClone (foldr addFunction net encodedFuncs) newPlaces)
    modify $ set solverNet functionedNet

fixEncoder :: MonadIO m => Environment -> RType -> EncodeState -> SplitInfo -> PNSolver m EncodeState
fixEncoder env dst st info@(SplitInfo splitedTys splitedGps) = do
    let binds = env ^. boundTypeVars
    abstraction <- view abstractionTree <$> get
    let tgt = head (cutoff env abstraction (toAbstractType (shape dst)))
    modify $ set targetType tgt
    srcTypes <- view sourceTypes <$> get
    writeLog 2 "fixEncoder" $ text "fixed parameter types are" <+> pretty srcTypes
    writeLog 2 "fixEncoder" $ text "fixed return type is" <+> pretty tgt
    loc <- view currentLoc <$> get
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    net <- view solverNet <$> get
    -- TODO remove dupes from the info's group
    let newGroups = concat (snd (unzip splitedGps))
    fm <- view functionMap <$> get
    let encodedFuncs = map (\name -> case HashMap.lookup name fm of
                                       Nothing -> error $ "cannot find function name " ++ name ++ " in functionMap"
                                       Just n -> n) newGroups
    shouldRemoveDuplicates' <- view (searchParams . shouldRemoveDuplicates) <$> get
    info' <- ifM (shouldDedupe) (
        do
            error "not implemented"
            (_, toBeRemoved) <- partitionDuplicateFunctions encodedFuncs
            let dupedNames = map funName toBeRemoved
            let newGroups = map (\(x, ys) -> (x, filter (not . (flip elem dupedNames)) ys)) splitedGps
            return (SplitInfo splitedTys newGroups))
        (return info)
    writeLog 2 "fixEncoder" $ text "get split information" <+> pretty info
    liftIO $ execStateT (encoderRefine net info' (map show srcTypes) (show tgt)) st

findProgram :: MonadIO m => Environment -> RType -> EncodeState -> PNSolver m (RProgram, EncodeState)
findProgram env dst st = do
    mesgChan <- view messageChan <$> get
    stats <- view solverStats <$> get
    liftIO $ writeChan mesgChan (MesgS stats)
    modify $ set splitTypes []
    modify $ set typeAssignment Map.empty
    writeLog 2 "findProgram" $ text "calling findProgram"
    (codeResult, st') <- findPath env dst st
    oldSemantic <- view abstractionTree <$> get
    writeLog 2 "findProgram" $ pretty (Set.toList codeResult)
    checkResult <- withTime TypeCheckTime (firstCheckedOrError $ sortOn length (Set.toList codeResult))
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
        writeLog 1 "findProgram" $ text "Find program" <+> pretty (recoverNames mapping prog)
        modify $ set isChecked True
        (btm, at) <- bottomUpCheck env prog
        mapping <- view nameMapping <$> get
        writeLog 3 "findProgram" $ text "bottom up checking get program" <+> pretty (recoverNames mapping btm)
        ifM (view isChecked <$> get)
            (do
                solveTypeConstraint env (shape (typeOf btm)) (shape dst)
                at' <- strengthenRoot env at (shape dst) (shape (typeOf btm))
                ifM (view isChecked <$> get)
                    (return (Left btm))
                    (return (Right (btm, at'))))
            (return (Right (btm, at)))

    findNextSolution st oldSemantic (prog, at) = do -- [errorProg, errorTop] = do
        rs <- gets $ view (searchParams . refineStrategy)
        case rs of
            NoRefine -> findProgram env dst st
            Combination -> do
                -- let splitInfo = SplitInfo (AExclusion Set.empty) [] []
                splitInfo <- withTime RefinementTime (refineSemantic env prog at)
                -- add new places and transitions into the petri net
                newSemantic <- view abstractionTree <$> get
                refine st oldSemantic newSemantic splitInfo
            AbstractRefinement -> do
                -- let splitInfo = SplitInfo (AExclusion Set.empty) [] []
                splitInfo <- withTime RefinementTime (refineSemantic env prog at)
                -- add new places and transitions into the petri net
                newSemantic <- view abstractionTree <$> get
                refine st oldSemantic newSemantic splitInfo
            QueryRefinement -> do
                splitInfo <- withTime RefinementTime (refineSemantic env prog at)
                -- add new places and transitions into the petri net
                newSemantic <- view abstractionTree <$> get
                refine st oldSemantic newSemantic splitInfo

    refine st oldSemantic newSemantic info | oldSemantic == newSemantic = do
        let st' = st { prevChecked = True }
        findProgram env dst st'
    refine st oldSemantic newSemantic info = do
        withTime ConstructionTime (fixNet env info)
        net' <- view solverNet <$> get
        modify $ over solverStats (\s -> s {
            iterations = iterations s + 1,
            numOfTransitions = Map.insert (iterations s + 1) (HashMap.size (pnTransitions net')) (numOfTransitions s),
            numOfPlaces = Map.insert (iterations s + 1) (HashMap.size (pnPlaces net')) (numOfPlaces s)
        })
        st' <- withTime EncodingTime (fixEncoder env dst st info)
        sigs <- view currentSigs <$> get
        dsigs <- view detailedSigs <$> get
        findProgram env dst st'

    checkSolution st code = do
        let st' = st { prevChecked = True }
        solutions <- view currentSolutions <$> get
        mapping <- view nameMapping <$> get
        let code' = recoverNames mapping code
        checkedSols <- withTime TypeCheckTime (filterM (liftIO . runGhcChecks env dst) [code'])
        if (code' `elem` solutions) || (null checkedSols)
           then do
               findProgram env dst st'
           else do
               -- printSolution solution
               -- printStats
               net' <- view solverNet <$> get
               modify $ over solverStats (\s -> s {
                   numOfTransitions = Map.insert (iterations s + 1) (HashMap.size (pnTransitions net')) (numOfTransitions s),
                   numOfPlaces = Map.insert (iterations s + 1) (HashMap.size (pnPlaces net')) (numOfPlaces s)
               })
               modify $ over currentSolutions ((:) code')
               return $ (code', st')

printSolution solution = do
    liftIO $ putStrLn "*******************SOLUTION*********************"
    liftIO $ putStrLn $ "SOLUTION: " ++ (mkOneLine $ show solution)
    liftIO $ putStrLn "************************************************"


findFirstN :: (MonadIO m) => Environment -> RType -> EncodeState -> Int -> PNSolver m ()
findFirstN env dst st cnt | cnt == 1  = do
    (res, _) <- withTime TotalSearch $ findProgram env dst st
    stats <- view solverStats <$> get
    depth <- view currentLoc <$> get
    msgChan <- view messageChan <$> get
    writeLog 1 "findFirstN" $ text (show depth)
    let stats' = stats{pathLength = depth}
    printSolution res
    -- printStats
    liftIO $ writeChan msgChan (MesgP (res, stats'))
findFirstN env dst st cnt | otherwise = do
    (res, st') <- withTime TotalSearch $ findProgram env dst st
    msgChan <- view messageChan <$> get
    stats <- view solverStats <$> get
    loc <- view currentLoc <$> get
    let stats' = stats{pathLength = loc}
    printSolution res
    liftIO $ writeChan msgChan (MesgP (res, stats'))
    resetTiming
    findFirstN env dst st' (cnt-1)

runPNSolver :: MonadIO m => Environment -> Int -> RType -> PNSolver m ()
runPNSolver env cnt t = do
    initNet env
    st <- withTime TotalSearch $ withTime EncodingTime (resetEncoder env t)
    findFirstN env t st cnt
    msgChan <- view messageChan <$> get
    liftIO $ writeChan msgChan (MesgClose CSNormal)
    return ()
