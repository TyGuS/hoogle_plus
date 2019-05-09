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
import Text.Pretty.Simple

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
import PetriNet.Util
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
encodeFunction id t@(AScalar {}) = FunctionCode id [] [] [show t]

instantiate :: MonadIO m => Environment -> Map Id RSchema -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = Map.fromList <$> instantiate' (Map.toList (Map.map toMonotype sigs))
  where
    instantiate' sigs = do
        tree <- gets (view abstractionTree)
        -- to test first level abstraction, please disable the complement type here
        let typs = map AScalar (leafTypes tree)
        writeLog 3 $ text "Current abstract types:" <+> pretty typs
        foldM (\acc -> (<$>) (acc ++) . uncurry (instantiateWith env typs)) [] sigs

instantiateWith :: MonadIO m => Environment -> [AbstractSkeleton] -> Id -> RType -> PNSolver m [(Id, AbstractSkeleton)]
instantiateWith env typs id t = do
    let varNum = nestedArity t + 1
    let multiSubsts = multiPermutation varNum typs
    foldM (\accMap ats -> do
        newId <- if "Pair" `isPrefixOf` id then freshId (id ++ "_") else freshId "f"
        modify $ over nameMapping (Map.insert newId id)
        t' <- shape <$> freshType (Monotype t)
        let at = construct ats t'
        constraints <- sort . Set.toList <$> allConstraints t' at
        let unifier = getUnifier (env ^. boundTypeVars) constraints
        if isJust unifier then return ((newId, at) : accMap)
                          else return accMap) [] multiSubsts
  where
    nestedArity (FunctionT _ tArg tRes) = 1 + nestedArity tArg + nestedArity tRes
    nestedArity _ = 0

    -- for higher order args
    construct ats (FunctionT _ tArg tRes) = let (argAt, resAt) = splitAt (nestedArity tArg + 1) ats
                                           in AFunctionT (construct argAt tArg) (construct resAt tRes)
    construct [at] t = at

    allConstraints (FunctionT _ tArg tRes) (AFunctionT aArg aRes) = do
        argCons <- allConstraints tArg aArg
        resCons <- allConstraints tRes aRes
        return (argCons `Set.union` resCons)
    allConstraints t (AScalar at) = do
        v <- freshId "_v"
        let local = TypeShape (ScalarT (TypeVarT Map.empty v) ()) t
        return (local `Set.insert` at)

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
    -- step 5: update the detailed signature ids
    modify $ over detailedSigs (Set.union (Set.fromList newIds) . Set.delete tid)
    -- step 6: return the new split information with splited transitions
    if null newIds
       then return info
       else do
           info <- addPairMatch unifiedTyps
           colorPairs <- mapM (addColorTrans typ) unifiedTyps
           let tyPairs = concat (fst (unzip colorPairs))
           let idPairs = concat (snd (unzip colorPairs))
           let pgs = groupOn fst (sortOn fst idPairs)
           let hops = groupOn fst (sortOn fst tyPairs)
           let tps = map (\xs -> (fst (head xs), nubOrd (snd (unzip xs)))) hops
           let newPairs = map (\xs -> (fst (head xs), nubOrd (snd (unzip xs)))) pgs
           return (info { splitedPlaces = splitedPlaces info ++ tps
                        , splitedGroup = (tid, newIds) : newPairs ++ splitedGroup info })
  where
    addPairMatch unifiedTyps | "Pair" `isPrefixOf` tid = do
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
        let appendUnColor n = n ++ "|uncolor"
        let hops = filter isAFunctionT (abstractParamList unifiedTyp)
        let oldHops = filter isAFunctionT (abstractParamList typ)
        mapM_ addHoParam hops
        let hoPairs = zip oldHops hops
        let uncolors = zip (map (appendUnColor . show) oldHops) (map (appendUnColor . show) hops)
        return (hoPairs, uncolors)
    addColorTrans _ _ = return ([], [])

    getHoParams (AFunctionT tArg tRet) = init (decompose tArg) ++ getHoParams tRet
    getHoParams _ = []

    typeMap id ty = do
        let tys = decomposeHo ty
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
        constraints <- mapM (`typeConstraints` polyTyp) typs
        writeLog 3 $ text "trying to solve constraints" <+> pretty constraints
        let unifiers = map (getUnifier (env ^. boundTypeVars) . sort . Set.toList) constraints
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


-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> AProgram -> AbstractSkeleton -> PNSolver m SplitInfo
refineSemantic env prog at = do
    -- back propagation of the error types to get all split information
    propagate env prog at
    -- get the split pairs
    splits <- gets (view splitTypes)
    -- the only problem is to deal with split one type more than once, let's assume it would not happen first
    splitInfos <- filterM (uncurry hasNewSplit) (nubOrdOn fst splits) >>= mapM (uncurry transSplit)
    let SplitInfo pls trs = combineInfo splitInfos
    return (SplitInfo pls (flattenInfo trs))
  where
    -- if any of the transitions is splitted again, merge the results
    combineInfo [] = SplitInfo [] []
    combineInfo (x:xs) = let SplitInfo ts trs = combineInfo xs
                             SplitInfo ps trs' = x
                          in SplitInfo (ps ++ ts) (trs ++ trs')

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
        writeLog 2 $ text "check add type" <+> pretty t2 <+> text "into" <+> pretty semantic
        let AScalar t2' = t2
        let semantic' = updateSemantic env semantic t2'
        return (semantic /= semantic')

    transSplit t1 t2 = do
        semantic <- view abstractionTree <$> get
        writeLog 2 $ text "add type" <+> pretty t2 <+> text "into" <+> pretty semantic
        let AScalar t2' = t2
        let semantic' = updateSemantic env semantic t2'
        modify $ set abstractionTree semantic'
        writeLog 2 $ text $ printf "%s is splited into %s" (show t1) (show t2)
        writeLog 2 $ text "new semantic is" <+> pretty semantic'
        t2tr <- view type2transition <$> get
        let tids = Map.findWithDefault [] t1 t2tr
        let nts = map AScalar (leafTypes semantic' \\ leafTypes semantic)
        if null tids
           then error "null transition splits"
               {-               -- find one of the leaf node to split at
               let t1' = head (filter (isSubtypeOf t2) (leafTypes semantic))
               let splitNode = SplitInfo [(t1', nts)] []
               let tids' = Map.findWithDefault [] t1' t2tr
               SplitInfo _ trs <- foldrM (splitTransition env) splitNode tids'
               return (SplitInfo [(t1, nts)] trs) -}
           else do
               let splitNode = SplitInfo [(t1, nts)] []
               foldrM (splitTransition env) splitNode tids

initNet :: MonadIO m => Environment -> PNSolver m ()
initNet env = withTime ConstructionTime $ do
    -- reset the solver state
    modify $ set functionMap HashMap.empty
    modify $ set currentSigs Map.empty
    modify $ set type2transition Map.empty

    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    let envSymbols = allSymbols env
    -- first order arguments are tokens but not transitions in petri net
    let usefulPipe k _ = k `notElem` ("fst" : "snd" : Map.keys foArgs)
    let usefulSymbols = Map.filterWithKey usefulPipe envSymbols
    sigs <- instantiate env usefulSymbols
    modify $ set detailedSigs (Map.keysSet sigs)
    writeLog 3 $ text "instantiated sigs" <+> pretty (Map.toList sigs)
    mapM_ addEncodedFunction (Map.toList sigs)
    -- add clone functions for each type
    allTy <- gets (Map.keys . view type2transition)
    mapM_ addCloneFunction (filter (not . isAFunctionT) allTy)  
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
        let includedTyps = decomposeHo f
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
        let addTransition k tid = Map.insertWith union k [tid]
        let includedTyps = decomposeHo f
        mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps
    addEncodedFunction (id, f) = do
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert id ef)
        modify $ over currentSigs (Map.insert id f)
        -- store the used abstract types and their groups into mapping
        let addTransition k tid = Map.insertWith union k [tid]
        let includedTyps = decomposeHo f
        mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps

resetEncoder :: (MonadIO m) => Environment -> RType -> PNSolver m EncodeState
resetEncoder env dst = do
    -- reset source and destination types
    let binds = env ^. boundTypeVars
    abstraction <- gets (view abstractionTree)
    let tgt = currentAbst binds abstraction (toAbstractType (shape dst))
    modify $ set targetType tgt
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    let srcTypes = map ( currentAbst binds abstraction
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

incEncoder :: MonadIO m => Environment -> EncodeState -> PNSolver m EncodeState
incEncoder env st = do
    tgt <- gets (view targetType)
    src <- gets (view sourceTypes)
    loc <- gets (view currentLoc)
    funcs <- gets (view functionMap)
    t2tr <- gets (view type2transition)
    let tid2tr = Map.foldrWithKey (\k v -> Map.insert (show k) v) Map.empty t2tr
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    liftIO $ execStateT (encoderInc (HashMap.elems funcs) (map show src) (show tgt)) st

findPath :: (MonadIO m) => Environment -> RType -> EncodeState -> PNSolver m (CodePieces, EncodeState)
findPath env dst st = do
    (res, st') <- withTime SolverTime (liftIO (encoderSolve st))
    case res of
        [] -> do
            currSt <- get
            maxDepth <- view maxApplicationDepth <$> get
            when (currSt ^. currentLoc >= maxDepth) (error "cannot find a path")
            modify $ set currentLoc ((currSt ^. currentLoc) + 1)
            st'' <- withTime EncodingTime (incEncoder env st')
            findPath env dst st''
        _  -> withTime FormerTime $ do
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
    let srcTypes = map ( currentAbst binds abstraction
                       . toAbstractType
                       . shape
                       . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    let tgt = currentAbst binds abstraction (toAbstractType (shape dst))
    modify $ set targetType tgt
    writeLog 2 $ text "fixed parameter types are" <+> pretty srcTypes
    writeLog 2 $ text "fixed return type is" <+> pretty tgt
    loc <- gets (view currentLoc)
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    writeLog 2 $ text "get split information" <+> pretty info
    let newTyps = concat (snd (unzip (splitedPlaces info)))
    mapM_ addCloneFunction (filter (not . isAFunctionT) newTyps)
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
    checkResult <- withTime TypeCheckTime (firstCheckedOrError $ sortOn length (Set.toList codeResult))
    rs <- gets (view refineStrategy)
    if isLeft checkResult
       then let Left code = checkResult in checkSolution st' code
       else let Right err = checkResult in nextSolution st' rs err
  where
    firstCheckedOrError [] = return (Right (Program PHole (AnyT, AnyT, AScalar Set.empty), AScalar Set.empty))
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
        modify $ set typeAssignment Map.empty
        btm <- bottomUpCheck env prog
        mapping <- view nameMapping <$> get
        writeLog 3 $ text "bottom up checking get program" <+> pretty (recoverNames mapping btm)
        checkStatus <- gets (view isChecked)
        let (tyBtm, typ, AScalar at) = typeOf btm
        let expected = shape (if checkStatus then dst else typ)
        when checkStatus (solveTypeConstraint env (shape tyBtm) (shape dst))
        ifM (view isChecked <$> get)
            (return (Left (pureType btm)))
            (do
                at' <- strengthenRoot env at expected (shape tyBtm)
                return (Right (btm, AScalar at')))

    nextSolution st NoRefine (prog, at) = findProgram env dst st
    nextSolution st _ (prog, at) = do
        splitInfo <- withTime RefinementTime (refineSemantic env prog at)
        -- add new places and transitions into the petri net
        newSemantic <- view abstractionTree <$> get
        refine st newSemantic splitInfo

    refine st newSemantic info = do
        modify $ over solverStats (\s -> s {
            iterations = iterations s + 1
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
        checkedSols <- withTime TypeCheckTime (filterM (liftIO . haskellTypeChecks env dst) [code'])
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


findFirstN :: (MonadIO m) => Environment -> RType -> EncodeState -> Int -> PNSolver m [(RProgram, TimeStatistics)]
findFirstN env dst st cnt | cnt == 1  = do
    (res, _) <- withTime TotalSearch $ findProgram env dst st
    stats <- view solverStats <$> get
    depth <- view currentLoc <$> get
    liftIO $ pPrint (depth)
    let stats' = stats{pathLength = depth}
    printSolution res
    -- printStats
    return [(res, stats')]
findFirstN env dst st cnt | otherwise = do
    (res, st') <- withTime TotalSearch $ findProgram env dst st
    stats <- view solverStats <$> get
    loc <- view currentLoc <$> get
    let stats' = stats{pathLength = loc}
    printSolution res
    -- printStats
    resetTiming
    rest <- (findFirstN env dst st' (cnt-1))
    return $ (res, stats'):rest

runPNSolver :: MonadIO m => Environment -> Int -> RType -> PNSolver m [(RProgram, TimeStatistics)]
runPNSolver env cnt t = do
    writeLog 3 $ text $ show (allSymbols env)
    initNet env
    st <- withTime EncodingTime (resetEncoder env t)
    findFirstN env t st cnt

recoverNames :: Map Id Id -> Program t -> Program t
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

{- helper functions -}
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
    let params = init (decomposeHo t)
    let ret = last (decomposeHo t)
    let uncolorTr = show t ++ "|uncolor"
    modify $ over type2transition (addTransition ret uncolorTr)
    modify $ over type2transition (addTransition t uncolorTr)

pureType = fmap (\(t1, t2, t3) -> t1)

currentAbst :: [Id] -> AbstractionTree -> AbstractSkeleton -> AbstractSkeleton
currentAbst tvs (ALeaf t) (AScalar at) = AScalar t
currentAbst tvs (ANode t l r) (AScalar at) | isJust (getUnifier tvs (sort (Set.toList (t `Set.union` at)))) =
    if isJust (getUnifier tvs (sort (Set.toList (nodeValue l `Set.union` at))))
       then currentAbst tvs l (AScalar at)
       else if isJust (getUnifier tvs (sort (Set.toList (nodeValue r `Set.union` at))))
                then currentAbst tvs r (AScalar at)
                else error $ "current abstraction for type " ++ show at ++ " is not a leaf type"
currentAbst _ _ (AScalar at) = error $ "cannot find abstraction for type " ++ show at
currentAbst tvs tree (AFunctionT tArg tRes) = AFunctionT tArg' tRes'
  where
    tArg' = currentAbst tvs tree tArg
    tRes' = currentAbst tvs tree tRes
