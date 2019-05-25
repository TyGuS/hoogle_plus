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
import Types.Encoder hiding (varName, mustFirers)
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Program
import Synquid.Type
import Synquid.Logic hiding (varName)
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
instantiate env sigs = Map.fromList <$> instantiate' sigs
  where
    instantiate' sigs = do
        tree <- gets (view abstractionTree)
        let typs = Set.toList tree
        -- to test first level abstraction, please disable the complement type here
        writeLog 3 $ text "Current abstract types:" <+> pretty typs
        sigs' <- Map.toList <$> mapM freshType sigs
        foldM (\acc -> (<$>) (acc ++) . uncurry (instantiateWith env typs)) [] sigs'

-- add Pair_match function as needed
instantiateWith :: MonadIO m => Environment -> [AbstractSkeleton] -> Id -> RType -> PNSolver m [(Id, AbstractSkeleton)]
instantiateWith env typs id t = do
    abstraction <- gets (view abstractionTree)
    ft <- freshType (Monotype t)
    let t' = toAbstractType (shape ft)
    let bound = env ^. boundTypeVars
    let comps = decompose t'
    let vars = Set.toList (typeVarsOf ft)
    let typComp = [(comp, typ) | comp <- comps, typ <- typs]
    let unifiers = getUnifier bound typComp
    let usefulUnifs = filter isJust unifiers
    let unboxUnifs = nub (map fromJust usefulUnifs)
    let findSubst = Map.findWithDefault (AScalar (ATypeVarT varName))
    let mkSubst u = map (\id -> (id, findSubst id u)) vars
    let fullUnifs = map mkSubst unboxUnifs
    mapM (\subst -> do
        newId <- if "Pair" `isPrefixOf` id then freshId (id ++ "_") 
                                           else freshId "f"
        modify $ over nameMapping (Map.insert newId id)
        currentInsts <- gets (view instanceMapping)
        let sig = foldr (uncurry abstractSubstitute) t' subst
        let currSig = currentAbst bound abstraction sig
        -- check whether this signature is used
        -- TODO: check whether the hash function is correct here, if not change back to list and it is definitely correct
        modify $ over instanceMapping (Map.insert (id, subst) newId)
        when (Map.member id (env ^. arguments))
             (modify $ over mustFirers ((:) newId))
        return (newId, at'))

-- | we need to decide which transitions to be disabled after our current refinement
-- if any of the arguments in the signature after the refinement is more 
-- fine-grained than those in the polymorphic version 
splitTransition :: MonadIO m => Environment -> Id -> SplitInfo -> PNSolver m SplitInfo
splitTransition env tid info | "|clone" `isSuffixOf` tid = do
    let newTyps = snd (head (splitedPlaces info))
    let mkFc t = FunctionCode (show t ++ "|clone") [] [show t] [show t, show t]
    let unifiedTyps = map (\t -> (show t ++ "|clone", mkFc t)) newTyps
    -- step 1: add new transitions into the environment
    mapM_ (\(id, ty) -> modify $ over functionMap (HashMap.insert id ty)) unifiedTyps
    -- step 2: pack the information into SplitInfo for incremental encoding
    let newIds = fst (unzip unifiedTyps)
    -- step 3: return the new split information with splited transitions
    return (info { newTrans = (tid, newIds) : newTrans info })
splitTransition env tid info | "Pair_match" `isPrefixOf` tid || 
                               "|uncolor" `isSuffixOf` tid = do
    return info
splitTransition env tid info = do
    writeLog 3 $ pretty tid
    sigs <- gets (view currentSigs)
    abstraction <- gets (view abstractionTree)
    musters <- gets (view mustFirers)
    let splitedTyp = fst (head (splitedPlaces info))
    let newTyps = snd (head (splitedPlaces info))
    let typ = fromMaybe (error $ printf "cannot find transition %s in sig map" tid) (Map.lookup tid sigs)
    let id' = removeLast '_' tid
    nameMap <- gets (view nameMapping)
    let name = fromMaybe id' (Map.lookup id' nameMap)
    polyTyp <- findSymbol env id'
    -- step 1: unify the type with refined abstract type and get new signatures
    unifiedTyps <- unifyNewType typ newTyps
    symTyp <- toAbstractType . shape <$> findSymbol env tid 
    let bound = env ^. boundTypeVars
    -- let abstractCmp t1 t2 = if isSubtypeOf bound t2 t1 then LT else GT
    -- let sortedTyps = sortBy (\(_, t1) (_, t2) -> abstractCmp t1 t2) unifiedTyps
    -- writeLog 3 $ text "sorted signatures" <+> pretty sortedTyps
    -- let purgedTyps = purge symTyp sortedTyps
    -- writeLog 3 $ text "purged signatures" <+> pretty purgedTyps
    let purgedTyps = filter (\(_, t) -> t /= typ) unifiedTyps
    writeLog 3 $ text "new instantiated types" <+> pretty purgedTyps
    -- step 2: add new transitions into the environment
    mapM_ (\(id, ty) -> modify $ over currentSigs (Map.insert id ty)) purgedTyps
    mapM_ (\(id, ty) -> modify $ over functionMap (HashMap.insert id (encodeFunction id ty))) purgedTyps
    mapM_ (uncurry typeMap) purgedTyps
    when (tid `elem` musters) 
         (mapM_ (\(id,_) -> modify $ over mustFirers ((:) id)) purgedTyps)
    -- step 3: pack the information into SplitInfo for incremental encoding
    let newIds = fst (unzip purgedTyps)
    -- step 4: remove splited transition from the type2transition mapping when necessary
    -- if original type signature is the more or equal fine-grained than any instantiations,
    -- remove the old transition
    let fineCmp = map (isSubtypeOf bound symTyp . snd) purgedTyps
    let canDelete = typ `notElem` snd (unzip unifiedTyps)
    when canDelete (do
        modify $ over type2transition (Map.map (delete tid))
        modify $ over functionMap (HashMap.delete tid)
        -- step 5: update the detailed signature ids
        modify $ over detailedSigs (Set.delete tid)
        modify $ over mustFirers (delete tid))
    -- step 5: update the detailed signature ids
    modify $ over detailedSigs (Set.union (Set.fromList newIds))
    -- step 6: return the new split information with splited transitions
    if null newIds
       then return info
       else do
           info <- addPairMatch purgedTyps
           colorPairs <- mapM (addColorTrans typ) purgedTyps
           let tyPairs = concat (fst (unzip colorPairs))
           let idPairs = concat (snd (unzip colorPairs))
           let pgs = groupOn fst (sortOn fst idPairs)
           let hops = groupOn fst (sortOn fst tyPairs)
           let tps = map (\xs -> (fst (head xs), nubOrd (snd (unzip xs)))) hops
           let newPairs = map (\xs -> (fst (head xs), nubOrd (snd (unzip xs)))) pgs
           let tid' = Text.unpack (Text.replace "Pair" "Pair_match" (Text.pack tid))
           return (info { splitedPlaces = splitedPlaces info ++ tps
                        , removedTrans = if canDelete
                                            then if "Pair" `isPrefixOf` tid 
                                                    then tid':tid:removedTrans info 
                                                    else tid:removedTrans info 
                                            else removedTrans info
                        , newTrans = (tid, newIds) : newPairs ++ newTrans info })
  where
    restrictArgs abstraction (AScalar (ADatatypeT "Pair" args)) params =
        let args' = map (currentAbst (env ^. boundTypeVars) abstraction) args
            tyPairs = zip params args'
         in and (map (uncurry (isSubtypeOf (env ^. boundTypeVars))) tyPairs)
    restrictArgs _ _ _ = True

    addPairMatch unifiedTyps | "Pair" `isPrefixOf` tid = do
        abstraction <- gets (view abstractionTree)
        let tid' = Text.unpack (Text.replace "Pair" "Pair_match" (Text.pack tid))
        -- when the parameter are not most restrictive, do not add the pair projection
        let restrictPairs = filter (\(_, ty) -> let tys = decompose ty in restrictArgs abstraction (last tys) (init tys)) unifiedTyps
        let pairs = map (\(id, ty) -> mkPairMatch (encodeFunction id ty)) restrictPairs
        mapM_ (\(id, ty) -> do
                                let AFunctionT arg0 (AFunctionT arg1 ret) = ty
                                modify $ over currentSigs (Map.insert (replaceId "Pair" "fst" id) (AFunctionT ret arg0))
                                modify $ over currentSigs (Map.insert (replaceId "Pair" "snd" id) (AFunctionT ret arg1))) unifiedTyps
        mapM_ (\ef -> modify $ over functionMap (HashMap.insert (funName ef) ef)) pairs
        mapM_ (\(id, ty) -> typeMap (replaceId "Pair" "Pair_match" id) ty) restrictPairs
        let newIds' = map funName pairs
        return (info { newTrans = (tid', newIds') : newTrans info })
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

    getHoParams (AFunctionT tArg tRet) = init (decomposeHo tArg) ++ getHoParams tRet
    getHoParams _ = []

    typeMap id ty = do
        let tys = nub (decomposeHo ty)
        mapM_ (\t -> modify $ over type2transition (Map.insertWith union t [id])) tys

    genTypes pattern newTyps (AFunctionT tArg tRet) (AFunctionT tArg' tRet') =
            [ AFunctionT arg ret | arg <- args, ret <- rets ]
        where
            args = genTypes pattern newTyps tArg tArg'
            rets = if isAFunctionT tRet then genTypes pattern newTyps tRet tRet' else [tRet]
    genTypes pattern [] t t' = error "empty newTyps"
    genTypes pattern newTyps t t' | t == pattern = if isSubtypeOf (env ^. boundTypeVars) t' (head newTyps)
                                                      then newTyps 
                                                      else t:newTyps
                                  | otherwise = [t]

    isConsistent t1 t2 = let (_, res) = isConsistent' Map.empty t1 t2 in res
    -- unify all the arguments and substitute the result in the return value
    argConstraints (AFunctionT tArg tRes) (AFunctionT tArg' tRes') = do
        freshArg <- freshAbstract (env ^. boundTypeVars) tArg'
        resConstraints <- if isAFunctionT tRes then argConstraints tRes tRes' else return []
        return ((tArg, freshArg):resConstraints)
    isConsistent' m (AFunctionT tArg tRes) (AFunctionT tArg' tRes') =
        let (m', res) = isConsistent' m tArg tArg'
        in if res then isConsistent' m' tRes tRes'
                  else (m', res)
    isConsistent' m t1 t2 = if t1 `Map.member` m then (m, t2 == fromJust (Map.lookup t1 m))
                                                 else (Map.insert t1 t2 m, True)

    constructFinal res unifier t = do
        abstraction <- gets (view abstractionTree)
        let bound = env ^. boundTypeVars
        let res' = foldl' (\acc (id, t) -> abstractSubstitute id t acc) res (Map.toList unifier)
        (resCand, currList) <- constructFinal' res' unifier (AScalar (ATypeVarT varName), []) (Set.toList abstraction)
        if isSubtypeOf bound res' resCand
           then return [attachLast resCand t]
           else do
               -- we are underapproximating the return type
               -- relax the type until we are overapproximating it
               let resCands = takeWhile' res' currList
               let compTyps = map (`attachLast` t) resCands
               return compTyps

    insertTyp t [] = [t]
    insertTyp t (x:xs) | isSubtypeOf (env ^. boundTypeVars) t x = t:x:xs
    insertTyp t (x:xs) = x:(insertTyp t xs)

    takeWhile' _ [] = []
    takeWhile' t (x:xs) | equalAbstract (env ^. boundTypeVars) t x = [x]
                        | isSubtypeOf (env ^. boundTypeVars) x t = x:takeWhile' t xs
                        | otherwise = [x]

    constructFinal' res unifier sofar [] = return sofar
    constructFinal' res unifier sofar (t:ts) = do
        let bound = env ^. boundTypeVars
        t' <- freshAbstract bound t
        let unifier' = getUnifier' bound (Just unifier) [(res, t')]
        let (approx, currList) = sofar
        case unifier' of
            Nothing -> constructFinal' res unifier sofar ts
            Just u | isSubtypeOf bound t approx && (isSubtypeOf bound t res || isSubtypeOf bound res t) 
                        -> constructFinal' res unifier (t, insertTyp t currList) ts
                   | isSubtypeOf bound t res || isSubtypeOf bound res t 
                        -> constructFinal' res unifier (approx, insertTyp t currList) ts
                   | otherwise -> constructFinal' res unifier (approx, currList) ts

    unifyNewType typ newTyps = do
        let id' = removeLast '_' tid
        let bound = env ^. boundTypeVars
        polyTyp <- toAbstractType . shape <$> findSymbol env id'
        -- pass the current abstract hierarchy into the unifier
        abstraction <- view abstractionTree <$> get
        -- get all the constraints
        writeLog 3 $ pretty (splitedPlaces info)
        let typs = genTypes (fst (head (splitedPlaces info))) newTyps typ polyTyp
        constraints <- mapM (typeConstraints bound polyTyp) typs
        writeLog 3 $ text "trying to solve constraints" <+> pretty constraints
        let unifiers = map (getUnifier bound) constraints
        writeLog 3 $ text "unify result is" <+> pretty (show unifiers)
        let usefulUnifs = filter (isJust . fst) (zip unifiers typs)
        if not (null usefulUnifs)
           then do
               let unboxUnifs = map (\(u, t) -> (fromJust u, t)) usefulUnifs
               ts <- nub . filter (isConsistent polyTyp)
                        <$> if isAFunctionT polyTyp 
                               then concat <$> mapM (uncurry (constructFinal (lastAbstract polyTyp))) unboxUnifs
                               else return (snd (unzip unboxUnifs))
               -- writeLog 3 $ text "get signatures" <+> pretty ts
               -- let filteredTs = filter (isConsistent polyTyp) ts
               let filteredTs = ts
               ids <- mapM (\_ -> if Map.member id' (env ^. arguments) || "Pair" `isPrefixOf` id' then freshId (id'++"_") else freshId "f") filteredTs
               mapping <- view nameMapping <$> get
               let actualName = case Map.lookup tid mapping of
                                  Nothing -> error $ "cannot find name " ++ tid ++ " in the function name mapping"
                                  Just n -> n
               mapM_ (\name -> modify $ over nameMapping (Map.insert name actualName)) ids
               return (zip ids filteredTs)
           else return []

    -- we would delete t1 because t2 is more fine-grained than t1
    -- but t2 is less fine-grained than symTyp
    purge symTyp [] = []
    purge symTyp ((id, t):ts) = 
        if or (map (purge' symTyp t . snd) ts) then purge symTyp ts 
                                               else ((id, t) : purge symTyp ts)

    purge' (AFunctionT tArg tRes) (AFunctionT tArg1 tRes1) (AFunctionT tArg2 tRes2) | tArg1 == tArg2 = purge' tRes tRes1 tRes2
    purge' (AFunctionT tArg tRes) (AFunctionT tArg1 tRes1) (AFunctionT tArg2 tRes2) = 
        isSubtypeOf (env ^. boundTypeVars) tArg2 tArg1 &&
        isSubtypeOf (env ^. boundTypeVars) tArg tArg2
    purge' t t1 t2 | t1 == t2 = False
    purge' t t1 t2 = isSubtypeOf (env ^. boundTypeVars) t2 t1 && 
                     isSubtypeOf (env ^. boundTypeVars) t t2

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> AProgram -> AbstractSkeleton -> PNSolver m SplitInfo
refineSemantic env prog at = do
    -- back propagation of the error types to get all split information
    propagate env prog at
    -- get the split pairs
    splits <- gets (view splitTypes)
    -- writeLog 3 $ pretty splits
    splitInfos <- mapM transSplit splits
    let SplitInfo pls dtrs trs = combineInfo (concat splitInfos)
    -- writeLog 3 $ pretty trs
    (dtrs', trs') <- flattenInfo dtrs trs
    return (SplitInfo pls dtrs' trs')
  where
    -- if any of the transitions is splitted again, merge the results
    combineInfo [] = SplitInfo [] [] []
    combineInfo (x:xs) = let SplitInfo ts dtrs trs = combineInfo xs
                             SplitInfo ps dtrs' trs' = x
                          in SplitInfo (ps ++ ts) (dtrs ++ dtrs') (trs ++ trs')

    replaceTrans dtrs tr trs [] = return (False, dtrs, [])
    replaceTrans dtrs tr trs ((x,xs):remains) | tr `elem` xs = do
        modify $ over type2transition (Map.map (delete tr))
        modify $ over functionMap (HashMap.delete tr)
        -- step 5: update the detailed signature ids
        modify $ over detailedSigs (Set.delete tr)
        let dtrs' = delete tr dtrs
        return (True, dtrs', (x, trs ++ delete tr xs):remains)
    replaceTrans dtrs tr trs ((x,xs):remains) = do
        (res, dtrs', remains') <- replaceTrans dtrs tr trs remains
        return (res, dtrs', (x, xs):remains')

    flattenInfo dtrs [] = return (dtrs, [])
    flattenInfo dtrs ((tr, trs):infos) = do
        (res, dtrs', infos') <- replaceTrans dtrs tr trs infos
        if res then flattenInfo dtrs' infos' 
               else do
                   (dtrs'', infos') <- flattenInfo dtrs' infos
                   return (dtrs'', (tr, trs):infos')

    transSplit t = do
        semantic <- gets (view abstractionTree)
        writeLog 3 $ text "add type" <+> pretty t <+> text "into" <+> pretty (Set.toList semantic)
        let semantic' = t `Set.insert` semantic
        modify $ set abstractionTree semantic'
        -- writeLog 2 $ text $ printf "%s is splited into %s" (show t1) (show t2)
        writeLog 2 $ text "new semantic is" <+> pretty (Set.toList semantic')
        let bound = env ^. boundTypeVars
        t2tr <- view type2transition <$> get
        let potentialSplits = filter (isSubtypeOf bound t) (Set.toList semantic)
        mapM (\pt -> do
            let tids = Map.findWithDefault [] pt t2tr
            let nts = [t]
            let splitNode = SplitInfo [(pt, nts)] [] []
            foldrM (splitTransition env) splitNode tids) potentialSplits

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
        let includedTyps = nub (decomposeHo f)
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
        let includedTyps = nub (decomposeHo f)
        mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps
    addEncodedFunction (id, f) = do
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert id ef)
        modify $ over currentSigs (Map.insert id f)
        -- store the used abstract types and their groups into mapping
        let addTransition k tid = Map.insertWith union k [tid]
        let includedTyps = nub (decomposeHo f)
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
    musters <- gets (view mustFirers)
    let tid2tr = Map.foldrWithKey (\k v -> Map.insert (show k) v) Map.empty t2tr
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    liftIO $ encoderInit loc musters (map show srcTypes) (show tgt) (HashMap.elems funcs) tid2tr

incEncoder :: MonadIO m => Environment -> EncodeState -> PNSolver m EncodeState
incEncoder env st = do
    tgt <- gets (view targetType)
    src <- gets (view sourceTypes)
    loc <- gets (view currentLoc)
    funcs <- gets (view functionMap)
    t2tr <- gets (view type2transition)
    musters <- gets (view mustFirers)
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
    musters <- gets (view mustFirers)
    let tid2tr = Map.foldrWithKey (\k v -> Map.insert (show k) v) Map.empty t2tr
    liftIO $ execStateT (encoderRefine info (HashMap.elems funcs) tid2tr (map show srcTypes) (show tgt) musters) st

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
    firstCheckedOrError [] = return (Right (Program PHole (AnyT, AnyT, AScalar (ATypeVarT varName)), AScalar (ATypeVarT varName)))
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
        let (tyBtm, typ, at) = typeOf btm
        let expected = shape (if checkStatus then dst else typ)
        when checkStatus (solveTypeConstraint env (shape tyBtm) (shape dst))
        ifM (view isChecked <$> get)
            (return (Left (pureType btm)))
            (do
                at' <- strengthenRoot env at expected (shape tyBtm)
                return (Right (btm, at')))

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
    let params = init (nub (decomposeHo t))
    let ret = last (nub (decomposeHo t))
    let uncolorTr = show t ++ "|uncolor"
    modify $ over type2transition (addTransition ret uncolorTr)
    modify $ over type2transition (addTransition t uncolorTr)

pureType = fmap (\(t1, t2, t3) -> t1)

-- | find the current most restrictive abstraction for a given type
currentAbst :: [Id] -> Set AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
currentAbst tvs cover (AFunctionT tArg tRes) = AFunctionT tArg' tRes'
  where
    tArg' = currentAbst tvs cover tArg
    tRes' = currentAbst tvs cover tRes
currentAbst tvs cover at = currentAbst' tvs (Set.toList cover) at (AScalar (ATypeVarT varName))

currentAbst' :: [Id] -> [AbstractSkeleton] -> AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
currentAbst' _ [] _ sofar = sofar
currentAbst' tvs (t:ts) at sofar | isSubtypeOf tvs at t && isSubtypeOf tvs t sofar = currentAbst' tvs ts at t
currentAbst' tvs (t:ts) at sofar = currentAbst' tvs ts at sofar

attachLast :: AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
attachLast t (AFunctionT tArg tRes) | isAFunctionT tRes = AFunctionT tArg (attachLast t tRes)
attachLast t (AFunctionT tArg _) = AFunctionT tArg t
attachLast t _ = t
