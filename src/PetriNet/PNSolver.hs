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
import Control.Monad.Logic
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
instantiate env sigs = do
    modify $ set toRemove []
    Map.fromList <$> instantiate' sigs
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
-- skip "snd" function, it would be handled together with "fst"
instantiateWith env typs id t | id == "snd" = return []
instantiateWith env typs id t = do
    abstraction <- gets (view abstractionTree)
    instMap <- gets (view instanceMapping)
    let bound = env ^. boundTypeVars
    if id == "fst"
       then do -- this is hack, hope to get rid of it sometime
           first <- freshType (Monotype t)
           secod <- findSymbol env "snd"
           fstSigs <- enumSigs $ toAbstractType $ shape first
           sndSigs <- enumSigs $ toAbstractType $ shape secod
           -- assertion, check they have same elements
           when (length fstSigs /= length sndSigs)
                (error "fst and snd have different number of instantiations")
           let matches = map (uncurry assemblePair) (zip fstSigs sndSigs)
           mapM (mkNewSig "pair_match") matches
       else do
           ft <- freshType (Monotype t)
           let t' = toAbstractType (shape ft)
           rawSigs <- enumSigs t'
           let sigs = filter (\t -> noneInst instMap id t || diffInst instMap id t) rawSigs
           mapM (mkNewSig id) sigs
  where
    noneInst instMap id t = not (HashMap.member (id, absFunArgs id t) instMap)

    diffInst instMap id t = (snd (fromJust $ HashMap.lookup (id, absFunArgs id t) instMap)) /= t

    assemblePair first secod | absFunArgs "fst" first == absFunArgs "snd" secod =
        let AFunctionT p f = first
            AFunctionT _ s = secod
         in AFunctionT p (AFunctionT f s)
    assemblePair first second | otherwise = error "fst and snd have different arguments"

    enumSigs typ = do
        let bound = env ^. boundTypeVars
        -- [TODO] to support higher order function, this is incorrect
        let argNum = length (decompose typ) - 1
        let allArgCombs = multiPermutation argNum typs
        applyRes <- mapM (applySemantic bound typ) allArgCombs
        let resSigs = filter (not . isBot . fst) (zip applyRes allArgCombs)
        return $ map (uncurry $ foldr AFunctionT) resSigs

    mkNewSig id ty = do
        instMap <- gets (view instanceMapping)
        newId <- if "pair_match" `isPrefixOf` id then freshId (id ++ "_") 
                                                 else freshId "f"
        -- when same arguments exist for a function, replace it
        when (not (noneInst instMap id ty)) (excludeUseless id ty)
        -- add corresponding must firers, function code and type mapping
        when (Map.member id (env ^. arguments)) 
             (modify $ over mustFirers (Map.insertWith (++) id [newId]))
        modify $ over nameMapping (Map.insert newId id)
        modify $ over instanceMapping (HashMap.insert (id, absFunArgs id ty) (newId, ty))
        writeLog 3 $ text newId <+> text "::" <+> pretty ty
        return (newId, ty)

    excludeUseless id ty = do
        instMap <- gets (view instanceMapping)
        let (tid, _) = fromJust (HashMap.lookup (id, absFunArgs id ty) instMap)
        -- writeLog 3 $ text tid <+> text "exists in the instance map for" <+> text id
        modify $ over toRemove ((:) tid)
        musts <- gets (view mustFirers)
        when (id `Map.member` musts)
             (modify $ over mustFirers (Map.insertWith (\new old -> old \\ new) id [tid]))

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m SplitInfo
refineSemantic env prog at = do
    -- back propagation of the error types to get all split information
    propagate env prog at
    -- get the split pairs
    splits <- gets (view splitTypes)
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    let envSymbols = allSymbols env
    -- first order arguments are tokens but not transitions in petri net
    let usefulPipe k _ = k `notElem` (Map.keys foArgs)
    let usefulSymbols = Map.filterWithKey usefulPipe envSymbols
    sigs <- instantiate env usefulSymbols
    modify $ over detailedSigs (Set.union (Map.keysSet sigs))
    mapM_ addEncodedFunction (Map.toList sigs)
    useless <- gets (view toRemove)
    -- add clone functions and add them into new transition set
    cloneNames <- mapM addCloneFunction $ Set.toList splits
    -- call the refined encoder on these signatures and disabled signatures
    return SplitInfo { newPlaces = Set.toList splits
                     , removedTrans = useless
                     , newTrans = Map.keys sigs ++ cloneNames
                     }

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
    let usefulPipe k _ = k `notElem` (Map.keys foArgs)
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

addEncodedFunction :: MonadIO m => (Id, AbstractSkeleton) -> PNSolver m ()
addEncodedFunction (id, f) | "pair_match" `isPrefixOf` id = do
    let toMatch (FunctionCode name ho [p1,p2] ret) = FunctionCode name ho [p1] (p2:ret)
    let ef = toMatch $ encodeFunction id f
    modify $ over functionMap (HashMap.insert id ef)
    modify $ over currentSigs (Map.insert id f)
    let AFunctionT ret (AFunctionT arg0 arg1) = f
    -- add fst and snd sigs
    modify $ over currentSigs (Map.insert (replaceId "pair_match" "fst" id) (AFunctionT ret arg0))
    modify $ over currentSigs (Map.insert (replaceId "pair_match" "snd" id) (AFunctionT ret arg1))
    -- store the used abstract types and their groups into mapping
    let addTransition k tid = Map.insertWith union k [tid]
    let includedTyps = nub (decompose f)
    mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps
addEncodedFunction (id, f) | isAHigherOrder f = do
    -- for higher order functions, we add coloring and uncoloring transitions
    let ef = encodeFunction id f
    modify $ over functionMap (HashMap.insert id ef)
    modify $ over currentSigs (Map.insert id f)
    -- add transitions to color and uncolor tokens
    let hops = filter isAFunctionT (abstractParamList f)
    mapM_ addHoParam hops
    let addTransition k tid = Map.insertWith union k [tid]
    let includedTyps = nub (decompose f)
    mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps
addEncodedFunction (id, f) = do
    let ef = encodeFunction id f
    modify $ over functionMap (HashMap.insert id ef)
    modify $ over currentSigs (Map.insert id f)
    -- store the used abstract types and their groups into mapping
    let addTransition k tid = Map.insertWith union k [tid]
    let includedTyps = nub (decompose f)
    mapM_ (\t -> modify $ over type2transition (addTransition t id)) includedTyps

resetEncoder :: (MonadIO m) => Environment -> RType -> PNSolver m EncodeState
resetEncoder env dst = do
    -- reset source and destination types
    let binds = env ^. boundTypeVars
    abstraction <- gets (view abstractionTree)
    tgt <- currentAbst binds abstraction (toAbstractType (shape dst))
    modify $ set targetType tgt
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    srcTypes <- mapM ( currentAbst binds abstraction
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
            let sigNames' = filter (\name -> Set.member name dsigs || "pair_match" `isPrefixOf` name) sigNames
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
    substPair (x:xs) = if "pair_match" `isPrefixOf` funName x
                          then   ( x { funName = replaceId "pair_match" "fst" (funName x), funReturn = [head (funReturn x)] } )
                               : ( x { funName = replaceId "pair_match" "snd" (funName x), funReturn = [funReturn x !! 1] } )
                               : substPair xs
                          else x : substPair xs

fixEncoder :: MonadIO m => Environment -> RType -> EncodeState -> SplitInfo -> PNSolver m EncodeState
fixEncoder env dst st info = do
    let binds = env ^. boundTypeVars
    abstraction <- gets (view abstractionTree)
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    srcTypes <- mapM ( currentAbst binds abstraction
                     . toAbstractType
                     . shape
                     . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    tgt <- currentAbst binds abstraction (toAbstractType (shape dst))
    modify $ set targetType tgt
    writeLog 2 $ text "fixed parameter types are" <+> pretty srcTypes
    writeLog 2 $ text "fixed return type is" <+> pretty tgt
    loc <- gets (view currentLoc)
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    writeLog 2 $ text "get split information" <+> pretty info
    let newTyps = newPlaces info
    mapM_ addCloneFunction (filter (not . isAFunctionT) newTyps)
    modify $ over type2transition (Map.filter (not . null))
    funcs <- gets (view functionMap)
    t2tr <- gets (view type2transition)
    musters <- gets (view mustFirers)
    let tid2tr = Map.foldrWithKey (\k v -> Map.insert (show k) v) Map.empty t2tr
    liftIO $ execStateT (encoderRefine info (HashMap.elems funcs) tid2tr (map show srcTypes) (show tgt) musters) st

findProgram :: MonadIO m => Environment -> RType -> EncodeState -> PNSolver m (RProgram, EncodeState)
findProgram env dst st = do
    modify $ set splitTypes Set.empty
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
    firstCheckedOrError [] = return (Right (uHole, AScalar (ATypeVarT varName)))
    firstCheckedOrError (x:xs) = do
        res <- parseAndCheck x
        case res of
          Left prog -> return (Left prog)
          Right err -> do
              res' <- firstCheckedOrError xs
              case res' of
                Left prog -> return (Left prog)
                Right _   -> return (Right err)

    pickGeneralization ABottom target = return ABottom
    pickGeneralization ty target = do
        let bound = env ^. boundTypeVars
        ty' <- generalize bound ty
        let unifier = getUnifier bound [(ty', target)]
        guard (isNothing unifier)
        return ty'

    parseAndCheck code = do
        let prog = case parseExp code of
                       ParseOk exp -> toSynquidProgram exp
                       ParseFailed loc err -> error err
        mapping <- view nameMapping <$> get
        writeLog 1 $ text "Find program" <+> pretty (recoverNames mapping prog)
        modify $ set isChecked True
        modify $ set typeAssignment Map.empty
        btm <- bottomUpCheck env prog
        writeLog 3 $ text "bottom up checking get program" <+> pretty (recoverNames mapping btm)
        checkStatus <- gets (view isChecked)
        let tyBtm = typeOf btm
        when checkStatus (solveTypeConstraint env (shape tyBtm) (shape dst))
        tass <- gets (view typeAssignment)
        ifM (view isChecked <$> get)
            (return (Left btm))
            (do
                let tyBtm' = toAbstractType $ stypeSubstitute tass $ shape tyBtm
                let absDst = toAbstractType $ shape dst
                absBtm <- observeT $ pickGeneralization tyBtm' absDst
                return (Right (btm, absBtm)))

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
recoverNames mapping (Program (PApp fun pArg) t) = Program (PApp fun' pArg') t
  where
    fun' = case Map.lookup fun mapping of
                Nothing -> removeLast '_' fun
                Just name -> removeLast '_' name
    pArg' = map (recoverNames mapping) pArg
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
    return fname

addHoParam t = do
    let addTransition k tid = Map.insertWith union k [tid]
    let params = init (nub (decomposeHo t))
    let ret = last (nub (decomposeHo t))
    let uncolorTr = show t ++ "|uncolor"
    modify $ over type2transition (addTransition ret uncolorTr)
    modify $ over type2transition (addTransition t uncolorTr)

attachLast :: AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
attachLast t (AFunctionT tArg tRes) | isAFunctionT tRes = AFunctionT tArg (attachLast t tRes)
attachLast t (AFunctionT tArg _) = AFunctionT tArg t
attachLast t _ = t
