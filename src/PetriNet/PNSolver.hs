{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module PetriNet.PNSolver (runPNSolver) where

import Database.Convert
import Database.Generate
import Database.Utils
import Encoder.ConstraintEncoder
import HooglePlus.Abstraction
import HooglePlus.CodeFormer
import HooglePlus.Refinement
import HooglePlus.Stats
import HooglePlus.TypeChecker
import PetriNet.AbstractType
import HooglePlus.GHCChecker
import HooglePlus.FilterTest
import HooglePlus.Utils
import PetriNet.Transition
import PetriNet.Utils
import Synquid.Error
import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Utils
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Program
import Types.Solver
import Types.Type
import Types.IOFormat
import Types.TypeChecker

import Control.Concurrent.Chan
import Control.Lens
import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Extra
import qualified Data.Char as Char
import Data.Data (Data)
import Data.Either hiding (fromLeft, fromRight)
import Data.Foldable
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.List.Extra hiding (stripSuffix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ord
import Data.Tuple
import Debug.Trace
import Language.Haskell.Exts.Parser (ParseResult(..), parseExp)
import Text.Printf
import System.IO
import qualified Hoogle as Hoogle

addSignatures :: (ConstraintEncoder enc, MonadIO m)
              => Environment 
              -> PNSolver enc m (Map Id AbstractSkeleton)
addSignatures env = do
    let foArgs = Map.keys $ foArgsOf env
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    let envSymbols = allSymbols env
    let usefulPipe k _ = k `notElem` foArgs
    let usefulSymbols = Map.filterWithKey usefulPipe envSymbols
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let binds = env ^. boundTypeVars
    envSigs <- instantiate env usefulSymbols
    let sigs = envSigs

    sigs' <- ifM (getExperiment coalesceTypes) (mkGroups env sigs) (return sigs)
    modify $ over (searchState . activeSigs) (Set.union (Map.keysSet sigs'))
    mapM_ addEncodedFunction (Map.toList sigs')
    return sigs'

--------------------------------------------------------------------------------
-- | petri net construction and refinement
--------------------------------------------------------------------------------

initNet :: (ConstraintEncoder enc, MonadIO m) 
        => Environment 
        -> PNSolver enc m ()
initNet env = withTime ConstructionTime $ do
    -- reset the solver state
    modify $ set (searchState . functionMap) HashMap.empty
    modify $ set (searchState . currentSigs) Map.empty
    modify $ set (refineState . instanceMapping) HashMap.empty
    modify $ \st -> st { _encoder = setTy2tr HashMap.empty (_encoder st) }

    addSignatures env
    -- add clone functions for each type
    noClone <- getExperiment disableCopy
    unless noClone $ do
        ty2tr <- getTy2tr <$> (gets $ view encoder)
        let allTy = HashMap.keys ty2tr
        mapM_ addCloneFunction allTy
    -- add higher order query arguments
    let hoArgs = Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    mapM_ addMusters (Map.keys hoArgs)
  where
    abstractSymbol id sch = do
        let bound = env ^. boundTypeVars
        t <- freshType bound sch
        let absTy = toAbstractType t
        return (id, absTy)

resetEncoder :: (ConstraintEncoder enc, MonadIO m)
             => Environment 
             -> TypeSkeleton 
             -> PNSolver enc m ()
resetEncoder env dst = do
    (srcTypes, tgt) <- updateSrcTgt env dst
    writeLog 2 "resetEncoder" $ text "parameter types are" <+> pretty srcTypes
    writeLog 2 "resetEncoder" $ text "return type is" <+> pretty tgt
    encodeState <- gets $ view encoder
    params <- gets $ view searchParams
    (loc, rets, funcs) <- prepEncoderArgs env tgt
    let encoder' = setParams params encodeState
    encoder <- liftIO $ encoderInit encoder' loc srcTypes rets funcs 
    modify $ \st -> st { _encoder = encoder }

incEncoder :: (ConstraintEncoder enc, MonadIO m)
           => Environment
           -> PNSolver enc m ()
incEncoder env = do
    tgt <- gets $ view (refineState . targetType)
    src <- gets $ view (refineState . sourceTypes)
    (_, rets, funcs) <- prepEncoderArgs env tgt
    encoder <- gets $ view encoder
    encoder' <- liftIO $ encoderInc funcs src rets encoder
    modify $ \st -> st { _encoder = encoder' }

fixEncoder :: (ConstraintEncoder enc, MonadIO m)
           => Environment
           -> TypeSkeleton
           -> SplitInfo
           -> PNSolver enc m ()
fixEncoder env dst info = do
    st <- gets $ view encoder
    cover <- gets $ view (refineState . abstractionCover)
    writeLog 2 "fixEncoder" $ text "new abstraction cover:" <+> pretty (allTypesOf cover)
    (srcTypes, tgt) <- updateSrcTgt env dst
    writeLog 1 "fixEncoder" $ text "fixed parameter types:" <+> pretty srcTypes
    writeLog 1 "fixEncoder" $ text "fixed return type:" <+> pretty tgt
    writeLog 3 "fixEncoder" $ text "get split information" </> pretty info
    modify $ \st -> 
        st { _encoder = modifyTy2tr (HashMap.filter (not . null)) (_encoder st) }
    (loc, rets, _) <- prepEncoderArgs env tgt
    fm <- gets $ view (searchState . functionMap)
    let funcs = map (fromJust . (`HashMap.lookup` fm)) (newTrans info)
    encoder' <- liftIO $ encoderRefine info srcTypes rets funcs st
    modify $ \st -> st { _encoder = encoder' }

findPath :: (ConstraintEncoder enc, MonadIO m)
         => Environment
         -> TypeSkeleton
         -> PNSolver enc m [Id]
findPath env dst = do
    encoder <- gets $ view encoder
    (res, encoder') <- withTime SolverTime $ liftIO $ encoderSolve encoder
    modify $ \st -> st { _encoder = encoder' }
    case res of
        [] -> do
            loc <- gets $ view (searchState . currentLoc)
            maxDepth <- getExperiment maxApplicationDepth
            when (loc >= maxDepth) $ do
                mesgChan <- gets $ view messageChan
                liftIO $ writeChan mesgChan (MesgClose CSNoSolution)
                error "cannot find a path"
            modify $ set (searchState . currentLoc) (loc + 1)
            withTime EncodingTime $ incEncoder env
            findPath env dst
        _  -> return res

findProgram :: (ConstraintEncoder enc, MonadIO m)
            => Environment -- the search environment
            -> SchemaSkeleton     -- the goal type
            -> [Example]   -- examples for post-filtering
            -> Int         -- remaining number of solutions to be found
            -> PNSolver enc m ()
findProgram env goal examples cnt = do
    let dst = lastType (toMonotype goal)
    modify $ set (refineState . splitTypes) Set.empty
    modify $ set (refineState . passOneOrMore) True
    modify $ set (typeChecker . typeAssignment) Map.empty
    writeLog 2 "findProgram" $ text "calling findProgram"
    path <- findPath env dst
    writeLog 2 "findProgram" $ text "unfiltered path:" <+> pretty path
    let usefulTrans = filter skipClone path
    searchResults <- withTime FormerTime $ observeManyT cnt $
        enumeratePath env goal examples usefulTrans
    mapM_ handleResult searchResults
    let solnNum = length searchResults
    when (solnNum < cnt) -- get enough solutions, search search
         (nextSolution env goal examples (cnt - solnNum))
    where
        handleResult NotFound = error "NotFound appeared in search results"
        handleResult (Found (soln, exs)) = modify $ over (searchState . currentSolutions) ((:) soln)
        handleResult (MoreRefine err)  = error "Should not encounter more refine"

        skipClone = not . isInfixOf "|clone"

enumeratePath :: (ConstraintEncoder enc, MonadIO m)
              => Environment
              -> SchemaSkeleton
              -> [Example] 
              -> [Id] 
              -> BackTrack enc m SearchResult
enumeratePath env goal examples path = do
    ngm <- gets $ view (groupState . nameToGroup)
    gm <- gets $ view (groupState . groupMap)
    uc <- gets $ view (statistics . useCount)
    nameMap <- gets $ view (typeChecker . nameMapping)
    let getGroup p = lookupWithError "nameToGroup" p ngm
    let getFuncs p = Map.findWithDefault Set.empty (getGroup p) gm
    let substName x = lookupWithError "nameMapping" x nameMap
    let nameCount x = Map.findWithDefault 0 (substName x) uc
    let sortFuncs p = sortOn nameCount $ Set.toList p
    let allPaths = map (sortFuncs . getFuncs) path
    writeLog 2 "enumeratePath" $ pretty allPaths
    msum $ map (checkPath env goal examples) (sequence allPaths)

checkPath :: (ConstraintEncoder enc, MonadIO m)
          => Environment 
          -> SchemaSkeleton
          -> [Example] 
          -> [Id] 
          -> BackTrack enc m SearchResult
checkPath env goal examples path = do
    -- ensure the usage of all the higher order arguments
    disrel <- getExperiment disableRelevancy
    nameMap <- gets $ view (typeChecker . nameMapping)
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let getRealName x = replaceId hoPostfix "" $ lookupWithError "nameMapping" x nameMap
    let filterPaths p = disrel || all (`elem` map getRealName p) hoArgs
    guard (filterPaths path)
    modify $ set (refineState . passOneOrMore) False

    -- fill the sketch with the functions in the path
    codeResult <- fillSketch env path
    writeLog 1 "checkPath" $ pretty codeResult
    let dst = lastType (toMonotype goal)
    checkResult <- withTime TypeCheckTime $ parseAndCheck env dst codeResult
    writeLog 1 "checkPath" $ text "get result" <+> text (show checkResult)
    rs <- getExperiment refineStrategy
    stop <- getExperiment stopRefine
    placeNum <- getExperiment threshold
    cover <- gets $ view (refineState . abstractionCover)
    case checkResult of
        Right code -> writeStats >> checkSolution env goal examples code
        Left err -> do
            modify $ set (refineState . lastError) err
            let stopRefine = not (doRefine rs) || (stop && coverSize cover >= placeNum)
            when stopRefine $ modify $ set (refineState . passOneOrMore) True
            mzero
    where
        writeStats = do
            cover <- gets $ view (refineState . abstractionCover)
            fm <- gets $ view (searchState . functionMap)
            currIter <- gets $ view (statistics . solverStats . iterations)
            let nextIter = currIter + 1
            modify $ over (statistics . solverStats . numOfPlaces) (Map.insert nextIter (coverSize cover))
            modify $ over (statistics . solverStats . numOfTransitions) (Map.insert nextIter (HashMap.size fm))

-- TODO: maybe we can change the order here
-- once we get a correct solution, stop the refine
parseAndCheck :: (ConstraintEncoder enc, MonadIO m)
              => Environment
              -> TypeSkeleton 
              -> String 
              -> BackTrack enc m (Either CheckError TProgram )
parseAndCheck env dst code = do
    prog <- case parseExp code of
                ParseOk exp         -> return (toSynquidProgram exp)
                ParseFailed loc err -> mzero
    writeLog 1 "parseAndCheck" $ text "Find program first" <+> pretty prog
    mapping <- gets $ view (typeChecker . nameMapping)
    counter <- gets $ view (typeChecker . nameCounter)
    writeLog 1 "parseAndCheck" $ text "Find program second" <+> pretty (recoverNames mapping prog)
    checkerState <- gets $ view typeChecker
    let checkerState' = checkerState { _typeAssignment = Map.empty, _isChecked = True }
    (btm, checkerState) <- runStateT (bottomUpCheck env prog) checkerState'
    modify $ set typeChecker checkerState
    writeLog 2 "parseAndCheck" $ text "bottom up checking get program" <+> pretty (recoverNames mapping btm)
    let checkStatus = checkerState ^. isChecked
    let tyBtm = typeOf btm
    writeLog 2 "parseAndCheck" $ pretty tyBtm <+> pretty dst
    let checkerState' = if checkStatus then execState (solveTypeConstraint env tyBtm dst) checkerState
                                       else checkerState
    let tass = checkerState' ^. typeAssignment
    modify $ set typeChecker checkerState'
    if checkerState' ^. isChecked
        then do
            modify $ set (refineState . passOneOrMore) True
            return (Right btm)
        else do
            writeLog 1 "parseAndCheck" $ text "Generalizing abstract type" <+> pretty tyBtm
            let tyBtm' = toAbstractType $ typeSubstitute tass tyBtm
            let absDst = toAbstractType dst
            absBtm <- once $ observeT $ pickGeneralization env tyBtm' absDst
            return (Left (btm, absBtm))

pickGeneralization :: (ConstraintEncoder enc, MonadIO m)
                   => Environment 
                   -> AbstractSkeleton 
                   -> AbstractSkeleton 
                   -> LogicT (BackTrack enc m) AbstractSkeleton
pickGeneralization _ BottomT _ = return BottomT
pickGeneralization env ty target = do
    let bound = env ^. boundTypeVars
    ty' <- lift $ generalize bound ty
    let unifier = getUnifier env ty' target
    guard (isNothing unifier)
    return ty'

fillSketch :: (ConstraintEncoder enc, MonadIO m)
           => Environment 
           -> [Id] 
           -> BackTrack enc m String
fillSketch env firedTrans = do
    src <- gets $ view (refineState . sourceTypes)
    nameMap <- gets $ view (typeChecker . nameMapping)
    repLists <- lift $ mapM getGroupRep firedTrans
    fm <- gets $ view (searchState . functionMap)
    let args = Map.keys $ foArgsOf env
    writeLog 1 "fillSketch" $ text "found path" <+> pretty firedTrans
    mapM_ (\f -> do
         let name = lookupWithError "nameMapping" f nameMap
         modify $ over (statistics . useCount) $ Map.insertWith (+) name 1) firedTrans
    let reps = map head repLists
    let sigs = substPair $ substName firedTrans $ map (findFunction fm) reps
    writeLog 1 "fillSketch" $ text "found filtered sigs" <+> pretty sigs
    let initialFormer = FormerState HashMap.empty []
    progSet <- withTime FormerTime $ generateCode initialFormer env src args sigs
    let progList = sortOn (Data.Ord.Down . length) $ Set.toList progSet
    msum $ map return progList
    where
        substPair [] = []
        substPair (x:xs) 
            | pairProj `isPrefixOf` funName x =
                ( x { funName = replaceId pairProj "fst" (funName x), funReturn = [head (funReturn x)] } )
              : ( x { funName = replaceId pairProj "snd" (funName x), funReturn = [funReturn x !! 1] } )
              : substPair xs
            | otherwise = x : substPair xs

generateCode :: (ConstraintEncoder enc, MonadIO m)
             => FormerState
             -> Environment
             -> [AbstractSkeleton]
             -> [Id]
             -> [FunctionCode]
             -> BackTrack enc m (Set String)
generateCode initialFormer env src args sigs = do
    tgt <- gets $ view (refineState . targetType)
    cover <- gets $ view (refineState . abstractionCover)
    disrel <- getExperiment disableRelevancy
    let rets = filter (isSubtypeOf env tgt) (allTypesOf cover)
    writeLog 1 "generateCode" $ pretty src
    writeLog 1 "generateCode" $ pretty rets
    liftIO (evalStateT (generateProgram sigs src args rets disrel) initialFormer)

nextSolution :: (ConstraintEncoder enc, MonadIO m)
             => Environment 
             -> SchemaSkeleton
             -> [Example] 
             -> Int
             -> PNSolver enc m ()
nextSolution env goal examples cnt = do
    -- note: when we come to the next solution, there are two cases:
    -- case I: all of the programs from the previous path are spurious
    -- case II: some of the programs from the previous path are correct
    -- no matter in which case, we have checked every possible program
    -- corresponds to that path, so we may safely block this path anyway
    blockCurrent
    hasPass <- gets $ view (refineState . passOneOrMore)
    if hasPass -- block the previous path and then search
       then findProgram env goal examples cnt
       else do -- refine and then search
            let dst = lastType (toMonotype goal)
            cover <- gets $ view (refineState . abstractionCover)
            (prog, at) <- gets $ view (refineState . lastError)
            splitInfo <- withTime RefinementTime (refineSemantic env prog at)
            writeLog 1 "nextSolution" $ text "get split info" <+> pretty splitInfo
            -- add new places and transitions into the petri net
            cover <- gets $ view (refineState . abstractionCover)
            funcs <- gets $ view (searchState . functionMap)
            currIter <- gets $ view (statistics . solverStats . iterations)
            modify $ over (statistics . solverStats . iterations) (+ 1)
            modify $ over (statistics . solverStats . numOfPlaces)
                          (Map.insert currIter (coverSize cover))
            modify $ over (statistics . solverStats . numOfTransitions)
                          (Map.insert currIter (HashMap.size funcs))
            withTime EncodingTime $ fixEncoder env dst splitInfo
            findProgram env goal examples cnt
    where
        blockCurrent = modify $ \st ->
            st { _encoder = setPrevChecked True (_encoder st) }

checkSolution :: (ConstraintEncoder enc, MonadIO m)
              => Environment 
              -> SchemaSkeleton
              -> [Example] 
              -> TProgram 
              -> BackTrack enc m SearchResult
checkSolution env goal examples code = do
    solutions <- gets $ view (searchState . currentSolutions)
    mapping <- gets $ view (typeChecker . nameMapping)
    params <- gets $ view searchParams
    msgChan <- gets $ view messageChan
    fState <- gets $ view filterState
    let code' = recoverNames mapping code
    (checkResult, fState') <- withTime TypeCheckTime $ 
        liftIO $ runStateT (check env params examples code' goal msgChan) fState
    modify $ set filterState fState'
    if (code' `elem` solutions) || isNothing checkResult
        then mzero
        else do
            let exs = fromJust checkResult
            out <- liftIO $ toOutput env code' exs
            lift $ writeSolution out
            return $ Found (code', exs)

runPNSolver :: (ConstraintEncoder enc, MonadIO m)
            => Environment 
            -> SchemaSkeleton 
            -> [Example] 
            -> PNSolver enc m ()
runPNSolver env goal examples = do
    writeLog 3 "runPNSolver" $ text $ show (allSymbols env)
    cnt <- getExperiment solutionCnt
    withTime TotalSearch $ initNet env
    let t = lastType (toMonotype goal)
    withTime TotalSearch $ withTime EncodingTime $ resetEncoder env t
    -- findFirstN env goal st examples [] cnt
    findProgram env goal examples cnt
    msgChan <- gets $ view messageChan
    liftIO $ writeChan msgChan (MesgClose CSNormal)

--------------------------------------------------------------------------------
-- Helper functions for printing results
--------------------------------------------------------------------------------
writeSolution :: (ConstraintEncoder enc, MonadIO m)
              => QueryOutput 
              -> PNSolver enc m ()
writeSolution out = do
    stats <- gets $ view (statistics . solverStats)
    loc <- gets $ view (searchState . currentLoc)
    msgChan <- gets $ view messageChan
    let stats' = stats { _pathLength = loc }
    liftIO $ writeChan msgChan (MesgP (out, stats', undefined))
    writeLog 1 "writeSolution" $ text (show stats')

--------------------------------------------------------------------------------
-- Helper functions for refinements
--------------------------------------------------------------------------------
substName :: [Id] -> [FunctionCode] -> [FunctionCode]
substName [] [] = []
substName (n:ns) (fc:fcs) = fc { funName = n } : substName ns fcs

updateSrcTgt :: (ConstraintEncoder enc, MonadIO m)
            => Environment
            -> TypeSkeleton
            -> PNSolver enc m ([AbstractSkeleton], AbstractSkeleton)
updateSrcTgt env dst = do
    -- reset source and destination types
    abstraction <- gets $ view (refineState . abstractionCover)
    tgt <- currentAbst env abstraction (toAbstractType dst)
    modify $ set (refineState . targetType) tgt

    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    srcTypes <- mapM (currentAbst env abstraction . toAbstractType . toMonotype) $ Map.elems foArgs
    modify $ set (refineState . sourceTypes) srcTypes
    return (srcTypes, tgt)

type EncoderArgs = (Int, [AbstractSkeleton], [FunctionCode])

prepEncoderArgs :: (ConstraintEncoder enc, MonadIO m) => Environment -> AbstractSkeleton -> PNSolver enc m EncoderArgs
prepEncoderArgs env tgt = do
    cover <- gets $ view (refineState . abstractionCover)
    loc <- gets $ view (searchState . currentLoc)
    funcs <- gets $ view (searchState . functionMap)
    let accepts = superTypeOf env cover tgt
    let rets = sortBy (compareAbstract env) accepts
    let sigs = HashMap.elems funcs
    return (loc, rets, sigs)

foArgsOf :: Environment -> Map Id SchemaSkeleton
foArgsOf = Map.filter (not . isFunctionType . toMonotype) . _arguments

findFunction :: HashMap Id FunctionCode -> Id -> FunctionCode
findFunction fm name = fromMaybe (error $ "cannot find function name " ++ name)
                                 (HashMap.lookup name fm)