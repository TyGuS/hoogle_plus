module PetriNet.PNSolver
  ( runPNSolver
  , nextSolution
  ) where

import           Control.Lens                   ( (^.)
                                                , over
                                                , set
                                                , view
                                                )
import           Control.Monad
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.Logic
import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Char                     as Char
import           Data.Data                      ( Data )
import           Data.Function                  ( on )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.List                      ( nub
                                                , sortBy
                                                , sortOn
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isNothing
                                                )
import           Data.Ord                       ( Down(Down) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           System.IO                      ( hFlush
                                                , stdout
                                                )

import qualified Hoogle
import           Language.Haskell.Exts.Parser   ( ParseResult(..)
                                                , parseExp
                                                )

import           Database.Environment
import           HooglePlus.CodeGenerator
import           HooglePlus.Refinement
import           PetriNet.PNEncoder
import           Types.Common
import           Types.Encoder           hiding ( incrementalSolving
                                                , varName
                                                )
import           Types.Environment
import           Types.Experiments
import           Types.Fresh
import           Types.Log
import           Types.Pretty
import           Types.Program
import           Types.Solver
import           Types.Type
import           Types.TypeChecker
import           Utility.Utils

import Debug.Trace

encodeFunction :: Id -> TypeSkeleton -> EncodedFunction
encodeFunction id t | pairProj `Text.isPrefixOf` id =
  case encodeFunction "__f" t of
    EncodedFunction _ [p1, p2] ret -> EncodedFunction id [p1] (p2 : ret)
    _                              -> error "encodeFunction: not a valid pair"
encodeFunction id t@(FunctionT _ tArg tRet) =
  EncodedFunction id (allArgTypes t) [lastType t]
encodeFunction id t = EncodedFunction id [] [t]

instantiate
  :: SolverMonad m
  => Environment
  -> Map Id SchemaSkeleton
  -> PNSolver m (Map Id TypeSkeleton)
instantiate env sigs = do
  modify $ set (refineState . toRemove) []
  blacks <- liftIO $ readFile "blacklist.txt"
  let sigs' =
        Map.withoutKeys sigs (Set.fromList $ map Text.pack $ words blacks)
  Map.fromList <$> instantiate' sigs'
 where
  instantiate' sigs = do
    tree <- gets $ view (refineState . abstractionCover)
    let typs = typesInCover tree
    writeLog 2 "instantiate" $ text "Current abstract types:" <+> pretty tree
    let bound = getBoundTypeVars env
    sigs' <- Map.toList <$> mapM (freshType bound . toMonotype) sigs
    writeLog 2 "instantiate" $ text "Number of signatures:" <+> pretty
      (length sigs')
    foldM (\acc -> (<$>) (acc ++) . uncurry (instantiateWith env typs)) [] sigs'

-- add Pair_match function as needed
instantiateWith
  :: SolverMonad m
  => Environment
  -> [TypeSkeleton]
  -> Id
  -> TypeSkeleton
  -> PNSolver m [(Id, TypeSkeleton)]
-- skip "snd" function, it would be handled together with "fst"
instantiateWith env typs id t | id == "snd" = return []
instantiateWith env typs id t               = do
  instMap <- gets $ view (refineState . instanceMapping)
  groups  <- gets $ view groupState
  let nameMap = nameToGroup groups
  let bvs     = getBoundTypeVars env
  (sigId, sigs) <- if id == "fst"
    then do -- this is hack, hope to get rid of it sometime
      first   <- freshType bvs t
      secod   <- toMonotype <$> findSymbol nameMap env "snd"
      fstSigs <- fullApplication bvs [] (toAbstractType first) typs
      sndSigs <- fullApplication bvs [] (toAbstractType secod) typs
      -- assertion, check they have same elements
      when (length fstSigs /= length sndSigs)
           (error "fst and snd have different number of instantiations")
      let matches = zipWith assemblePair fstSigs sndSigs
      return (pairProj, matches)
    else do
      t'           <- toAbstractType <$> freshType bvs t
      abstractSigs <- fullApplication bvs [] t' typs
      return (id, abstractSigs)

  -- get rid of duplicates and only keep the ones with refined information
  -- i.e. only return the newly added ones
  let sigs' = filter
        (\t ->
          not (isExistingInstance instMap sigId t)
            || isRefinedInstance bvs instMap sigId t
        )
        sigs
  writeLog 3 "instantiateWith"
    $   text "Number of instantiated signatures:"
    <+> pretty (length sigs')
    <+> text "for"
    <+> pretty id
  mapM (mkNewSig sigId) sigs'

-- | Do a DFS search for all possible instantiations of a given type
fullApplication
  :: SolverMonad m
  => [Id]
  -> [TypeSkeleton]
  -> TypeSkeleton
  -> [TypeSkeleton]
  -> PNSolver m [TypeSkeleton]
fullApplication bvs argsSoFar t@FunctionT{} typs = do
  cover <- gets $ view (refineState . abstractionCover)
  let typs = typesInCover cover
  applyResults <- mapM (abstractStep bvs cover t) typs
  let validResults = filter (not . isBot . fst) (zip applyResults typs)
  concat <$> mapM doDFS validResults
  where doDFS (t, arg) = fullApplication bvs (arg : argsSoFar) t typs
fullApplication bvs argsSoFar t _ = do
  cover <- gets $ view (refineState . abstractionCover)
  t'    <- currentAbstraction bvs cover t
  return [foldr (FunctionT "") t' (reverse argsSoFar)]

mkNewSig
  :: SolverMonad m => Id -> TypeSkeleton -> PNSolver m (Id, TypeSkeleton)
mkNewSig id ty = do
  instMap <- gets $ view (refineState . instanceMapping)
  -- function name do not need check duplicates from user defined names
  newId   <- if pairProj `Text.isPrefixOf` id
    then freshId [] (appendSuffix id "_")
    else freshId [] "f"
  -- when same arguments exist for a function, replace it
  when (isExistingInstance instMap id ty) (excludeUseless id ty)
  modify $ over (refineState . instanceName) (Map.insert newId id)
  modify $ over (refineState . instanceMapping)
                (HashMap.insert (id, absFunArgs id ty) (newId, ty))
  writeLog 3 "mkNewSig"
    $   text id
    <+> text "==>"
    <+> text newId
    <+> text "::"
    <+> pretty ty
  return (newId, ty)

splitTransition
  :: SolverMonad m
  => Environment
  -> TypeSkeleton
  -> Id
  -> PNSolver m [(Id, TypeSkeleton)]
splitTransition env newAt fid = do
  rep  <- getGroup fid
  sigs <- gets $ view (searchState . currentSigs)
  let ty = lookupWithError "currentSigs" rep sigs
  writeLog 3 "splitTransition"
    $   text "split transtion"
    <+> text fid
    <+> text "::"
    <+> pretty ty
  allSubsts ty
 where
  allSubsts ty = allSubsts' (lastType ty) (allArgTypes ty)

  allSubsts' ret args = do
    cover   <- gets $ view (refineState . abstractionCover)
    instMap <- gets $ view (refineState . instanceMapping)
    nameMap <- gets $ view (refineState . instanceName)
    splits  <- gets $ view (refineState . splitTypes)
    let parents = Map.keys $ Map.filter (Set.member newAt) cover
    if pairProj `Text.isPrefixOf` fid
      then do
        writeLog 3 "allSubsts'" $ "argument number:" <+> pretty (length args)
        let args'  = enumArgs parents (take 1 args)
        let fstRet = args !! 1
        let sndRet = ret
        fstType <- findSymbol nameMap env "fst"
        sndType <- findSymbol nameMap env "snd"
        let first = toAbstractType $ toMonotype fstType
        let secod = toAbstractType $ toMonotype sndType
        let tvs   = getBoundTypeVars env
        fstRets <- mapM (abstractApply tvs cover first) args'
        sndRets <- mapM (abstractApply tvs cover secod) args'
        let sameFunc (a, (fret, sret)) =
              equalAbstract tvs fret fstRet
                && equalAbstract tvs sret sndRet
                && head a
                == head args
        let validFunc (a, (fret, sret)) =
              not (sameFunc (a, (fret, sret))) && not (isBot fret) && not
                (isBot sret)
        let funcs = filter validFunc (zip args' (zip fstRets sndRets))
        let sigs = map
              (\([a], (ft, st)) ->
                assemblePair (FunctionT "x" a ft) (FunctionT "y" a st)
              )
              funcs
        let sigs' = filter (isNewInstance tvs instMap pairProj) sigs
        mapM (mkNewSig pairProj) sigs'
      else do
        let args' = enumArgs parents args
        writeLog 3 "allSubsts'" $ pretty args'
        funType <- toMonotype <$> findSymbol nameMap env fid
        writeLog 3 "allSubsts'" $ text fid <+> text "::" <+> pretty funType
        let absFunType = toAbstractType funType
        let tvs        = getBoundTypeVars env
        rets <- mapM (abstractApply tvs cover absFunType) args'
        writeLog 3 "allSubsts'" $ text fid <+> text "returns" <+> pretty rets
        let validFunc (a, r) =
              not (equalAbstract tvs ret r && a == args) && not (isBot r)
        let funcs   = filter validFunc (zip args' rets)
        let sigs    = map (\(args, res) -> foldr (FunctionT "") res args) funcs
        let groupId = lookupWithError "nameMapping" fid nameMap
        let sigs'   = filter (isNewInstance tvs instMap groupId) sigs
        mapM (mkNewSig groupId) sigs'

  enumArgs :: [TypeSkeleton] -> [TypeSkeleton] -> [[TypeSkeleton]]
  enumArgs parents [] = [[]]
  enumArgs parents (arg : args)
    | arg `elem` parents
    = let args' = enumArgs parents args
      in  [ a : as | a <- [arg, newAt], as <- args' ]
    | otherwise
    = map (arg :) (enumArgs parents args)

addSignatures
  :: SolverMonad m => Environment -> PNSolver m (Map Id TypeSkeleton)
addSignatures env = do
  let foArgs     = map fst $ getFirstOrderArgs env
  -- first abstraction all the symbols with fresh type variables and then instantiate them
  let envSymbols = allSymbols env
  let usefulPipe k _ = k `notElem` foArgs
  let usefulSymbols = Map.filterWithKey usefulPipe envSymbols
  let hoArgs        = getHigherOrderArgs env
  let bvs           = getBoundTypeVars env
  sigs  <- instantiate env usefulSymbols
  sigs' <- ifM (getExperiment coalesceTypes) (mkGroups sigs) (return sigs)
  writeLog 2 "addSignatures" $ text "Number of groups:" <+> pretty
    (Map.size sigs')
  mapM_ addEncodedFunction (Map.toList sigs')
  return sigs'

-- | Take a list of type signatures, group them by type and return the signatures for each group
mkGroups
  :: SolverMonad m => Map Id TypeSkeleton -> PNSolver m (Map Id TypeSkeleton)
mkGroups sigs = do
  let encodedSigs = Map.mapWithKey encodeFunction sigs
  newGroups <- groupSignatures encodedSigs
  modify $ over groupState (`mergeGroups` newGroups)
  -- after merging, we will know which groups are added
  groups <- gets $ view groupState
  -- note: sigs are the original signatures, they do not know the group ids
  -- therefore, we need to rename them with group ids and then return
  let newGroups =
        Map.mapMaybe (firstMatch (`Map.member` sigs)) (groupMap groups)
  return $ Map.map (flip (lookupWithError "sigs") sigs) newGroups

addMusters :: SolverMonad m => Id -> PNSolver m ()
addMusters arg = do
  nameMap <- gets $ view (refineState . instanceName)
  let eqArg n = n == arg || n == arg `Text.append` hoPostfix
  let argFuncs = Map.keys $ Map.filter eqArg nameMap
  argRps <- mapM getGroup argFuncs
  modify $ over (encoder . mustFirers) (Map.insert arg argRps)

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic
  :: SolverMonad m
  => Environment
  -> TProgram
  -> TypeSkeleton
  -> PNSolver m SplitInfo
refineSemantic env prog at = do
  cover <- gets $ view (refineState . abstractionCover)
  writeLog 2 "refineSemantic" $ text "Current abstract types:" <+> pretty cover
  -- back propagation of the error types to get all split information
  nameMapping <- gets $ view (refineState . instanceName)
  propagate nameMapping env prog $ toAbstractFun at
  writeLog 2 "refineSemantic" "Back propagation done"
  -- get the split pairs
  splits <- gets $ view (refineState . splitTypes)
  let tvs          = getBoundTypeVars env
  let sortedSplits = sortBy (flip (abstractCmp tvs)) (Set.toList splits)
  writeLog 2 "refineSemantic splitTypes" $ pretty sortedSplits
  -- get removed transitions
  splitInfos <- mapM (splitTransitions env) sortedSplits

  -- add clone functions and add them into new transition set
  noClone    <- getExperiment disableCopy
  cloneNames <- if noClone
    then return []
    else mapM addCloneFunction $ Set.toList splits
  -- update the higer order query arguments
  let hoArgs = getHigherOrderArgs env
  mapM_ (addMusters . fst) hoArgs
  -- call the refined encoder on these signatures and disabled signatures
  let splitInfo = SplitInfo { newPlaces    = Set.toList splits
                            , removedTrans = []
                            , newTrans     = cloneNames
                            }
  return $ unionsSplitInfo (splitInfo : splitInfos)

splitTransitions
  :: SolverMonad m => Environment -> TypeSkeleton -> PNSolver m SplitInfo
splitTransitions env at = do
  modify $ set (refineState . toRemove) []

  cover  <- gets $ view (refineState . abstractionCover)
  t2tr   <- gets $ view (encoder . variables . type2transition)
  groups <- gets $ view groupState
  let gm      = groupMap groups
  let parents = Map.keys $ Map.filter (Set.member at) cover
  let transIds =
        Set.unions $ map (\p -> Map.findWithDefault Set.empty p t2tr) parents
  let affectedGroups = Map.filterWithKey (\k _ -> k `Set.member` transIds) gm
  let fids           = Set.unions $ Map.elems affectedGroups
  sigs <- mapM (splitTransition env at) (Set.toList fids)
  let hoArgs = getHigherOrderArgs env
  let tvs    = getBoundTypeVars env
  sigs' <- mkGroups (Map.fromList $ concat sigs)
  mapM_ addEncodedFunction (Map.toList sigs')

  -- update the group information by the current toRemove
  toCoalesce <- getExperiment coalesceTypes
  removables <- changeGroups toCoalesce
  writeLog 3 "splitTransitions removables" $ pretty removables

  modify $ over (encoder . mustFirers)
                (Map.map (filter (`Set.notMember` removables)))
  modify $ over (refineState . instanceName) (`Map.withoutKeys` removables)
  return SplitInfo { newTrans     = Map.keys sigs'
                   , removedTrans = Set.toList removables
                   , newPlaces    = []
                   }
 where
  changeGroups toCoalesce = do
    if toCoalesce
      then do
        -- these removables are not in groupIds
        removables' <- gets $ view (refineState . toRemove)
        writeLog 3 "changeGroups" $ pretty removables'
        splitGroups removables'
      else do
        removables <- gets $ view (refineState . toRemove)
        return $ Set.fromList removables

splitGroups :: SolverMonad m => [Id] -> PNSolver m (Set Id)
splitGroups removables = do
    -- Step 1: modify groupmap to exclude all those in removables
    -- Some groups may no longer exist as a result of this operation.
    -- Some groups representative may not longer be valid.
  groups <- gets $ view groupState
  let gm' =
        Map.mapMaybe (shrinkSet (Set.fromList removables)) (groupMap groups)
  let t2g = Map.filter (`notElem` removables) (typeToGroup groups)
  modify $ over groupState $ \gs -> gs { groupMap = gm', typeToGroup = t2g }
  -- Step 2: examine the new group and decide what we can safely remove
  let toRemove = Map.keysSet (groupMap groups) `Set.difference` Map.keysSet gm'
  mapM_
    (\tr -> modify
      $ over (encoder . variables . type2transition) (Map.map $ Set.delete tr)
    )
    toRemove
  modify $ over (refineState . instanceName)
                (`Map.withoutKeys` Set.fromList removables)
  modify $ over (searchState . currentSigs) (`Map.withoutKeys` toRemove)
  return toRemove

initNet :: SolverMonad m => Environment -> PNSolver m ()
initNet env = do
    -- reset the solver state
  modify $ set (searchState . functionMap) HashMap.empty
  modify $ set (searchState . currentSigs) Map.empty
  modify $ set (encoder . variables . type2transition) Map.empty
  modify $ set (refineState . instanceMapping) HashMap.empty

  addSignatures env
  -- add clone functions for each type
  noClone <- getExperiment disableCopy
  unless noClone $ do
    ty2tr <- gets $ view (encoder . variables . type2transition)
    let allTy = Map.keys ty2tr
    mapM_ addCloneFunction allTy
  -- add higher order query arguments
  let hoArgs = getHigherOrderArgs env
  mapM_ (addMusters . fst) hoArgs
 where
  abstractSymbol
    :: SolverMonad m => Id -> TypeSkeleton -> PNSolver m (Id, TypeSkeleton)
  abstractSymbol id sch = do
    let bound = getBoundTypeVars env
    t <- freshType bound sch
    let absTy = toAbstractType t
    return (id, absTy)

addEncodedFunction :: SolverMonad m => (Id, TypeSkeleton) -> PNSolver m ()
addEncodedFunction (id, f) = do
  GroupResult _ t2g n2g <- gets $ view groupState
  let (ef, _) = Map.findMin $ Map.filter (== id) t2g
  let [ef'] = substName [id] [ef]
  modify $ over (searchState . functionMap) (HashMap.insert id ef')
  modify $ over (searchState . currentSigs) (Map.insert id f)
  -- store the used abstract types and their groups into mapping
  updateTy2Tr id f

resetEncoder :: (SolverMonad m) => Environment -> TypeSkeleton -> PNSolver m ()
resetEncoder env dst = do
  (srcTypes, tgt) <- updateSrcTgt env dst
  writeLog 2 "resetEncoder" $ text "parameter types are" <+> pretty srcTypes
  writeLog 2 "resetEncoder" $ text "return type is" <+> pretty tgt
  encodeState        <- gets $ view encoder
  params             <- gets $ view searchParams
  (loc, rets, funcs) <- prepEncoderArgs env tgt
  let encoder' = encodeState { _encSearchParams = params }
  let places   = Map.keys (encoder' ^. (variables . type2transition))
  writeLog 2 "resetEncoder" $ text "places:" <+> pretty places
  st <- liftIO $ encoderInit encoder' loc srcTypes rets funcs
  modify $ set encoder st

incEncoder :: SolverMonad m => Environment -> PNSolver m ()
incEncoder env = do
  tgt              <- gets $ view (refineState . targetType)
  src              <- gets $ view (refineState . sourceTypes)
  (_, rets, funcs) <- prepEncoderArgs env tgt
  st               <- gets $ view encoder
  st'              <- liftIO $ execStateT (encoderInc funcs src rets) st
  modify $ set encoder st'

findPath :: SolverMonad m => Environment -> TypeSkeleton -> PNSolver m [Id]
findPath env dst = do
  st         <- gets $ view encoder
  (res, st') <- liftIO $ encoderSolve st
  loc        <- gets $ view (searchState . currentLoc)
  maxDepth   <- getExperiment maxApplicationDepth
  modify $ set encoder st'
  case res of
    [] -> do
      when (loc >= maxDepth) $ error "cannot find a path"
      modify $ set (searchState . currentLoc) (loc + 1)
      incEncoder env >> findPath env dst
    _ -> return res

fixEncoder
  :: SolverMonad m => Environment -> TypeSkeleton -> SplitInfo -> PNSolver m ()
fixEncoder env dst info = do
  st    <- gets $ view encoder
  cover <- gets $ view (refineState . abstractionCover)
  writeLog 2 "fixEncoder" $ text "new abstraction cover:" <+> pretty
    (typesInCover cover)
  (srcTypes, tgt) <- updateSrcTgt env dst
  writeLog 2 "fixEncoder" $ text "fixed parameter types:" <+> pretty srcTypes
  writeLog 2 "fixEncoder" $ text "fixed return type:" <+> pretty tgt
  writeLog 3 "fixEncoder" $ text "get split information" </> pretty info
  modify
    $ over (encoder . variables . type2transition) (Map.filter (not . null))
  (loc, rets, _) <- prepEncoderArgs env tgt
  fm             <- gets $ view (searchState . functionMap)
  let funcs = map (fromJust . (`HashMap.lookup` fm)) (newTrans info)
  st' <- liftIO $ execStateT (encoderRefine info srcTypes rets funcs) st
  modify $ set encoder st'

findProgram
  :: SolverMonad m => Environment -> TypeSkeleton -> PNSolver m [TProgram]
findProgram env goal = do
  let dst = lastType goal
  modify $ set (refineState . splitTypes) Set.empty
  modify $ set (refineState . passOneOrMore) True
  modify $ over typeChecker $ \checker ->
    checker { getTypeAssignment = Map.empty }
  writeLog 2 "findProgram" $ text "calling findProgram"
  path <- findPath env dst
  writeLog 2 "findProgram" $ text "unfiltered path:" <+> pretty path
  let usefulTrans = filter skipClone path
  observeAllT $ enumeratePath env goal usefulTrans
  where skipClone = not . Text.isInfixOf "|clone"

enumeratePath
  :: SolverMonad m
  => Environment
  -> TypeSkeleton
  -> [Id]
  -> BackTrack m TProgram
enumeratePath env goal path = do
  groups  <- gets $ view groupState
  nameMap <- gets $ view (refineState . instanceName)
  -- let getGroup p = lookupWithError "nameToGroup" p (nameToGroup groups)
  let getFuncs p = Map.findWithDefault Set.empty p (groupMap groups)
  let substName x = lookupWithError "nameMapping" x nameMap
  let sortFuncs p = Set.toList p -- TODO: any useful trick?
  let allPaths = map (sortFuncs . getFuncs) path
  writeLog 2 "enumeratePath" $ pretty allPaths
  msum $ map (checkPath env goal) (sequence allPaths)

checkPath
  :: SolverMonad m
  => Environment
  -> TypeSkeleton
  -> [Id]
  -> BackTrack m TProgram
checkPath env goal path = do
    -- ensure the usage of all the higher order arguments
  disrel  <- getExperiment disableRelevancy
  nameMap <- gets $ view (refineState . instanceName)
  let hoArgs = map fst $ getHigherOrderArgs env
  let getRealName x =
        Text.replace hoPostfix "" $ lookupWithError "nameMapping" x nameMap
  let filterPaths p = disrel || all (`elem` map getRealName p) hoArgs
  guard (filterPaths path)
  modify $ set (refineState . passOneOrMore) False

  -- fill the sketch with the functions in the path
  codeResult <- fillSketch env path
  writeLog 1 "checkPath" $ pretty codeResult
  let dst = lastType goal
  checkResult <- parseAndCheck env dst codeResult
  writeLog 1 "checkPath" $ text "get result" <+> pretty (show checkResult)
  rs       <- getExperiment refineStrategy
  stop     <- getExperiment stopRefine
  placeNum <- getExperiment threshold
  cover    <- gets $ view (refineState . abstractionCover)
  case checkResult of
    Left err -> do
      writeLog 1 "checkPath" $ text "check failed for" <+> pretty codeResult
      modify $ set (refineState . lastError) err
      let stopRefine = not (doRefine rs) || stop && coverSize cover >= placeNum
      when stopRefine $ modify $ set (refineState . passOneOrMore) True
      mzero
    Right code -> return $ recoverNames nameMap code

-- TODO: maybe we can change the order here
-- once we get a correct solution, stop the refine
parseAndCheck
  :: SolverMonad m
  => Environment
  -> TypeSkeleton
  -> TProgram
  -> BackTrack m (Either CheckError TProgram)
parseAndCheck env dst code = do
  mapping <- gets $ view (refineState . instanceName)
  writeLog 1 "parseAndCheck" $ text "Find program" <+> pretty
    (recoverNames mapping code)
  checkerState <- gets $ view typeChecker
  llv          <- getLogLevel
  let checkerState' =
        checkerState { getTypeAssignment = Map.empty, clogLevel = llv }
  let (btm, checkerState) =
        runState (runExceptT (bottomUpCheck mapping env code)) checkerState'
  modify $ set typeChecker checkerState
  writeLog 2 "parseAndCheck" $ text "bottom up checking get program" <+> pretty
    (recoverNames mapping $ either id id btm)
  case btm of
    Left  prog -> return $ Left $ CheckError prog BotT
    Right prog -> checkCompleteProgram env dst prog

checkCompleteProgram
  :: SolverMonad m
  => Environment
  -> TypeSkeleton
  -> TProgram
  -> BackTrack m (Either CheckError TProgram)
checkCompleteProgram env dst prog = do
  let tyBtm = typeOf prog
  let bvs   = getBoundTypeVars env
  writeLog 2 "parseAndCheck"
    $   text "Checking type"
    <+> pretty tyBtm
    <+> text "against"
    <+> pretty dst
  let mbSubst = solveTypeConstraint bvs Map.empty (SubtypeOf tyBtm dst)
  case mbSubst of
    Nothing -> do
      let progTyp = typeOf prog
      checkerState <- gets $ view typeChecker
      let tass = getTypeAssignment checkerState
      writeLog 1 "parseAndCheck"
        $   text "Generalizing abstract type"
        <+> pretty tyBtm
      let actualTyp = toAbstractType $ typeSubstitute tass progTyp
      let expectTyp = toAbstractType dst
      absBtm <- once $ observeT $ pickGeneralization env actualTyp expectTyp
      return $ Left $ CheckError prog absBtm
    Just _ -> do
      modify $ set (refineState . passOneOrMore) True
      return $ Right prog

pickGeneralization
  :: SolverMonad m
  => Environment
  -> TypeSkeleton
  -> TypeSkeleton
  -> LogicT (BackTrack m) TypeSkeleton
pickGeneralization _   BotT _      = return BotT
pickGeneralization env ty   target = do
  let bound = getBoundTypeVars env
  ty' <- lift $ generalize bound ty
  let unifier = getUnifier bound [SubtypeOf ty' target]
  guard (isNothing unifier)
  return ty'

fillSketch :: SolverMonad m => Environment -> [Id] -> BackTrack m TProgram
fillSketch env firedTrans = do
  srcs    <- gets $ view (refineState . sourceTypes)
  nameMap <- gets $ view (refineState . instanceName)
  fm      <- gets $ view (searchState . functionMap)
  writeLog 1 "fillSketch" $ text "found path" <+> pretty firedTrans

  reps <- lift $ mapM getGroup firedTrans
  let sigs = substPair $ substName firedTrans $ map (findFunction fm) reps
  writeLog 1 "fillSketch" $ text "found filtered sigs" <+> pretty sigs
  let args = map fst $ getFirstOrderArgs env
  progSet <- generateCode env srcs args sigs
  let progList = sortOn (Data.Ord.Down . programSize) $ Set.toList progSet
  writeLog 2 "fillSketch" $ text "number of programs from sketch:" <+> pretty
    (length progList)
  msum $ map return progList
 where
  substPair :: [EncodedFunction] -> [EncodedFunction]
  substPair [] = []
  substPair (x : xs)
    | pairProj `Text.isPrefixOf` funName x
    = (x { funName   = Text.replace pairProj "fst" (funName x)
         , funReturn = [head (funReturn x)]
         }
      )
      : (x { funName   = Text.replace pairProj "snd" (funName x)
           , funReturn = [funReturn x !! 1]
           }
        )
      : substPair xs
    | otherwise
    = x : substPair xs

generateCode
  :: SolverMonad m
  => Environment
  -> [TypeSkeleton]
  -> [Id]
  -> [EncodedFunction]
  -> BackTrack m (Set TProgram)
generateCode env src args sigs = do
  tgt    <- gets $ view (refineState . targetType)
  cover  <- gets $ view (refineState . abstractionCover)
  disrel <- getExperiment disableRelevancy
  let bound = getBoundTypeVars env
  let rets = filter (isSubtypeOf bound tgt) (typesInCover cover)
  writeLog 1 "generateCode" $ pretty src
  writeLog 1 "generateCode" $ pretty rets
  return $ runCodeGenerator sigs src args rets disrel

nextSolution
  :: SolverMonad m => Environment -> TypeSkeleton -> PNSolver m [TProgram]
nextSolution env goal = do
    -- note: when we come to the next solution, there are two cases:
    -- case I: all of the programs from the previous path are spurious
    -- case II: some of the programs from the previous path are correct
    -- no matter in which case, we have checked every possible program
    -- corresponds to that path, so we may safely block this path anyway
  modify $ set (encoder . prevChecked) True
  hasPass <- gets $ view (refineState . passOneOrMore)
  if hasPass -- block the previous path and then search
    then findProgram env goal
    else do -- refine and then search
      let dst = lastType goal
      cover              <- gets $ view (refineState . abstractionCover)
      CheckError prog at <- gets $ view (refineState . lastError)
      splitInfo          <- refineSemantic env prog at
      writeLog 1 "nextSolution" $ text "get split info" <+> pretty splitInfo
      -- add new places and transitions into the petri net
      cover <- gets $ view (refineState . abstractionCover)
      funcs <- gets $ view (searchState . functionMap)
      fixEncoder env dst splitInfo
      findProgram env goal

runPNSolver
  :: SolverMonad m => Environment -> TypeSkeleton -> PNSolver m [TProgram]
runPNSolver env goal = do
  writeLog 3 "runPNSolver" $ text "all components" <+> pretty (allSymbols env)
  initNet env
  let t = lastType goal
  resetEncoder env t
  findProgram env goal

{- helper functions -}

recoverNames :: Map Id Id -> Program t -> Program t
recoverNames mapping (Program (PSymbol sym) t) = case Map.lookup sym mapping of
  Nothing -> Program (PSymbol (stripSuffix hoPostfix $ removeLast '_' sym)) t
  Just name ->
    Program (PSymbol (stripSuffix hoPostfix $ removeLast '_' name)) t
recoverNames mapping (Program (PApp fun pArg) t) = Program (PApp fun' pArg') t
 where
  fun' = case Map.lookup fun mapping of
    Nothing   -> stripSuffix hoPostfix $ removeLast '_' fun
    Just name -> stripSuffix hoPostfix $ removeLast '_' name
  pArg' = map (recoverNames mapping) pArg
recoverNames mapping (Program (PFun x body) t) = Program (PFun x body') t
  where body' = recoverNames mapping body
recoverNames _ p = p

substName :: [Id] -> [EncodedFunction] -> [EncodedFunction]
substName []       []         = []
substName (n : ns) (fc : fcs) = fc { funName = n } : substName ns fcs
substName _        _          = error "substName: inconsistent list lengths"

addCloneFunction :: SolverMonad m => TypeSkeleton -> PNSolver m Id
addCloneFunction ty = do
  let fname = Text.pack $ show ty ++ "|clone"
  let fc    = EncodedFunction fname [ty] [ty, ty]
  modify $ over (searchState . functionMap) (HashMap.insert fname fc)
  updateTy2Tr fname ty
  return fname

doRefine :: RefineStrategy -> Bool
doRefine NoGar      = False
doRefine NoGar0     = False
doRefine SypetClone = False
doRefine _          = True

updateTy2Tr :: SolverMonad m => Id -> TypeSkeleton -> PNSolver m ()
updateTy2Tr id f = do
  let addTransition k tid = Map.insertWith Set.union k (Set.singleton tid)
  let includedTyps = nub (allBaseTypes f)
  mapM_
    (\t -> modify
      $ over (encoder . variables . type2transition) (addTransition t id)
    )
    includedTyps

updateSrcTgt
  :: SolverMonad m
  => Environment
  -> TypeSkeleton
  -> PNSolver m ([TypeSkeleton], TypeSkeleton)
updateSrcTgt env dst = do
    -- reset source and destination types
  let binds = getBoundTypeVars env
  abstraction <- gets $ view (refineState . abstractionCover)
  tgt         <- currentAbstraction binds abstraction (toAbstractType dst)
  modify $ set (refineState . targetType) tgt

  let foArgs = map snd $ getFirstOrderArgs env
  srcTyps <- mapM (currentAbstraction binds abstraction . toAbstractType) foArgs
  modify $ set (refineState . sourceTypes) srcTyps
  return (srcTyps, tgt)

type EncoderArgs = (Int, [TypeSkeleton], [EncodedFunction])

prepEncoderArgs
  :: SolverMonad m => Environment -> TypeSkeleton -> PNSolver m EncoderArgs
prepEncoderArgs env tgt = do
  cover <- gets $ view (refineState . abstractionCover)
  loc   <- gets $ view (searchState . currentLoc)
  funcs <- gets $ view (searchState . functionMap)
  let bound   = getBoundTypeVars env
  let accepts = superTypeOf bound cover tgt
  let rets    = sortBy (abstractCmp bound) accepts
  let sigs    = HashMap.elems funcs
  return (loc, rets, sigs)

isExistingInstance :: InstanceMapping -> Id -> TypeSkeleton -> Bool
isExistingInstance instMap name typ =
  HashMap.member (name, absFunArgs name typ) instMap

isRefinedInstance :: [Id] -> InstanceMapping -> Id -> TypeSkeleton -> Bool
isRefinedInstance bvs instMap name typ =
  case HashMap.lookup (name, absFunArgs name typ) instMap of
    Nothing ->
      error "isDeperecatedInstance: cannot find the given instance record"
    Just (_, oldTyp) ->
      typ /= oldTyp && isSubtypeOf bvs (lastType typ) (lastType oldTyp)

isNewInstance :: [Id] -> InstanceMapping -> Id -> TypeSkeleton -> Bool
isNewInstance bvs instMap name typ =
  not (isExistingInstance instMap name typ)
    || isRefinedInstance bvs instMap name typ

excludeUseless :: SolverMonad m => Id -> TypeSkeleton -> PNSolver m ()
excludeUseless id ty = do
  instMap <- gets $ view (refineState . instanceMapping)
  case HashMap.lookup (id, absFunArgs id ty) instMap of
    Nothing -> error "excludeUseless: cannot exclude non-existing instance"
    Just (tid, ty') -> do
      writeLog 3 "excludeUseless"
        $   text "delete"
        <+> pretty tid
        <+> text "==>"
        <+> pretty id
        <+> text "::"
        <+> pretty ty'
      modify $ over (refineState . toRemove) (tid :)

getGroup :: SolverMonad m => Id -> PNSolver m Id
getGroup name = do
  groups <- gets $ view groupState
  let ngm = nameToGroup groups
  case Map.lookup name ngm of
    Nothing -> error "getGroup: cannot find the given name"
    Just gp -> do
      writeLog 3 "getGroup"
        $   text name
        <+> text "is contained in group"
        <+> pretty gp
      return gp

assemblePair :: TypeSkeleton -> TypeSkeleton -> TypeSkeleton
assemblePair first secod | absFunArgs "fst" first == absFunArgs "snd" secod =
  let FunctionT x p f = first
      FunctionT y _ s = secod
  in  FunctionT x p (FunctionT y f s)
assemblePair first second = error "fst and snd have different arguments"

findFunction :: HashMap Id EncodedFunction -> Id -> EncodedFunction
findFunction fm name = fromMaybe
  (error $ "cannot find function name " ++ Text.unpack name)
  (HashMap.lookup name fm)
