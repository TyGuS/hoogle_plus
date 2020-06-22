{-# LANGUAGE FlexibleContexts #-}

module PetriNet.Transition where

import Database.Utils
import Encoder.ConstraintEncoder
import Types.Environment
import Types.Experiments
import Types.Type
import Types.Common
import Types.Solver
import Types.TypeChecker
import Synquid.Pretty
import Synquid.Type
import Synquid.Utils
import PetriNet.Utils
import PetriNet.AbstractType

import Control.Monad.State
import Control.Lens
import Data.Function
import Data.List
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Text.Printf

--------------------------------------------------------------------------------
-- transition initialization
--------------------------------------------------------------------------------

-- | instantiate polymorphic type variables with types in current abstraction cover
instantiate :: (ConstraintEncoder enc, MonadIO m)
            => Environment
            -> Map Id SchemaSkeleton
            -> PNSolver enc m (Map Id AbstractSkeleton)
instantiate env sigs = do
    modify $ set (refineState . toRemove) []
    noBlack <- getExperiment disableBlack
    blacks <- liftIO $ readFile "blacklist.txt"
    let filteredSigs = Map.filterWithKey (\k _ -> noBlack || k `notElem` words blacks) sigs
    Map.fromList <$> instantiate' filteredSigs
  where
    instantiate' sigs = do
        tree <- gets $ view (refineState . abstractionCover)
        writeLog 2 "instantiate" $ text "Current abstract types:" <+> text (show tree)
        let typs = allTypesOf tree
        let bound = env ^. boundTypeVars
        renamedSigs <- mapM (freshType bound) sigs
        instantiatedSigs <- mapM (uncurry (instantiateWith env typs)) (Map.toList renamedSigs)
        return (concat instantiatedSigs)

-- add Pair_match function as needed
instantiateWith :: (ConstraintEncoder enc, MonadIO m)
                => Environment
                -> [AbstractSkeleton]
                -> Id
                -> TypeSkeleton
                -> PNSolver enc m [(Id, AbstractSkeleton)]
-- skip "snd" function, it would be handled together with "fst"
instantiateWith env typs id t | id == "snd" = return []
instantiateWith env typs id t | id == "fst" = do
    instMap <- gets $ view (refineState . instanceMapping)
    let bound = env ^. boundTypeVars
    first <- freshType bound (Monotype t)
    secod <- findSymbol env "snd"
    fstSigs <- enumSigs env typs (toAbstractType first)
    sndSigs <- enumSigs env typs (toAbstractType secod)
    -- assertion, check they have same elements
    when (length fstSigs /= length sndSigs)
        (error "fst and snd have different number of instantiations")
    let matches = zipWith assemblePair fstSigs sndSigs
    let matches' = filter (augmentedInstance env instMap pairProj) matches
    mapM (mkNewSig pairProj) matches'
instantiateWith env typs id t = do
    instMap <- gets $ view (refineState . instanceMapping)
    let bound = env ^. boundTypeVars
    ft <- freshType bound (Monotype t)
    rawSigs <- enumSigs env typs (toAbstractType ft)
    let sigs = filter (augmentedInstance env instMap id) rawSigs
    mapM (mkNewSig id) sigs

-- | enumerate possible inputs
enumSigs :: (ConstraintEncoder enc, MonadIO m)
         => Environment
         -> [AbstractSkeleton] -- ^ all the types
         -> AbstractSkeleton
         -> PNSolver enc m [AbstractSkeleton]
enumSigs env typs typ = do
    let bound = env ^. boundTypeVars
    let argNum = length (allArgTypes typ)
    let allArgCombs = replicateM argNum typs
    applyRes <- mapM (applySemantic env typ) allArgCombs
    let resSigs = filter (not . isBot . fst) (zip applyRes allArgCombs)
    return $ map (uncurry $ foldr (FunctionT "")) resSigs

-- | create a new transition
mkNewSig :: (ConstraintEncoder enc, MonadIO m) => Id -> AbstractSkeleton -> PNSolver enc m (Id, AbstractSkeleton)
mkNewSig id ty = do
    instMap <- gets $ view (refineState . instanceMapping)
    -- function name do not need check duplicates from user defined names
    newId <- if pairProj `isPrefixOf` id then freshId [] (id ++ "_") else freshId [] "f"
    writeLog 3 "mkNewSig" $ text id <+> text "==>"  <+> text newId <+> text "::" <+> pretty ty
    -- when same arguments exist for a function, replace it
    let fc@(FunctionCode _ params rets) = encodeFunction id ty
    unless (newInstance instMap fc) (excludeUseless fc)
    modify $ over (typeChecker . nameMapping) (Map.insert newId id)
    modify $ over (statistics . instanceCounts) (HashMap.insertWith (+) id 1 )
    modify $ over (refineState . instanceMapping) (HashMap.insert (id, params) (newId, rets))
    return (newId, ty)

--------------------------------------------------------------------------------
-- Helper functions for transition reroute or delete
--------------------------------------------------------------------------------

-- | does this transition exist?
-- we consider the transition exists if we already see these argument types
newInstance :: InstanceMap -> FunctionCode -> Bool
newInstance instMap (FunctionCode id params _) =
    not (HashMap.member (id, params) instMap)

-- | does this transition need to be rerouted?
-- if a transition has the same argument types but different return types
-- (different means the return type is refined)
-- the transition has to be rerouted
rerouteInstance :: Environment -> InstanceMap -> FunctionCode -> Bool
rerouteInstance env instMap (FunctionCode id params rets) =
    oldRets /= rets && all (uncurry (isSubtypeOf env)) (zip rets oldRets)
    where
        (_, oldRets) = HashMap.lookupDefault (error "cannot find in instMap") (id, params) instMap

-- | should this transition be added? 
augmentedInstance :: Environment -> InstanceMap -> Id -> AbstractSkeleton -> Bool
augmentedInstance env instMap name t =
    newInstance instMap fc || rerouteInstance env instMap fc
    where
        fc = encodeFunction name t

-- | remove the old transition when it is rerouted
excludeUseless :: (ConstraintEncoder enc, MonadIO m) => FunctionCode -> PNSolver enc m ()
excludeUseless (FunctionCode id params rets) = do
    instMap <- gets $ view (refineState . instanceMapping)
    let (tid, ty') = HashMap.lookupDefault (error "cannot find in instMap") (id, params) instMap
    writeLog 3 "excludeUseless" $ text "replace" <+> pretty tid
                               <+> text "with" <+> pretty id
                               <+> text "where the new return type is"
                               <+> pretty ty'
    modify $ over (refineState . toRemove) (tid :)

--------------------------------------------------------------------------------
-- signature group operations
--------------------------------------------------------------------------------

-- | group signatures by their transition representation (FunctionCode)
groupSignatures :: (ConstraintEncoder enc, MonadIO m)
                => Map Id FunctionCode -- ^ signatures
                -> PNSolver enc m (Map FunctionCode GroupId, Map GroupId (Set Id)) -- ^ group name of signatures and group members
groupSignatures sigs = do
    let sigsByType = Map.map Set.fromList $ groupByMap sigs
    writeLog 3 "groupSignatures" $ pretty sigsByType
    let sigLists = Map.toList sigsByType
    signatureGroups <- flip zip sigLists <$> mapM (\_ -> freshId [] "gm") [() | _ <- sigLists]
    let dupes = [Set.size $ snd $ snd x | x <- signatureGroups, Set.size (snd $ snd x) > 1]
    let allIds = [Set.size $ snd $ snd x | x <- signatureGroups]
    writeLog 3 "groupSignatures" $ text $ printf "%d class; %d equiv; %d total"
        (length sigLists) (sum dupes) (Map.size sigs)
    let groupMap = Map.fromList $ map (\(gid, (_, ids)) -> (gid, ids)) signatureGroups
    let t2g = Map.fromList $ map (\(gid, (aty, _)) -> (aty, gid)) signatureGroups
    -- write out the info.
    mesgChan <- gets $ view messageChan
    modify $ over (statistics . solverStats . duplicateSymbols) (++ [(length sigLists, sum dupes, sum $ map length $ sigLists)])
    return (t2g, groupMap)

mkGroups :: (ConstraintEncoder enc, MonadIO m)
         => Environment
         -> Map Id AbstractSkeleton
         -> PNSolver enc m (Map Id AbstractSkeleton)
mkGroups env sigs = do
    let encodedSigs = Map.mapWithKey encodeFunction sigs
    (t2g, sigGroups) <- groupSignatures encodedSigs
    -- Do I want to take the sigs here?
    nameMap <- gets $ view (typeChecker . nameMapping)
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let hoAlias = Map.keysSet $ Map.filter (`elem` hoArgs) nameMap
    representatives <- mapM (selectOurRep hoAlias) sigGroups
    let sigs' = Map.restrictKeys sigs (Set.fromList $ Map.elems representatives)
    modify $ over (groupState . groupRepresentative) $ Map.union representatives
    modify $ over (groupState . typeToGroup) (Map.union t2g)
    mapM_ updateGroups (Map.toList sigGroups)
    return sigs'
  where
    selectOurRep = selectRepresentative
    -- Given the new set of groups, update the current group mapping
    -- updateGroups :: Map Id (Set Id) -> Map Id (Set Id) -> Map Id (Set Id)
    -- updateGroups sigGroups gm = foldr updategm gm (Map.toList sigGroups)
    -- updategm (groupName, groupMembers) = Map.insertWith Set.union groupName groupMembers
    updateGroups (gid, fids) = do
        modify $ over (groupState . groupMap) (Map.insertWith Set.union gid fids)
        mapM_ (\fid -> modify $ over (groupState . nameToGroup) (Map.insert fid gid)) fids

selectRepresentative :: (ConstraintEncoder enc, MonadIO m)
                     => Set Id
                     -> Set Id
                     -> PNSolver enc m Id
selectRepresentative hoArgs s = do
    let setToPickFrom = s
    strat <- getExperiment coalesceStrategy
    case strat of
        First -> return $ Set.elemAt 0 setToPickFrom
        LeastInstantiated -> pickReprOrder sortOn setToPickFrom
        MostInstantiated -> pickReprOrder sortDesc setToPickFrom
    where
        pickReprOrder sorting setToPickFrom = do
            nm <- gets $ view (typeChecker . nameMapping)
            instCounts <- gets $ view (statistics . instanceCounts)
            let idToCount id = instCounts HashMap.! (nm Map.! id)
            let countMapping = sorting snd $ map (\x -> (x, idToCount x)) $ Set.toList setToPickFrom
            -- writeLog 3 "SelectRepresentative" $ text gid <+> "needs to pick: " <+> pretty countMapping
            return $ fst $ head countMapping

        sortDesc f = sortBy (on (flip compare) f)

changeGroups :: (ConstraintEncoder enc, MonadIO m)
             => Environment
             -> Bool
             -> PNSolver enc m ([(Id, AbstractSkeleton)], [Id])
changeGroups env True = do
    removables' <- gets $ view (refineState . toRemove)
    writeLog 3 "changeGroups" $ pretty removables'
    splitGroups env removables'
changeGroups _ False = do
    removables <- gets $ view (refineState . toRemove)
    return ([], removables)

splitGroups :: (ConstraintEncoder enc, MonadIO m)
            => Environment
            ->[Id]
            -> PNSolver enc m ([(Id, AbstractSkeleton)], [Id])
splitGroups env removables = do
    -- Step 1: modify groupmap to exclude all those in removables
    -- Some groups may no longer exist as a result of this operation.
    -- Some groups representative may not longer be valid.
    -- TODO: This operation could be slow. find a way to go from id -> groupId (or id -> abstrTy)
    gm <- gets $ view (groupState . groupMap)
    let gm' = Map.map fromJust
            $ Map.filter isJust
            $ Map.map (shrinkSet (Set.fromList removables)) gm
    modify $ set (groupState . groupMap) gm'
    grs <- gets $ view (groupState . groupRepresentative)
    -- Step 2: fixup the group representatives, while also deciding what we can safely remove,
    -- and what we need to add (new representatives).
    (toAdd, toRemove, grs') <- foldM (updateRepresentatives env) ([], [], Map.empty) (Map.toList grs)
    modify $ set (groupState . groupRepresentative) grs'
    mapM_ (\t -> modify $ \st ->
        st { _encoder = modifyTy2tr (HashMap.map $ Set.delete t) (_encoder st) }
        ) removables
    let removeSet = Set.fromList removables
    modify $ over (typeChecker . nameMapping) (`Map.withoutKeys` removeSet)
    modify $ over (searchState . currentSigs) (`Map.withoutKeys` removeSet)
    modify $ over (groupState . nameToGroup) (`Map.withoutKeys` removeSet)
    return (toAdd, toRemove)

updateRepresentatives :: (ConstraintEncoder enc, MonadIO m)
                      => Environment
                      -> ([(Id, AbstractSkeleton)], [Id], Map GroupId Id)
                      -> (GroupId, Id)
                      -> PNSolver enc m ([(Id, AbstractSkeleton)], [Id], Map GroupId Id)
updateRepresentatives env (addables, removables, newReps) (gid, rep)= do
    gm <- gets $ view (groupState . groupMap)
    sigs <- gets $ view (searchState . currentSigs)
    nameMap <- gets $ view (typeChecker . nameMapping)
    let mbCurrentGroup = Map.lookup gid gm
    let removables' = rep:removables
    case mbCurrentGroup of
        Nothing -> do
            -- The group was eliminated entirely by the removables
            writeLog 3 "updateRepresentative" $ text "remove rep" <+> text rep
            return (addables, rep:removables, newReps)
        Just currentGroup
            | rep `Set.notMember` currentGroup && not (null currentGroup) -> do
                -- We need to elect a new leader for the group!
                let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
                let hoAlias = Map.keysSet $ Map.filter (`elem` hoArgs) nameMap
                newRep <- selectRepresentative hoAlias currentGroup
                let abstractType = lookupWithError "currentSigs" rep sigs
                let addables' = (newRep, abstractType):addables
                return (addables', removables', Map.insert gid newRep newReps)
            | rep `Set.notMember` currentGroup -> do
                writeLog 3 "updateRepresentative" $ text "delete rep" <+> text rep
                -- There was only that one element in the group, so with the leader gone, the group goes away.
                return (addables, removables', newReps)
            | otherwise -> do -- after all the removals, the current representative is still in its original group
                writeLog 3 "updateRepresentative" $ text "keep rep" <+> text rep
                return (addables, removables, Map.insert gid rep newReps)

shrinkSet :: Set Id -> Set Id -> Maybe (Set Id)
shrinkSet toRemove ids = let
    ids' = Set.difference ids toRemove
    in if Set.null ids' then Nothing else Just ids'

getGroupRep :: (ConstraintEncoder enc, MonadIO m)
            => Id
            -> PNSolver enc m [Id]
getGroupRep name = do
    gr <- gets $ view (groupState . groupRepresentative)
    ngm <- gets $ view (groupState . nameToGroup)
    let argGps = maybeToList $ Map.lookup name ngm
    writeLog 3 "getGroupRep" $ text name <+> text "is contained in group" <+> pretty argGps
    let argRp = mapMaybe (`Map.lookup` gr) argGps
    writeLog 3 "getGroupRep" $ pretty argGps <+> text "has representative" <+> pretty argRp
    if null argRp then error ("cannot find group rep for " ++ name) else return argRp

--------------------------------------------------------------------------------
-- Miscellaneous helper functions
--------------------------------------------------------------------------------

addCloneFunction :: (ConstraintEncoder enc, MonadIO m) => AbstractSkeleton -> PNSolver enc m Id
addCloneFunction ty = do
    modify $ over (searchState . functionMap) (HashMap.insert fname fc)
    updateTy2Tr fname ty
    return fname
    where
        fname = show ty ++ "|clone"
        fc = FunctionCode fname [ty] [ty, ty]

addEncodedFunction :: (ConstraintEncoder enc, MonadIO m)
                   => (Id, AbstractSkeleton)
                   -> PNSolver enc m ()
addEncodedFunction (id, f) = do
    let ef = encodeFunction id f
    modify $ over (searchState . functionMap) (HashMap.insert id ef)
    modify $ over (searchState . currentSigs) (Map.insert id f)
    -- store the used abstract types and their groups into mapping
    updateTy2Tr id f

addMusters :: (ConstraintEncoder enc, MonadIO m)
           => Id
           -> PNSolver enc m ()
addMusters arg = do
    nameMap <- gets $ view (typeChecker . nameMapping)
    let eqArg n = n == arg || n == arg ++ hoPostfix
    let argFuncs = Map.keys $ Map.filter eqArg nameMap
    argRps <- mapM getGroupRep argFuncs
    modify $ \st ->
        st { _encoder = modifyMusters (HashMap.insert arg $ concat argRps) (_encoder st) }

doRefine :: RefineStrategy -> Bool
doRefine NoGar = False
doRefine NoGar0 = False
doRefine SypetClone = False
doRefine _ = True

updateTy2Tr :: (ConstraintEncoder enc, MonadIO m)
            => Id
            -> AbstractSkeleton
            -> PNSolver enc m ()
updateTy2Tr id f =
    mapM_ (\t -> 
        modify $ \st -> 
            st { _encoder = modifyTy2tr (addTransition t id) (_encoder st) }) includedTyps
    where
        includedTyps = nub (allArgTypes f)
        addTransition k tid = HashMap.insertWith Set.union k (Set.singleton tid)