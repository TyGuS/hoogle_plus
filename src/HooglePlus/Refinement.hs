{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.Refinement where

import Database.Convert
import Database.Utils
import Encoder.ConstraintEncoder
import PetriNet.AbstractType
import PetriNet.Transition
import PetriNet.Utils
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Utils
import Types.Common
import Types.Environment
import Types.Program
import Types.Solver
import Types.Type
import Types.TypeChecker
import Types.CheckMonad
import Types.Experiments

import Control.Lens
import Control.Monad.Fail
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Text.Printf

-- | add a new type into our cover and ensure all of them have proper lower bound
updateCover :: Environment -> AbstractSkeleton -> AbstractCover -> AbstractCover
updateCover env t cover = let (_, cover') = updateCover' env cover [] t rootNode in cover'

updateCover' :: Environment
             -> AbstractCover -- ^ current abstraction cover
             -> [AbstractSkeleton] -- ^ intersection types to be added
             -> AbstractSkeleton --
             -> AbstractSkeleton
             -> ([AbstractSkeleton], AbstractCover)
updateCover' env cover intscts t paren | equalAbstract env t paren = (intscts, cover)
updateCover' env cover intscts t paren | isSubtypeOf env t paren =
    let children = HashMap.lookupDefault Set.empty paren cover
        child_fun c (ints, acc) = updateCover' env acc ints t c
        (scts, updatedCover) = Set.foldr child_fun (intscts, cover) children
        lower c = isSubtypeOf env t c || isSubtypeOf env c t
        inSubtree = any lower (Set.toList children)
        baseCover = if inSubtree
                      then updatedCover
                      else HashMap.insertWith Set.union paren (Set.singleton t) updatedCover
        int_fun s (ints, acc) = updateCover' env acc ints s rootNode
     in foldr int_fun ([], baseCover) scts
updateCover' env cover intscts t paren | isSubtypeOf env paren t =
    let parents = HashMap.keys $ HashMap.filter (Set.member paren) cover
        rmParen = HashMap.map (Set.delete paren) cover
        addCurr p = HashMap.insertWith Set.union p $ Set.singleton t
        addedCurr = foldr addCurr rmParen parents
        cover' = HashMap.insertWith Set.union t (Set.singleton paren) addedCurr
     in (intscts, cover')
updateCover' env cover intscts t paren =
    let intsctMb = abstractIntersect env t paren
     in if isJust intsctMb then (fromJust intsctMb : intscts, cover)
                           else (intscts, cover)

-- | refine the current abstraction
-- do the bidirectional type checking first, compare the two programs we get,
-- with the type split information update the abstraction tree
refineSemantic :: (ConstraintEncoder enc, MonadIO m, MonadFail m)
               => Environment 
               -> TProgram 
               -> AbstractSkeleton 
               -> PNSolver enc m SplitInfo
refineSemantic env prog at = do
    cover <- gets $ view (refineState . abstractionCover)
    writeLog 2 "refineSemantic" $ text "Current abstract types:" <+> text (show cover)

    -- back propagation of the error types to get all split information
    propagate env prog $ toAbstractType at
    
    -- get the split pairs
    splits <- gets $ view (refineState . splitTypes)
    let sortedSplits = sortBy (flip (compareAbstract env)) (Set.toList splits)
    writeLog 3 "refineSemantic splitTypes" $ pretty sortedSplits

    -- get removed transitions
    addsAndRemoves <- mapM (splitTransitions env) sortedSplits
    let (adds, removes) = unzip addsAndRemoves
    let addedSigs = map fst (concat adds)
    let removedSigs = concat removes
    let toAdd = addedSigs \\ removedSigs
    let removables = removedSigs \\ addedSigs
    writeLog 3 "refineSemantic (toAdd, removables)" $ pretty (toAdd, removables)

    -- add clone functions and add them into new transition set
    noClone <- getExperiment disableCopy
    cloneNames <- if noClone then return []
                             else mapM addCloneFunction $ Set.toList splits

    -- update the higer order query arguments
    let hoArgs = filter (isFunctionType . toMonotype . snd) (env ^. arguments)
    mapM_ addMusters (map fst hoArgs)

    -- call the refined encoder on these signatures and disabled signatures
    return SplitInfo { newPlaces = Set.toList splits
                     , removedTrans = removables
                     , newTrans = addedSigs ++ cloneNames
                     }

-- | split all the transitions when a new abstract type is added
splitTransitions :: (ConstraintEncoder enc, MonadIO m) 
                 => Environment
                 -> AbstractSkeleton -- ^ new abstract type to be added
                 -> PNSolver enc m ([(Id, AbstractSkeleton)], [Id])
splitTransitions env at = do
    modify $ set (refineState . toRemove) []

    cover <- gets $ view (refineState . abstractionCover)
    t2tr <- getTy2tr <$> (gets $ view encoder)
    gm <- gets $ view (groupState . groupMap)
    gr <- gets $ view (groupState . groupRepresentative)
    let parents = HashMap.keys $ HashMap.filter (Set.member at) cover
    let pids = Set.unions $ map (\p -> HashMap.lookupDefault Set.empty p t2tr) parents
    let gids = Map.keys $ Map.filter (`Set.member` pids) gr
    let fids = concatMap (\gid -> Set.toList $ Map.findWithDefault Set.empty gid gm) gids
    sigs <- mapM (splitTransition env at) fids
    sigs' <- mkGroups env (Map.fromList (concat sigs))
    Map.traverseWithKey (curry addEncodedFunction) sigs'

    -- update the group information by the current toRemove
    toCoalesce <- getExperiment coalesceTypes
    (toAdd, removables) <- changeGroups env toCoalesce
    writeLog 3 "splitTransitions (toAdd, removables)" $ pretty (toAdd, removables)
    mapM_ addEncodedFunction toAdd

    let adds = Map.toList sigs' ++ toAdd
    modify $ over (searchState . activeSigs) (Set.union $ Set.fromList $ map fst adds)
    modify $ over (searchState . activeSigs) (`Set.difference` Set.fromList removables)
    modify $ over (typeChecker . nameMapping) (`Map.withoutKeys` Set.fromList removables)
    modify $ \st ->
        st { _encoder = modifyMusters (HashMap.map (filter (`notElem` removables))) (_encoder st) }
    return (adds, removables)

splitTransition :: (ConstraintEncoder enc, MonadIO m)
                => Environment
                -> AbstractSkeleton
                -> Id -- ^ name of the transition to be splitted
                -> PNSolver enc m [(Id, AbstractSkeleton)]
splitTransition env newAt fid = do
    rep <- head <$> getGroupRep fid
    sigs <- gets $ view (searchState . currentSigs)
    let ty = lookupWithError "currentSigs" rep sigs
    writeLog 3 "splitTransition" $ text "split transtion" <+> text fid <+> text "::" <+> pretty ty
    allSubsts (encodeFunction fid ty)
  where
    allSubsts (FunctionCode _ params rets) = allSubsts' params rets

    allSubsts' args rets = do
        cover <- gets $ view (refineState . abstractionCover)
        nameMap <- gets $ view (typeChecker . nameMapping)
        instMap <- gets $ view (refineState . instanceMapping)
        let parents = HashMap.keys $ HashMap.filter (Set.member newAt) cover
        let args' = enumArgs parents args
        let fids = if pairProj `isPrefixOf` fid then ["fst", "snd"] else [fid]
        newRets <- mapM (\f -> do
            funType <- findSymbol env f
            let absFunType = toAbstractType funType
            mapM (applySemantic env absFunType) args') fids
        writeLog 3 "allSubsts'" $ text fid <+> text "returns" <+> pretty newRets
        let sameReturns rs = all (uncurry (equalAbstract env)) (zip rets rs)
        let validFunc (a, rs) = not (sameReturns rs && args == a) && not (any isBot rs)
        let filteredFuncs = filter validFunc (zip args' (transpose newRets))
        let sigs = map (\(a, rs) -> foldr (FunctionT "") (last rs) (a ++ init rs)) filteredFuncs
        let funId = if pairProj `isPrefixOf` fid then pairProj
                                                 else lookupWithError "nameMapping" fid nameMap
        let sigs' = filter (augmentedInstance env instMap funId) sigs
        mapM (mkNewSig funId) sigs'

    enumArgs parents [] = [[]]
    enumArgs parents (arg:args)
        | arg `elem` parents = let args' = enumArgs parents args
                                in [a:as | a <- [arg, newAt], as <- args']
        | otherwise = map (arg:) (enumArgs parents args)

-- | back propagate the abstract type from root to leaves
-- by lattice search over the current abstraction cover
propagate :: (ConstraintEncoder enc, MonadIO m, MonadFail m) 
          => Environment 
          -> TProgram 
          -> AbstractSkeleton 
          -> PNSolver enc m ()
-- | base case, when we reach the leaf of the AST
propagate env p@(Program (PSymbol sym) t) upstream = do
    writeLog 2 "propagate" $ text "propagate" <+> pretty upstream <+> text "into" <+> pretty p
    cover <- gets $ view (refineState . abstractionCover)
    unless  (existAbstract env cover upstream)
            (do let newCover = updateCover env upstream cover
                modify $ set (refineState . abstractionCover) newCover
                let newTyps = allTypesOf newCover \\ allTypesOf cover
                modify $ over (refineState . splitTypes) (Set.union $ Set.fromList newTyps))
-- | starter case, when we start from a bottom type
-- find the most general abstraction that unifies with the concrete types
-- of the arguments, but not unify with the function args of its signature
propagate env p@(Program (PApp f args) _) upstream = do
    unless (isBot upstream) (propagate env (Program (PSymbol "x") AnyT) upstream)
    writeLog 2 "propagate" $ text "propagate" <+> pretty upstream <+> text "into" <+> pretty p
    t <- findSymbol env (removeLast '_' f)
    let closedArgs = map typeOf args
    let argConcs = map toAbstractFun closedArgs
    let absFun = toAbstractType t
    abstractArgs <- observeT $ mostGeneral argConcs absFun
    mapM_ (uncurry $ propagate env) (zip args abstractArgs)
  where
    mostGeneral cArgs t = do
        cover <- gets $ view (refineState . abstractionCover)
        let bound = env ^. boundTypeVars
        -- if the argument has the concrete type ct
        -- its current abstraction is at
        -- we are finding a refined abstraction at' < at
        -- such that ct < at' < at
        currAbs <- lift $ mapM (currentAbst env cover) cArgs
        absArgs <- mapM (generalize bound) cArgs
        freshArgs <- mapM (freshType bound . toPolytype bound) absArgs
        lift $ writeLog 3 "propagate" $ text "try" <+> pretty freshArgs <+> text "from" <+> pretty cArgs <+> text "with currently" <+> pretty currAbs
        guard ((all (uncurry $ isSubtypeOf env)) (zip freshArgs currAbs))
        lift $ writeLog 3 "propagate" $ text "get generalized types" <+> pretty freshArgs <+> text "from" <+> pretty cArgs
        res <- lift $ applySemantic env t freshArgs
        lift $ writeLog 3 "propagate" $ text "apply" <+> pretty freshArgs <+> text "to" <+> pretty t <+> text "gets" <+> pretty res
        guard (isSubtypeOf env res upstream)
        return $ map toAbstractFun freshArgs
-- | case for lambda functions
propagate env (Program (PFun x body) (FunctionT _ tArg tRet)) (FunctionT _ atArg atRet) =
    propagate (addVariable x tArg env) body atRet
propagate env (Program (PFun x body) t) (FunctionT _ atArg atRet) = do
    id <- freshId (env ^. boundTypeVars) "A"
    let tArg = TypeVarT id
    propagate (addVariable x tArg env) body atRet
propagate _ prog t = return ()

-- | generalize a closed concrete type into an abstract one
generalize :: (CheckMonad (t m), MonadIO (t m), MonadIO m) 
           => [Id] 
           -> AbstractSkeleton 
           -> LogicT (t m) AbstractSkeleton
generalize bound t@(TypeVarT id)
    | id `notElem` bound = return t
    | otherwise = do
        v <- lift $ freshId bound "T"
        return (TypeVarT v) `mplus` return t
-- for datatype, we define the generalization order as follows:
-- (1) v
-- (2) datatype with fresh type variables
-- (3) datatype with incrementally generalized inner types
generalize bound (DatatypeT id) = return (DatatypeT id)
generalize bound t@(TyAppT tFun tArg) =
    (lift (freshId bound "T") >>= (return . TypeVarT)) `mplus` do
        let (dt, args) = collectArgs t
        args' <- mapM (generalize bound) args
        return (foldl' TyAppT (DatatypeT dt) args')
generalize bound (TyFunT tArg tRes) = do
    tArg' <- generalize bound tArg
    tRes' <- generalize bound tRes
    return (TyFunT tArg' tRes')
generalize bound (FunctionT x tArg tRes) = do
    tArg' <- generalize bound tArg
    tRes' <- generalize bound tRes
    return (FunctionT x tArg' tRes')
