{-# LANGUAGE FlexibleContexts #-}

module HooglePlus.BiExplorer where

import Types.Solver
import Types.Common
import Types.Environment
import Types.Type
import Types.Checker
import Types.Abstract
import Synquid.Type
import Synquid.Logic
import Synquid.Pretty
import PetriNet.Util
import HooglePlus.TypeChecker
import PetriNet.AbstractType

import Control.Monad.State
import Control.Lens
import Control.Concurrent.Chan
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.List
import Debug.Trace

selectComp :: MonadIO m => Environment -> PNSolver m ()
selectComp env = do
    fset <- getSolver (explorer . forwardSet)
    bset <- getSolver (explorer . backwardSet)
    has <- getSolver (explorer . selectedNames)
    let allSymbols = env ^. symbols
    mapM_ (pickOne fset bset) (Map.toList allSymbols)
    writeLog 1 "selectComp" (text "finished selection")
    fset' <- getSolver (explorer . forwardSet)
    bset' <- getSolver (explorer . backwardSet)
    when (fset /= fset') (overSolver (explorer . currentDepth) (+ 1))
    when (bset /= bset') (overSolver (explorer . currentDepth) (+ 1))
  where
    pickOne :: MonadIO m => Set RType -> Set RType -> (Id, RSchema) -> PNSolver m ()
    pickOne fset bset (cname, sch)
            -- writeLog 1 "pickOne" $ text cname
     = do
        nulls <- getSolver (explorer . nullaries)
        ctype <- lift $ freshType sch
        let tvs = env ^. boundTypeVars
        let args = map toFunDts (allArgTypes ctype)
        let guardedArgs = filter (isGuarded tvs) args
            -- for forward sets,
            -- if one of the guarded argument unifies with some type in the forward set
        nullTypes <- lift $ mapM freshType nulls
        let candidateTypes = Set.toList fset
        constraints <- mapM (\a -> mkConstraints tvs [a] candidateTypes) guardedArgs
        mapM_ (addInhabitant env Forward cname ctype) (concat constraints)
        let res = lastType ctype
        let rets = [res | isGuarded tvs res]
        constraints <- mkConstraints tvs rets (Set.toList bset)
        mapM_ (addInhabitant env Backward cname ctype) constraints

addInhabitant :: MonadIO m => Environment -> Direction -> Id -> RType -> Constraints -> PNSolver m ()
addInhabitant env dir cname ctype constraints = do
    names <- getSolver nameMapping
    indices <- gets getNameIndices
    let initialState = emptyCheckerState {
        checkerNameMapping = names,
        checkerNameCounter = indices }
    -- writeLog 1 "addInhabitant" $ pretty constraints
    unless (null constraints) $ do
        let solveConstraint = uncurry $ solveTypeConstraint env
        finalState <- execStateT (mapM_ solveConstraint constraints) initialState
        setSolver nameMapping (checkerNameMapping finalState)
        when (isChecked finalState) $ do
            let tass = typeAssignment finalState
            addComponent env dir cname ctype tass

addComponent :: MonadIO m => Environment -> Direction -> Id -> RType -> TypeSubstitution -> PNSolver m ()
addComponent env dir cname ctype tass = do
    let ftype = typeSubstitute tass ctype
    let boundTvs = env ^. boundTypeVars
    let freeVars = Set.toList (typeVarsOf ftype) \\ boundTvs
    symbols <- getSolver (explorer . selectedSymbols)
    names <- getSolver nameMapping
    selected <- getSolver (explorer . selectedTypes)
    let smallTypes t = typeDepth (toFunDts t) <= 2
    writeLog 1 "addComponent" $ text cname <+> text "::" <+> pretty ftype
    when (not (hasInstance boundTvs names symbols ftype cname) &&
        all smallTypes (lastType ftype : allArgTypes ftype)) $ do
            fname <- lift $ freshId "f"
            writeLog 1 "addComponent" $ text fname <+> text "::" <+> pretty ftype
            let types = map toFunDts (lastType ftype : allArgTypes ftype)
            let places = decompose (toAbstractType ftype)
            overSolver nameMapping (Map.insert fname cname)
            overSolver (explorer . selectedSymbols) (Map.insert fname ftype)
            let types' = filter (\t -> all (not . eqExceptFvs boundTvs t) selected) types
            overSolver (explorer . selectedTypes) (Set.union $ Set.fromList types')
            unless (cname `Map.member` Map.filter (not . isFunctionType) (env ^. arguments))
                   (overSolver (explorer . selectedNames) (Set.insert cname))
            updateSets boundTvs dir fname ftype

{- Helper functions -}
mkConstraints :: MonadIO m => [a] -> [b] -> [RType] -> PNSolver m [[(b, RType)]]
mkConstraints _ [] _ = return [[]]
mkConstraints tvs (arg:args) types = do
    more <- mkConstraints tvs args types
    types' <- lift $ mapM (freshType . Monotype) types
    return [c : m | c <- zip (repeat arg) types', m <- more]

isGuarded :: [Id] -> RType -> Bool
isGuarded tvs (ScalarT (TypeVarT _ id) _) | id `elem` tvs = True
isGuarded tvs (ScalarT DatatypeT {} _) = True
isGuarded tvs _ = False

eqExceptFvs :: [Id] -> TypeSkeleton r -> TypeSkeleton r -> Bool
eqExceptFvs tvs (ScalarT (TypeVarT _ id1) _) (ScalarT (TypeVarT _ id2) _) =
    id1 == id2 || ((id1 `notElem` tvs) && (id2 `notElem` tvs))
eqExceptFvs tvs (ScalarT (DatatypeT id1 args1 _) _) (ScalarT (DatatypeT id2 args2 _) _) =
    id1 == id2 && all (uncurry $ eqExceptFvs tvs) (zip args1 args2)
eqExceptFvs tvs (FunctionT _ arg1 res1) (FunctionT _ arg2 res2) =
    eqExceptFvs tvs arg1 arg2 && eqExceptFvs tvs res1 res2
eqExceptFvs _ BotT BotT = True
eqExceptFvs _ AnyT AnyT = True
eqExceptFvs _ _ _ = False

-- encode :: Map AbstractSkeleton Int -> (Id, RType) -> FunctionCode
-- encode m (id, t) = encodeFunction m id (toAbstractType $ shape t)

hasInstance :: [Id] -> Map Id Id -> Map Id RType -> RType -> Id -> Bool
hasInstance tvs nameMap typeMap t n = let
    sameSigs = Map.filter (eqExceptFvs tvs t) typeMap
    sigNames = Map.keys sameSigs
    funNames = map (fromJust . (`Map.lookup` nameMap)) sigNames
    in n `elem` funNames

updateSets :: MonadIO m => [Id] -> Direction -> Id -> RType -> PNSolver m ()
updateSets tvs Forward f t = do
    let res = lastType t
    fset <- getSolver (explorer . forwardSet)
    bset <- getSolver (explorer . backwardSet)
    when (isGuarded tvs res) $
        when (arity t > 0 && all (not . eqExceptFvs tvs res) fset)
            (overSolver (explorer . forwardSet) (Set.insert res))
    let args = Set.fromList $ filter (isGuarded tvs) $ map toFunDts $ allArgTypes t
    let args' = Set.filter (\t -> all (not . eqExceptFvs tvs t) bset) args
    overSolver (explorer . backwardSet) (Set.union args')
updateSets tvs Backward f t = do
    bset <- getSolver (explorer . backwardSet)
    let args = filter (isGuarded tvs) $ map toFunDts $ allArgTypes t
    let args' = Set.filter (\t -> all (not . eqExceptFvs tvs t) bset) (Set.fromList args)
    overSolver (explorer . backwardSet) (Set.union args')