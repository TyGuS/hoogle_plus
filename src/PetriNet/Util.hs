{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module PetriNet.Util where

import Types.Type
import Types.Common
import Types.Solver
import Types.Abstract
import Types.Experiments
import Types.Program
import Types.Encoder
import Synquid.Program
import Synquid.Logic hiding (varName)
import Synquid.Type
import Synquid.Pretty
import Synquid.Util

import Data.Maybe
import qualified Data.Text as Text
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Lens
import Control.Monad.State
import Control.Monad.Extra
import Data.List.Extra
import Text.Pretty.Simple
import Text.Printf
import Control.Concurrent.Chan
import Data.Hashable
import Control.Monad.ST (runST, ST)
import Data.Array.ST (STArray, readArray, writeArray, newListArray, getElems)
import Debug.Trace

getExperiment exp = gets $ view (searchParams . exp)

-------------------------------------------------------------------------------
-- | helper functions
-------------------------------------------------------------------------------
writeLog :: MonadIO m => Int -> String -> Doc -> PNSolver m ()
writeLog level tag msg = do
    mesgChan <- gets $ view messageChan
    liftIO $ writeChan mesgChan (MesgLog level tag $ show $ plain msg)
    -- if level <= 3 then trace (printf "[%s]: %s\n" tag (show $ plain msg)) $ return () else return ()

multiPermutation len elmts | len == 0 = [[]]
multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
multiPermutation len elmts            = nubSpence [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]

-- Thanks, this only helps when you get >100 elements, otherwise, use nubOrd:
-- https://github.com/AndreasPK/nubBench/blob/038bc644f32aaa47035484b4384a4aaf5b78320c/app/Main.hs
nubSpence :: (Hashable a, Eq a) => [a] -> [a]
nubSpence l = runST $ do
  arr <- mr
  forM_ l $ \j -> do
    let index = (hash j) `mod` 255
    current <- readArray arr index
    let new = if j `elem` current then current else j : current
    writeArray arr index new
  join <$> getElems arr
    where
      mr :: ST s (STArray s Int [a])
      mr = newListArray (0, 255) (replicate 256 [])

listDiff left right = Set.toList $ (Set.fromList left) `Set.difference` (Set.fromList right)

replaceId a b = Text.unpack . Text.replace (Text.pack a) (Text.pack b) . Text.pack

var2any env t@(ScalarT (TypeVarT _ id) _) | isBound env id = t
var2any env t@(ScalarT (TypeVarT _ id) _) = AnyT
var2any env (ScalarT (DatatypeT id args l) r) = ScalarT (DatatypeT id (map (var2any env) args) l) r
var2any env (FunctionT x tArg tRet) = FunctionT x (var2any env tArg) (var2any env tRet)

freshId :: MonadIO m => Id -> PNSolver m Id
freshId prefix = do
    indices <- gets (^. nameCounter)
    let idx = Map.findWithDefault 0 prefix indices
    modify (over nameCounter $ Map.insert prefix (idx+1))
    return $ prefix ++ show idx

-- | Replace all bound type variables with fresh free variables
freshType :: MonadIO m => RSchema -> PNSolver m RType
freshType = freshType' Map.empty []
  where
    freshType' subst constraints (ForallT a sch) = do
        a' <- freshId "A"
        freshType' (Map.insert a (vart a' ftrue) subst) (a':constraints) sch
    freshType' subst constraints (Monotype t) = return (typeSubstitute subst t)

freshAbstract :: MonadIO m => [Id] -> AbstractSkeleton -> PNSolver m AbstractSkeleton
freshAbstract bound t = do
    (_, t') <- freshAbstract' bound Map.empty t
    return t'
  where
    freshAbstract' bound m t@(AScalar (ATypeVarT id)) | id `elem` bound = return (m, t)
    freshAbstract' bound m (AScalar (ATypeVarT id)) | id `Map.member` m =
        return (m, fromJust (Map.lookup id m))
    freshAbstract' bound m (AScalar (ATypeVarT id)) = do
        v <- freshId "A"
        let t = AScalar (ATypeVarT v)
        return (Map.insert id t m, AScalar (ATypeVarT v))
    freshAbstract' bound m (AScalar (ADatatypeT id args)) = do
        (m', args') <- foldM (\(accm, acct) t -> do
            (m', t') <- freshAbstract' bound accm t
            return (m', acct++[t'])) (m,[]) args
        return (m', AScalar (ADatatypeT id args'))
    freshAbstract' bound m (AFunctionT tArg tRes) = do
        (m', tArg') <- freshAbstract' bound m tArg
        (m'', tRes') <- freshAbstract' bound m' tRes
        return (m'', AFunctionT tArg' tRes')

mkConstraint :: MonadIO m => [Id] -> Id -> AbstractSkeleton -> PNSolver m UnifConstraint
mkConstraint bound v t = do
    t' <- freshAbstract bound t
    return (AScalar (ATypeVarT v), t')

groupSignatures :: MonadIO m => Map Id FunctionCode -> PNSolver m (Map FunctionCode GroupId, Map GroupId (Set Id))
groupSignatures sigs = do
    let sigsByType = Map.map Set.fromList $ groupByMap sigs
    writeLog 3 "groupSignatures" $ pretty sigsByType
    let sigLists = Map.toList sigsByType
    signatureGroups <- flip zip sigLists <$> mapM (\_ -> freshId "gm") [() | _ <- sigLists]
    let dupes = [Set.size $ snd $ snd x | x <- signatureGroups, Set.size (snd $ snd x) > 1]
    let allIds = [Set.size $ snd $ snd x | x <- signatureGroups]
    writeLog 3 "groupSignatures" $ text $ printf "%d class; %d equiv; %d total"
        (length sigLists) (sum dupes) (Map.size sigs)
    let groupMap = Map.fromList $ map (\(gid, (_, ids)) -> (gid, ids)) signatureGroups
    let t2g = Map.fromList $ map (\(gid, (aty, _)) -> (aty, gid)) signatureGroups
    -- write out the info.
    mesgChan <- view messageChan <$> get
    modify $ over solverStats (\s -> s {
        duplicateSymbols = duplicateSymbols s ++ [(length sigLists, sum dupes, sum $ map length $ sigLists)]
    })
    stats <- view solverStats <$> get
    -- liftIO $ writeChan mesgChan (MesgS stats)
    return (t2g, groupMap)
