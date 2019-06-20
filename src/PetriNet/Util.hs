{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module PetriNet.Util where

import Types.Type
import Types.Common
import Types.Solver
import Types.Abstract
import Types.Experiments
import Types.Program
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

getExperiment exp = gets $ view (searchParams . exp)

-------------------------------------------------------------------------------
-- | helper functions
-------------------------------------------------------------------------------
writeLog :: MonadIO m => Int -> String -> Doc -> PNSolver m ()
writeLog level tag msg = do
    mesgChan <- gets $ view messageChan
    liftIO $ writeChan mesgChan (MesgLog level tag $ show $ plain msg)

multiPermutation len elmts | len == 0 = [[]]
multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
multiPermutation len elmts            = nubOrd [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]


listDiff left right = Set.toList $ (Set.fromList left) `Set.difference` (Set.fromList right)

replaceId a b = Text.unpack . Text.replace (Text.pack a) (Text.pack b) . Text.pack
mkPairMatch (FunctionCode name _ params ret) = FunctionCode (replaceId "Pair" "Pair_match" name) [] ret params

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

fullStats locationName = undefined {- do
    sigs <- gets $ view currentSigs
    fm <- view functionMap <$> get
    namemapping <- view nameMapping <$> get
    t2t <- view type2transition <$> get
    writeLog 3 locationName $ text "current sigs:" <+> pretty (Map.toList sigs)
    writeLog 3 locationName $ text "current fm:" <+> pretty (HashMap.toList $ HashMap.map funName fm)
    writeLog 3 locationName $ text "current nameMapping:" <+> pretty (Map.toList namemapping)
    writeLog 3 locationName $ text "current t2t:" <+> pretty (Map.toList t2t) -}

partitionDuplicateFunctions :: MonadIO m => [FunctionCode] -> PNSolver m ([FunctionCode],[FunctionCode])
partitionDuplicateFunctions symbols = undefined {-do
    let groupedSymbols = groupSortBy compare symbols
    let deduplicatedSymbols = map head $ groupedSymbols
    let toBeRemoved = (concatMap tail $ groupedSymbols)
    let newGroupMap = map (\(x:xs) -> (x, x:xs)) (map (map funName) groupedSymbols)
    modify $ set groupMap (Map.fromList newGroupMap)
    return (deduplicatedSymbols, toBeRemoved)-}

groupSignatures :: MonadIO m => Map Id AbstractSkeleton -> PNSolver m (Map Id (Set Id))
groupSignatures sigs = do
    let sigsByType = groupByMap sigs
    writeLog 3 "groupSignatures" $ pretty $ Map.toList sigsByType
    let sigLists = Map.elems sigsByType
    let signatureGroups = map (\xs -> (head xs, Set.fromList $ tail xs)) $ sigLists
    let dupes = [Set.size $ snd x | x <- signatureGroups, Set.size (snd x) > 0]
    let allIds = [Set.size $ snd x | x <- signatureGroups]
    writeLog 3 "groupSignatures" $ text $ printf "%d class; %d equiv; %d total"
        (length sigLists) (sum dupes) (sum $ map length $ sigLists)
    return $ Map.fromList signatureGroups

countDuplicateSigs :: Map Id AbstractSkeleton -> String
countDuplicateSigs sigs = undefined {-let
    reversedSigs = groupByMap sigs
    countList = Map.toList reversedSigs
    dupes = [length (snd x) | x <- countList, length (snd x) > 1]
    in
        printf "%d classes; %d equiv; %d total" (length dupes) (sum dupes) (length (Map.toList sigs))
        -}

countDuplicates symbols = undefined {-do
    mesgChan <- view messageChan <$> get
    let groupedSymbols = groupBySlow areEqFuncs symbols
    let counts = [ (length ss, head ss) | ss <- groupedSymbols]
    let dupes = [ (fst x) | x <- counts, (fst x) > 1]
    modify $ over solverStats (\s -> s {
        duplicateSymbols = duplicateSymbols s ++ [(length dupes, sum dupes, length symbols)]
    })
    -- when (length dupes > 0) (writeLog 2 "countDuplicates" $ pretty groupedSymbols)
    stats <- view solverStats <$> get
    liftIO $ writeChan mesgChan (MesgS stats)
    return $ printf "%d classes; %d equiv; %d total" (length dupes) (sum dupes) (length symbols)-}
