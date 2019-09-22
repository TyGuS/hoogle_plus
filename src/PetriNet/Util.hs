{-# LANGUAGE FlexibleContexts #-}

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
import Database.Util

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

getExperiment exp = gets $ view (_1 . searchParams . exp)
getSolver field = gets $ view (_1 . field)
setSolver field = modify . set (_1 . field)
overSolver field = modify . over (_1 . field)

-------------------------------------------------------------------------------
-- | helper functions
-------------------------------------------------------------------------------
writeLog :: MonadIO m => Int -> String -> Doc -> PNSolver m ()
writeLog level tag msg = do
    mesgChan <- getSolver messageChan
    liftIO $ writeChan mesgChan (MesgLog level tag $ show $ plain msg)
    -- when (level <= 1) (trace (printf "[%s]: %s\n" tag (show $ plain msg)) $ return ())

multiPermutation len elmts | len == 0 = [[]]
multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
multiPermutation len elmts            = nubSpence [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]

-- Thanks, this only helps when you get >100 elements, otherwise, use nubOrd:
-- https://github.com/AndreasPK/nubBench/blob/038bc644f32aaa47035484b4384a4aaf5b78320c/app/Main.hs
nubSpence :: (Hashable a, Eq a) => [a] -> [a]
nubSpence l =
    runST $ do
        arr <- mr
        forM_ l $ \j -> do
            let index = hash j `mod` 255
            current <- readArray arr index
            let new =
                    if j `elem` current
                        then current
                        else j : current
            writeArray arr index new
        join <$> getElems arr
  where
    mr :: ST s (STArray s Int [a])
    mr = newListArray (0, 255) (replicate 256 [])

listDiff left right = Set.toList $ Set.fromList left `Set.difference` Set.fromList right

replaceId a b = Text.unpack . Text.replace (Text.pack a) (Text.pack b) . Text.pack

var2any env t@(ScalarT (TypeVarT _ id) _) | isBound env id = t
var2any env t@(ScalarT (TypeVarT _ id) _) = AnyT
var2any env (ScalarT (DatatypeT id args l) r) = ScalarT (DatatypeT id (map (var2any env) args) l) r
var2any env (FunctionT x tArg tRet) = FunctionT x (var2any env tArg) (var2any env tRet)

freshId :: (Freshable s, MonadIO m) => Id -> StateT s m Id
freshId prefix = do
    s <- get
    let indices = getNameIndices s
    let idx = Map.findWithDefault 0 prefix indices
    put $ setNameIndices (Map.insert prefix (idx+1) indices) s
    return $ prefix ++ show idx

-- | Replace all bound type variables with fresh free variables
freshType :: (Freshable s, MonadIO m) => RSchema -> StateT s m RType
freshType = freshType' Map.empty
  where
    freshType' subst (ForallT a sch) = do
        a' <- freshId "A"
        freshType' (Map.insert a (vart a' ftrue) subst) sch
    freshType' subst (Monotype t) = return (typeSubstitute subst t)

freshAbstract :: MonadIO m => [Id] -> AbstractSkeleton -> PNSolver m AbstractSkeleton
freshAbstract bound t = do
    (_, t') <- freshAbstract' bound Map.empty t
    return t'
  where
    freshAbstract' bound m t@(AScalar (ATypeVarT id)) | id `elem` bound = return (m, t)
    freshAbstract' bound m (AScalar (ATypeVarT id)) | id `Map.member` m =
        return (m, fromJust (Map.lookup id m))
    freshAbstract' bound m (AScalar (ATypeVarT id)) = do
        v <- lift $ freshId "A"
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

groupSignatures :: MonadIO m => Map Id FunctionCode -> PNSolver m (Map GroupId FunctionCode, Map GroupId (Set Id))
groupSignatures sigs = do
    let sortArgs (FunctionCode f ho args res) = FunctionCode f ho (sort args) res
    let sigsByType = Map.map Set.fromList $ groupByMap $ Map.map sortArgs sigs
    writeLog 2 "groupSignatures" $ pretty sigsByType
    let sigLists = Map.toList sigsByType
    signatureGroups <- flip zip sigLists <$> mapM (\_ -> lift $ freshId "gm") [() | _ <- sigLists]
    let dupes = [Set.size $ snd $ snd x | x <- signatureGroups, Set.size (snd $ snd x) > 1]
    let allIds = [Set.size $ snd $ snd x | x <- signatureGroups]
    writeLog 3 "groupSignatures" $
        text $ printf "%d class; %d equiv; %d total" (length sigLists) (sum dupes) (Map.size sigs)
    let groupMap = Map.fromList $ map (\(gid, (_, ids)) -> (gid, ids)) signatureGroups
    let groupSig = Map.fromList $ map (\(gid, (aty, _)) -> (gid, aty {funName = gid})) signatureGroups
    -- write out the info.
    mesgChan <- getSolver messageChan
    overSolver solverStats (\s -> s {
            duplicateSymbols = duplicateSymbols s ++ [(length sigLists, sum dupes, sum $ map length sigLists)]
        })
    stats <- getSolver solverStats
    return (groupSig, groupMap)

recoverNames :: Map Id Id -> Program t -> Program t
recoverNames mapping (Program (PSymbol sym) t) =
    case Map.lookup sym mapping of
      Nothing -> Program (PSymbol (replaceId hoPostfix "" $ removeLast '_' sym)) t
      Just name -> Program (PSymbol (replaceId hoPostfix "" $ removeLast '_' name)) t
recoverNames mapping (Program (PApp fun pArg) t) = Program (PApp fun' pArg') t
  where
    fun' = case Map.lookup fun mapping of
                Nothing -> replaceId hoPostfix "" $ removeLast '_' fun
                Just name -> replaceId hoPostfix "" $ removeLast '_' name
    pArg' = map (recoverNames mapping) pArg
recoverNames mapping (Program (PFun x body) t) = Program (PFun x body') t
  where
    body' = recoverNames mapping body
    
listToMap :: Ord a => [a] -> [(a, Int)]
listToMap lst = map (\l -> (head l, length l)) (group (sort lst))

setIth :: [a] -> Int -> a -> [a]
setIth xs n x = reverse $ snd $ foldl fold_fun base xs
    where
        base = (0, [])
        fold_fun (i, acc) elmt = (i + 1, (if i == n then x else elmt) : acc)

setFromLists :: [a] -> [(Int, a)] -> [a]
setFromLists = foldl (uncurry . setIth)

addList :: [Int] -> [Int] -> [Int]
addList xs ys = zipWith (+) xs' ys'
    where
        xlen = length xs
        ylen = length ys
        xs' = if xlen < ylen then xs ++ replicate (ylen - xlen) 0 else xs
        ys' = if xlen < ylen then ys else ys ++ replicate (xlen - ylen) 0