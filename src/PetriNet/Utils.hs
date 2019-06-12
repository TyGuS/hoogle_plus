{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module PetriNet.Utils where

import Types.Common
import Types.Type
import Types.Environment
import Types.Abstract
import Types.Solver
import Types.Program
import Types.PetriNet
import Types.Experiments
import Types.Encoder
import Synquid.Util
import Synquid.Error
import Synquid.Pretty
import Synquid.Program
import Synquid.Logic
import Synquid.Type
import PetriNet.PNBuilder

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
import Control.Concurrent.Chan


shouldDedupe :: MonadIO m => PNSolver m Bool
shouldDedupe = gets $ view (searchParams. shouldRemoveDuplicates)

writeLog :: MonadIO m => Int -> String -> Doc -> PNSolver m ()
writeLog level tag msg = do
    mesgChan <- gets $ view messageChan
    liftIO $ writeChan mesgChan (MesgLog level tag $ show $ plain msg)

multiPermutation len elmts | len == 0 = []
multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
multiPermutation len elmts            = nubOrd $ [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]

replaceId a b = Text.unpack . Text.replace a b . Text.pack
mkPairMatch (FunctionCode name _ params ret) = FunctionCode (replaceId "Pair" "Pair_match" name) [] ret params

var2any env t@(ScalarT (TypeVarT _ id) _) | isBound env id = t
var2any env t@(ScalarT (TypeVarT _ id) _) | otherwise = AnyT
var2any env (ScalarT (DatatypeT id args l) r) = ScalarT (DatatypeT id (map (var2any env) args) l) r
var2any env (FunctionT x tArg tRet) = FunctionT x (var2any env tArg) (var2any env tRet)

fullStats locationName = do
    sigs <- gets $ view currentSigs
    fm <- view functionMap <$> get
    namemapping <- view nameMapping <$> get
    t2t <- view type2transition <$> get
    writeLog 3 locationName $ text "current sigs:" <+> pretty (Map.toList sigs)
    writeLog 3 locationName $ text "current fm:" <+> pretty (HashMap.toList $ HashMap.map funName fm)
    writeLog 3 locationName $ text "current nameMapping:" <+> pretty (Map.toList namemapping)
    writeLog 3 locationName $ text "current t2t:" <+> pretty (Map.toList t2t)

partitionDuplicateFunctions :: MonadIO m => [FunctionCode] -> PNSolver m ([FunctionCode],[FunctionCode])
partitionDuplicateFunctions symbols = do
    let groupedSymbols = groupSortBy compare symbols
    let deduplicatedSymbols = map head $ groupedSymbols
    let toBeRemoved = (concatMap tail $ groupedSymbols)
    let newGroupMap = map (\(x:xs) -> (x, x:xs)) (map (map funName) groupedSymbols)
    modify $ set groupMap (Map.fromList newGroupMap)
    return (deduplicatedSymbols, toBeRemoved)

removeDuplicates [] = return ()
removeDuplicates (FunctionCode{funName}:rest) = do
    gm <- view groupMap <$> get
    t2t <- view type2transition <$> get
    modify $ set type2transitionBackup t2t
    when (funName `elem` (concat $ Map.elems t2t)) (writeLog 1 "removeDuplicates" $ text "type2transition:" <+> text funName)
    modify $ over type2transition (Map.map (removeName funName))
    removeDuplicates rest
    where
        removeName :: String -> [String] -> [String]
        removeName _ [] = []
        removeName id (x:xs) | id == x = removeName id xs
                             | otherwise = x:(removeName id xs)

countDuplicateSigs :: Map Id AbstractSkeleton -> String
countDuplicateSigs sigs = let
    reversedSigs = groupByMap sigs
    countList = Map.toList reversedSigs
    dupes = [length (snd x) | x <- countList, length (snd x) > 1]
    in
        printf "%d classes; %d equiv; %d total" (length dupes) (sum dupes) (length (Map.toList sigs))

countDuplicates symbols = do
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
    return $ printf "%d classes; %d equiv; %d total" (length dupes) (sum dupes) (length symbols)

freshId :: (MonadIO m) => Id -> PNSolver m Id
freshId prefix = do
    indices <- gets $ flip (^.) nameCounter
    let idx = Map.findWithDefault 0 prefix indices
    modify (over nameCounter $ Map.insert prefix (idx+1))
    return $ prefix ++ show idx


-- | Replace all bound type variables with fresh free variables
freshType :: (MonadIO m) => RSchema -> PNSolver m RType
freshType sch = freshType' Map.empty [] sch
  where
    freshType' subst constraints (ForallT a sch) = do
        a' <- freshId "A"
        freshType' (Map.insert a (vart a' ftrue) subst) (a':constraints) sch
    freshType' subst constraints (Monotype t) = return (typeSubstitute subst t)


recoverNames :: Map Id Id -> RProgram -> RProgram
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