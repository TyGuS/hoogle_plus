{-# LANGUAGE FlexibleContexts #-} 
module PetriNet.Utils where

import Database.Utils (tyclassArgBase, hoPostfix)
import Encoder.ConstraintEncoder
import HooglePlus.Utils
import qualified Types.TypeChecker as Checker
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Utils
import Types.CheckMonad
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Filtering (AssociativeExamples)
import Types.IOFormat
import Types.Program
import Types.Solver
import Types.Type
import Types.TypeChecker (Checker)

import Control.Concurrent.Chan
import Control.Lens
import Control.Monad.State
import Control.Monad.Extra
import Control.Monad.ST (runST, ST)
import Debug.Trace
import Data.Array.ST (STArray, readArray, writeArray, newListArray, getElems)
import Data.Hashable
import Data.List.Extra hiding (stripSuffix)
import Data.Maybe
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Text.Pretty.Simple
import Text.Printf
import qualified Hoogle as Hoogle

--------------------------------------------------------------------------------
-- | Petri Net helper functions
--------------------------------------------------------------------------------

-- | turn an abstract type into encoded petri net
encodeFunction :: Id -> AbstractSkeleton -> FunctionCode
encodeFunction id t | pairProj `isPrefixOf` id =
    let toMatch (FunctionCode name [p1,p2] ret) = FunctionCode id [p1] (p2:ret)
     in toMatch $ encodeFunction "__f" t
encodeFunction id t@(FunctionT _ tArg tRet) = FunctionCode id params [lastType t]
    where
        base = (0, [])
        params = allArgTypes t
encodeFunction id t = FunctionCode id [] [t]

assemblePair :: AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
assemblePair first secod | allArgTypes first == allArgTypes secod =
    let FunctionT _ p f = first
        FunctionT _ _ s = secod
     in FunctionT "p" p (FunctionT "x" f s)
assemblePair first second = error "fst and snd have different arguments"

--------------------------------------------------------------------------------
-- | transition helper functions
--------------------------------------------------------------------------------

-- | find symbol types by their names
findSymbol :: (CheckMonad (t m), MonadIO (t m), MonadIO m) => Environment -> Id -> t m TypeSkeleton
findSymbol env sym = do
    nameMap <- getNameMapping
    let name = fromMaybe sym (Map.lookup sym nameMap)
    let bound = env ^. boundTypeVars
    case lookupSymbol name 0 env of
        Nothing ->
            case lookupSymbol ("(" ++ name ++ ")") 0 env of
                Nothing -> do
                    setIsChecked False
                    -- writeLog 2 "findSymbol" $ text "cannot find symbol" <+> text name <+> text "in the current environment"
                    return AnyT
                Just sch -> freshType bound sch
        Just sch -> freshType bound sch

--------------------------------------------------------------------------------
-- | create fresh items
--------------------------------------------------------------------------------

-- | create variable with fresh id
freshId :: (CheckMonad (t m), MonadIO m) => [Id] -> Id -> t m Id
freshId bounds prefix = do
    indices <- getNameCounter
    let idx = Map.findWithDefault 0 prefix indices
    setNameCounter $ Map.insert prefix (idx+1) indices
    let v = prefix ++ show idx
    if v `elem` bounds then freshId bounds prefix else return v

-- | instantiate all type variables with fresh free variables
freshType :: (CheckMonad (t m), MonadIO m) => [Id] -> SchemaSkeleton -> t m TypeSkeleton
freshType bounds = freshType' Map.empty []
  where
    freshType' subst constraints (ForallT a sch) = do
        a' <- freshId bounds "tau"
        freshType' (Map.insert a (TypeVarT a') subst) (a':constraints) sch
    freshType' subst constraints (Monotype t) = return (typeSubstitute subst t)

--------------------------------------------------------------------------------
-- | Helper functions for result output
--------------------------------------------------------------------------------

-- | remove html tags
innerTextHTML :: String -> String
innerTextHTML ('<':xs) = innerTextHTML $ drop 1 $ dropWhile (/= '>') xs
innerTextHTML (x:xs) = x : innerTextHTML xs
innerTextHTML [] = []

-- | turn html code to normal text
unHTML :: String -> String
unHTML = unescapeHTML . innerTextHTML

-- | create the output text with hoogle info
toOutput :: Environment -> TProgram -> AssociativeExamples -> IO QueryOutput
toOutput env soln exs = do
    let symbols = Set.toList $ symbolsOf soln
    let argNames = map fst $ env ^. arguments
    let args = env ^. arguments
    let argDocs = map (\(n, ty) -> FunctionDoc n (show ty) "") args
    let symbolsWoArgs = symbols \\ argNames
    docs <- liftIO $ hoogleIt symbolsWoArgs
    entries <- mapM (\(sol, ex) -> ResultEntry (toHaskellSolution sol) <$> mapM niceInputs ex) exs
    return $ QueryOutput entries "" (docs ++ argDocs)
    where
        hoogleIt syms = do
            dbPath <- Hoogle.defaultDatabaseLocation
            Hoogle.withDatabase dbPath (\db -> do
                let targets = map (head . Hoogle.searchDatabase db) syms
                let docs = map targetToDoc targets
                return docs)

        targetToDoc tg = let wholeSig = unHTML $ Hoogle.targetItem tg
                             segs = splitOn " :: " wholeSig
                             name = head segs
                             sig = unwords $ tail segs
                             doc = unHTML $ Hoogle.targetDocs tg
                          in FunctionDoc name sig doc

-- | replace the fake "f*" functions names with their real names
recoverNames :: Map Id Id -> Program t -> Program t
recoverNames mapping (Program (PSymbol sym) t) =
    case Map.lookup sym mapping of
      Nothing -> Program (PSymbol (stripSuffix sym)) t
      Just name -> Program (PSymbol (stripSuffix name)) t
recoverNames mapping (Program (PApp fun pArg) t) = Program (PApp fun' pArg') t
  where
    fun' = case Map.lookup fun mapping of
                Nothing -> stripSuffix fun
                Just name -> stripSuffix name
    pArg' = map (recoverNames mapping) pArg
recoverNames mapping (Program (PFun x body) t) = Program (PFun x body') t
  where
    body' = recoverNames mapping body

--------------------------------------------------------------------------------
-- | Miscellaneous
--------------------------------------------------------------------------------

foArgsOf :: Environment -> [(Id, SchemaSkeleton)]
foArgsOf = filter (not . isFunctionType . toMonotype . snd) . _arguments

getExperiment exp = gets $ view (searchParams . exp)

writeLog :: CheckMonad m => Int -> String -> Doc -> m ()
writeLog level tag msg = do
    logLevel <- getLogLevel
    when (level <= logLevel) (trace (printf "[%s]: %s\n" tag (show $ plain msg)) $ return ())

stripSuffix :: String -> String
stripSuffix = replaceId hoPostfix "" . removeLast '_'

removeLast :: Char -> String -> String
removeLast c1 = snd . remLast
    where
        remLast :: String -> (Bool, String)
        remLast [] = (False, [])
        remLast (c2:cs) =
            case remLast cs of
                (True, cs') -> (True, c2:cs')
                (False, cs') -> if c1 == c2 then (True, []) else (False, c2:cs')
