module Types.TypeClass
  (
    Tycl
  , TyclAssignment
  , TyclCache
  , MonadTycl
  , runMonadTycl
  , supportedTycls
  , getTycl
  , commonTycls
  , getDtTycls
  ) where

import Control.Monad.State
import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Maybe (catMaybes)
import GHC (exprType, TcRnExprMode(TM_Default))
import System.IO (stdout, stderr)

import Text.Printf (printf)
import System.IO.Silently (hSilence)

import Database.Dataset
import Examples.Utils
import Types.Common
import Types.Pretty
import Types.Substitution
import Types.Type

type Tycl = Id
type TyclAssignment = Map Id (Set Tycl)
type TyclCache = Map TypeSkeleton [Tycl]
type MonadTycl m = StateT TyclCache m

runMonadTycl :: Monad m => MonadTycl m a -> m a
runMonadTycl go = evalStateT go Map.empty

supportedTycls :: [Tycl]
supportedTycls = ["Num", "Ord", "Eq"]

getTycl :: MonadIO m => TyclAssignment -> TypeSkeleton -> TypeSkeleton -> MonadTycl m (Maybe (Set Tycl))
getTycl tyclAss t1 t2 = do
  tycls <- commonTycls tyclAss t1 t2
  return (guard (Set.null tycls) >> Just tycls)

-- return common type classes that are satisfied by the given types
commonTycls :: MonadIO m => TyclAssignment -> TypeSkeleton -> TypeSkeleton -> MonadTycl m (Set Tycl)
commonTycls tyclAss (TypeVarT _ id1) (TypeVarT _ id2) = do
  -- assume both type variables cannot be freely unified
  let tc1 = Map.findWithDefault Set.empty id1 tyclAss
  let tc2 = Map.findWithDefault Set.empty id2 tyclAss
  return $ tc1 `Set.intersection` tc2
commonTycls tyclAss (TypeVarT _ id) t2 = do
  -- check the type class for the data type
  tccache <- get
  let tc1 = Map.findWithDefault Set.empty id tyclAss
  case Map.lookup t2 tccache of
    Just tc2 -> return $ tc1 `Set.intersection` Set.fromList tc2
    Nothing  -> Set.intersection tc1 <$> (getDtTycls tyclAss t2)
commonTycls tyclAss t1 t2@TypeVarT{} = commonTycls tyclAss t2 t1
commonTycls tyclAss t1 t2 = do
  tccache <- get
  tc1 <- maybe (getDtTycls tyclAss t1) (return . Set.fromList) (Map.lookup t1 tccache)
  tc2 <- maybe (getDtTycls tyclAss t2) (return . Set.fromList) (Map.lookup t2 tccache)
  return $ tc1 `Set.intersection` tc2

getDtTycls :: MonadIO m => TyclAssignment -> TypeSkeleton -> MonadTycl m (Set Tycl)
getDtTycls tcass typ = do
  mbCandidates <- mapM (mkTyclQuery tcass typ) supportedTycls
  let tc = catMaybes mbCandidates
  modify $ Map.insert typ tc
  return $ Set.fromList tc

mkTyclQuery :: MonadIO m => TyclAssignment -> TypeSkeleton -> Tycl -> MonadTycl m (Maybe Tycl)
mkTyclQuery tcass typ tycl = do
  let vars = Set.toList $ freeVars typ
  let varTcs = concatMap (\v -> maybe [] (map (v,) . Set.toList) (Map.lookup v tcass)) vars
  let varTcStrs = map (\(v, tc) -> Text.unwords [tc, v]) varTcs
  let allTcs = Text.intercalate ", " $  varTcStrs ++ [Text.pack $ printf "%s (%s)" tycl (plainShow typ)]
  let query = printf "undefined :: (%s) => ()" allTcs
  liftIO $ hSilence [stdout, stderr]
         $ catch (askGhc includedModules (exprType TM_Default query) >> return (Just tycl))
                 (\(e :: SomeException) -> print e >> return Nothing)