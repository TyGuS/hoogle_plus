module Types.Fresh
  ( Fresh(..)
  , freshId
  , freshType
  , freshSchema
 ) where

import Control.Monad (foldM)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Logic (LogicT)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text

import Types.Common
import Types.Type
import Types.Substitution

class Monad m => Fresh s m where
  nextCounter :: Id -> StateT s m Int

instance Monad m => Fresh (Map Id Int) m where
  nextCounter prefix = do
    counters <- get
    let counter = Map.findWithDefault 0 prefix counters
    put $ Map.insert prefix (counter + 1) counters
    return counter

instance Monad m => Fresh Int m where
  nextCounter prefix = do
    counter <- get
    put $ counter + 1
    return counter

freshId :: Fresh s m => [Id] -> Id -> StateT s m Id
freshId bvs prefix = do
  i <- nextCounter prefix
  let x = Text.pack $ Text.unpack prefix ++ show i
  if x `elem` bvs then freshId bvs prefix else return x

-- | Replace all bound type variables with fresh free variables

freshSchema :: Fresh s m => [Id] -> SchemaSkeleton -> StateT s m SchemaSkeleton
freshSchema bounds t = go Map.empty t
 where
  go :: Fresh s m => TypeSubstitution -> SchemaSkeleton -> StateT s m SchemaSkeleton
  go subst (ForallT a sch) = do
    a' <- freshId bounds "tau"
    let v = if Text.take 1 a == wildcardPrefix then wildcardPrefix `Text.append` a' else a'
    go (Map.insert a (vart v) subst) sch
  go subst (Monotype t) = return $ Monotype (apply subst t)

freshType :: Fresh s m => [Id] -> TypeSkeleton -> StateT s m TypeSkeleton
freshType bound t = snd <$> go bound Map.empty t
 where
  go :: Fresh s m => [Id] -> TypeSubstitution -> TypeSkeleton -> StateT s m (TypeSubstitution, TypeSkeleton)
  go bound m t = case t of
    TypeVarT id
      | id `elem` bound -> return (m, t)
      | id `Map.member` m -> return (m, fromJust (Map.lookup id m))
      | otherwise -> do
        v <- freshId bound "A"
        let t = TypeVarT v
        return (Map.insert id t m, t)
    DatatypeT dt tArgs -> do
      (m', tArgs') <- foldM (freshArg bound) (m, []) tArgs
      return (m', DatatypeT dt tArgs')
    FunctionT x tArg tRes -> do
      (m' , tArg') <- go bound m tArg
      (m'', tRes') <- go bound m' tRes
      return (m'', FunctionT x tArg' tRes')
    _ -> return (m, t)

  freshArg :: Fresh s m => [Id] -> (TypeSubstitution, [TypeSkeleton]) -> TypeSkeleton -> StateT s m (TypeSubstitution, [TypeSkeleton])
  freshArg bound (accm, acct) tArg = do
    (m', tArg') <- go bound accm tArg
    return (m', acct ++ [tArg'])
