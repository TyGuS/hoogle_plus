module Types.SubsumptionLattice
  (
    substSubtract
  ) where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Logic

import Types.Type
import Types.Substitution
import Types.Antiunifier
import Types.Fresh
import Types.Pretty
import Types.TypeChecker

data LatticeNode = LatticeNode {
  subst :: TypeSubstitution,
  children :: [LatticeNode]
}

type Lattice = LatticeNode

insert :: (MonadIO m, MonadFail m) => TypeSubstitution -> Lattice -> MonadCounter m Lattice
insert su (LatticeNode n cs) = do
  nameCounter <- get
  lcgs <- runAntiunifier $ lift (lift (put nameCounter)) >> mapM (antiunify su . subst) cs
  let compatibles = map (Map.filter (not . isTypeVar)) lcgs
  LatticeNode n <$> insertChild su (zip cs compatibles)

insertChild :: (MonadIO m, MonadFail m) => TypeSubstitution -> [(LatticeNode, TypeSubstitution)] -> MonadCounter m [LatticeNode]
insertChild su [] = return [LatticeNode su []]
insertChild su ((c, tau):cs)
  -- case I: none of the children nodes is compatible with the new one,
  -- so we add a new leaf node for it.
  | Map.size tau == 0 = (c :) <$> insertChild su cs

  -- case II: one of the children nodes has the same substitution result as the new one,
  -- which means that the new node should be inserted as a subtree.
  | tau == subst c = (: map fst cs) <$> insert (su `substSubtract` tau) c

  -- case III: one of the children nodes partially matched the new one,
  -- which indicates that we need to change the label of this matched child node to fit the new one.
  | otherwise = do
    let newLabel = subst c `substSubtract` tau
    let childLabel = su `substSubtract` tau
    let newNode = LatticeNode tau [LatticeNode newLabel (children c), LatticeNode childLabel []]
    return $ newNode : map fst cs

--- Miscellaneous

substSubtract :: TypeSubstitution -> TypeSubstitution -> TypeSubstitution
substSubtract su1 su2 = Map.foldrWithKey (\v t su -> absenceOrModify v t su getUnifier) Map.empty su1
  where
    absenceOrModify v t su f = case Map.lookup v su2 of
      Nothing -> Map.insert v t su
      Just t' -> f t t' su

    getUnifier t t' su =
      case solveTypeConstraint (Set.toList $ freeVars t) su (UnifiesWith t t') of
        Nothing -> error $ "cannot subtract " ++ plainShow t ++ " by " ++ plainShow t'
        Just su' -> su'