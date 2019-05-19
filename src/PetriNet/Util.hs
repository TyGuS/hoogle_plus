{-# LANGUAGE FlexibleContexts #-}

module PetriNet.Util where

import Types.Type
import Types.Common
import Types.Solver
import Types.Abstract
import Synquid.Program
import Synquid.Logic hiding (varName)
import Synquid.Type

import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Lens
import Control.Monad.State
import Debug.Trace
import Data.List.Extra
import Text.Pretty.Simple

-------------------------------------------------------------------------------
-- | helper functions
-------------------------------------------------------------------------------

writeLog level msg = do
    st <- get
    if level <= st ^. logLevel then traceShow msg $ return () else return ()

multiPermutation len elmts | len == 0 = [[]]
multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
multiPermutation len elmts            = nubOrd $ [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]


replaceId a b = Text.unpack . Text.replace (Text.pack a) (Text.pack b) . Text.pack
mkPairMatch (FunctionCode name _ params ret) = FunctionCode (replaceId "Pair" "Pair_match" name) [] ret params 

var2any env t@(ScalarT (TypeVarT _ id) _) | isBound env id = t
var2any env t@(ScalarT (TypeVarT _ id) _) | otherwise = AnyT
var2any env (ScalarT (DatatypeT id args l) r) = ScalarT (DatatypeT id (map (var2any env) args) l) r
var2any env (FunctionT x tArg tRet) = FunctionT x (var2any env tArg) (var2any env tRet)

freshId :: MonadIO m => Id -> PNSolver m Id
freshId prefix = do
    indices <- flip (^.) nameCounter <$> get
    let idx = Map.findWithDefault 0 prefix indices
    modify (over nameCounter $ Map.insert prefix (idx+1))
    return $ prefix ++ show idx

-- | Replace all bound type variables with fresh free variables
freshType :: MonadIO m => RSchema -> PNSolver m RType
freshType sch = freshType' Map.empty [] sch
  where
    freshType' subst constraints (ForallT a sch) = do
        a' <- freshId "A"
        freshType' (Map.insert a (vart a' ftrue) subst) (a':constraints) sch
    freshType' subst constraints (Monotype t) = return (typeSubstitute subst t)

freshAbstract :: MonadIO m => [Id] -> AbstractSkeleton -> PNSolver m AbstractSkeleton
freshAbstract bound t@(AScalar (ATypeVarT id)) | id `elem` bound = return t
freshAbstract bound (AScalar (ATypeVarT {})) = do
    v <- freshId "A"
    return (AScalar (ATypeVarT v))
freshAbstract bound (AScalar (ADatatypeT id args)) = do
    args' <- mapM (freshAbstract bound) args
    return (AScalar (ADatatypeT id args'))
freshAbstract bound (AFunctionT tArg tRes) = do
    tArg' <- freshAbstract bound tArg
    tRes' <- freshAbstract bound tRes
    return (AFunctionT tArg' tRes')

mkConstraint :: MonadIO m => [Id] -> Id -> AbstractSkeleton -> PNSolver m UnifConstraint
mkConstraint bound v t = do
    t' <- freshAbstract bound t
    return (AScalar (ATypeVarT v), t')
