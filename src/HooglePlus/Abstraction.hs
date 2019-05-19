module HooglePlus.Abstraction where

import Types.Common
import Types.Type
import Types.Abstract
import Types.Environment
import PetriNet.AbstractType
import Synquid.Type
import Types.Environment
import Synquid.Util
import Synquid.Program
import Synquid.Logic (ftrue)
import Types.Solver
import Synquid.Pretty
import PetriNet.Util

import Data.Maybe
import Data.List
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Lens
import qualified Data.Char as Char
import Text.Printf
import Control.Monad.State

firstLvAbs :: Environment -> [RSchema] -> Set AbstractSkeleton
firstLvAbs env schs = dts
  where
    typs = map (shape . toMonotype) schs
    dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars)) typs)

allAbstractDts :: [Id] -> SType -> Set AbstractSkeleton
allAbstractDts bound t@(ScalarT (TypeVarT _ v) _) | v `elem` bound = Set.singleton (AScalar (ATypeVarT v))
allAbstractDts bound t@(ScalarT (DatatypeT id args _) _) = dt `Set.insert` argDts
  where
    newArgs = map (\i -> AScalar (ATypeVarT (varName ++ show i))) [1..(length args)]
    dt = AScalar (ADatatypeT id newArgs)
    argDts = Set.unions (map (allAbstractDts bound) args)
allAbstractDts bound (FunctionT _ tArg tRes) = allAbstractDts bound tArg `Set.union` allAbstractDts bound tRes
allAbstractDts _ _ = Set.empty

-- Produce the most specific abstraction possible from the given types.
specificAbstractionFromTypes :: Environment -> [RSchema] -> Set AbstractSkeleton
specificAbstractionFromTypes env schemas = let
    abstrSkels = concatMap (decompose . toAbstractType . shape . toMonotype) schemas
    baseTree = Set.empty
    in
        foldr Set.insert baseTree abstrSkels

abstractParamList :: AbstractSkeleton -> [AbstractSkeleton]
abstractParamList t@(AScalar {}) = [t]
abstractParamList (AFunctionT tArg tFun) =
    case tFun of
        AScalar _  -> [tArg]
        _          -> (tArg) : (abstractParamList tFun)

lastAbstractType :: AbstractSkeleton -> AbstractSkeleton
lastAbstractType (AFunctionT tArg tFun) = lastAbstractType tFun
lastAbstractType t                      = t

