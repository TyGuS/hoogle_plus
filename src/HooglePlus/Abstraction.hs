module HooglePlus.Abstraction where

import Types.Common
import Types.Type
import Types.Abstract
import Types.Environment
import PetriNet.AbstractType
import Synquid.Type
import Synquid.Util
import Synquid.Program
import Types.Solver
import Synquid.Pretty
import PetriNet.Util
import HooglePlus.Refinement

import Data.Maybe
import Data.List
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import Control.Monad.State

firstLvAbs :: Environment -> [SchemaSkeleton] -> AbstractCover
firstLvAbs env schs = Set.foldr (updateCover tvs) initCover dts
  where
    tvs = env ^. boundTypeVars
    typs = map toMonotype schs
    initCover = HashMap.singleton rootNode Set.empty
    dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars) (env ^. datatypes)) typs)

allAbstractDts :: [Id] -> Map Id DatatypeDef -> TypeSkeleton -> Set AbstractSkeleton
allAbstractDts bound _ t@(TypeVarT v k) | v `elem` bound = Set.singleton (ATypeVarT v k)
allAbstractDts bound dts (DatatypeT id k) = 
    let kind = case Map.lookup id dts of
                    Nothing -> error $ "Cannot find datatype " ++ id
                    Just df -> length (df ^. typeParams)
    in Set.singleton $ fillDtArgs (ADatatypeT id k, mkKind kind)
    where
        fillDtArgs (t, KnStar) = t
        fillDtArgs (t, KnArr k1 k2) = fillDtArgs (ATyAppT t (ATypeVarT ("T" ++ show k) KnStar) k2, k2)
allAbstractDts bound dts (TyAppT tFun tArg _) = funDts `Set.union` argDts
    where
        funDts = allAbstractDts bound dts tFun
        argDts = allAbstractDts bound dts tArg
allAbstractDts bound dts (TyFunT tArg tRes) = argDts `Set.union` resDts
    where
        argDts = allAbstractDts bound dts tArg
        resDts = allAbstractDts bound dts tRes
allAbstractDts bound dts (FunctionT _ tArg tRes) = argDts `Set.union` resDts
    where
        argDts = allAbstractDts bound dts tArg
        resDts = allAbstractDts bound dts tRes
allAbstractDts _ _ _ = Set.empty

-- Produce the most specific abstraction possible from the given types.
specificAbstractionFromTypes :: Environment -> [SchemaSkeleton] -> AbstractCover
specificAbstractionFromTypes env schemas = let
    abstrSkels = map (compactAbstractType . toAbstractType . toMonotype) schemas
    base = HashMap.singleton rootNode Set.empty
    in
        foldr (updateCover (env ^. boundTypeVars)) base abstrSkels