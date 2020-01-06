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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
allAbstractDts bound _ t@(TypeVarT v k) | v `elem` bound = Set.singleton (AScalar (ATypeVarT v))
allAbstractDts bound dts (DatatypeT id k) = case k of
    KnStar -> Set.singleton (AScalar (ADatatypeT id []))
    KnArr _ _ -> error "Arrow kind is not a datatype"
allAbstractDts bound dts t@(TyAppT tFun tArg _) = appDt `Set.insert` funDts `Set.union` argDts
    where
        appDt = toAbstractType t
        funDts = allAbstractDts bound dts tFun
        argDts = allAbstractDts bound dts tArg
allAbstractDts bound dts t@(TyFunT tArg tRes) = funDt `Set.insert` argDts `Set.union` resDts
    where
        funDt = toAbstractType t
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