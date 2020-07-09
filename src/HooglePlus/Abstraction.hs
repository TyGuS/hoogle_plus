module HooglePlus.Abstraction where

import Types.Common
import Types.Type
import Types.Environment
import PetriNet.AbstractType
import Synquid.Type
import Synquid.Utils
import Synquid.Program
import Types.Solver
import Synquid.Pretty
import PetriNet.Utils
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
firstLvAbs env schs = Set.foldr (updateCover env) initCover dts
    where
        typs = map toMonotype schs
        initCover = HashMap.singleton rootNode Set.empty
        dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars)) typs)

allAbstractDts :: [Id] ->TypeSkeleton -> Set AbstractSkeleton
allAbstractDts bound (TypeVarT v) | v `elem` bound = Set.singleton (TypeVarT v)
allAbstractDts bound t@(TyAppT tFun tArg) = t `Set.insert` argDts
    where
        (dt, args) = collectArgs t
        argDts = Set.unions $ map (allAbstractDts bound) args
allAbstractDts bound t@(TyFunT tArg tRes) = t `Set.insert` argDts `Set.union` resDts
    where
        argDts = allAbstractDts bound tArg
        resDts = allAbstractDts bound tRes
allAbstractDts bound (FunctionT _ tArg tRes) = argDts `Set.union` resDts
    where
        argDts = allAbstractDts bound tArg
        resDts = allAbstractDts bound tRes
allAbstractDts _ _ = Set.empty

-- Produce the most specific abstraction possible from the given types.
specificAbstractionFromTypes :: Environment -> [SchemaSkeleton] -> AbstractCover
specificAbstractionFromTypes env schemas =
    foldr (updateCover env) base abstrSkels
    where
        abstrSkels = map (toAbstractFun . toMonotype) schemas
        base = HashMap.singleton rootNode Set.empty