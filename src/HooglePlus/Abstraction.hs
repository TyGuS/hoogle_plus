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
import qualified Data.Map as Map
import Control.Lens
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import Control.Monad.State

firstLvAbs :: Environment -> [RSchema] -> AbstractCover
firstLvAbs env schs = Set.foldr (updateCover tvs) initCover dts
  where
    tvs = env ^. boundTypeVars
    typs = map (shape . toMonotype) schs
    initCover = HashMap.singleton rootNode Set.empty
    dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars)) typs)

allAbstractDts :: [Id] -> TypeSkeleton -> Set AbstractSkeleton
allAbstractDts bound (TypeVarT v) | v `elem` bound = Set.singleton (ATypeVarT v)
allAbstractDts bound (DatatypeT id k) = 
    let (t, _) = fillDtArgs k 0
    in Set.singleton (ATyAppT (ADatatypeT id k) t)
    where
        fillDtArgs (KnStar, i) = (ATypeVarT ("T" ++ show i), i+1)
        fillDtArgs (KnArr k1 k2, i) = let
            (t1, i') = fillDtArgs (k1, i)
            (t2, i'') = fillDtArgs (k2, i')
            in (ATyAppT t1 t2, i''+1)
allAbstractDts bound (TyAppT tFun tArg) = funDts `Set.union` argDts
    where
        funDts = allAbstractDts bound tFun
        argDts = allAbstractDts bound tArg
allAbstractDts bound (TyFunT tArg tRes) = argDts `Set.union` resDts
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
specificAbstractionFromTypes env schemas = let
    abstrSkels = map (compactAbstractType . toAbstractType . toMonotype) schemas
    base = HashMap.singleton rootNode Set.empty
    in
        foldr (updateCover (env ^. boundTypeVars)) base abstrSkels