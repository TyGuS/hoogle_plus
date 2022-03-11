module HooglePlus.Abstraction where

import Types.Common
import Types.Type
import Types.Abstract
import Types.Environment
import PetriNet.AbstractType
import Synquid.Type
import Synquid.Util
import Synquid.Program
import Synquid.Logic (ftrue)
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

firstLvAbs :: Environment -> [SchemaSkeleton] -> AbstractCover
firstLvAbs env schs = Set.foldr (updateCover tvs) initCover dts
  where
    tvs = env ^. boundTypeVars
    typs = map (shape . toMonotype) schs
    initCover = HashMap.singleton rootNode Set.empty
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
specificAbstractionFromTypes :: Environment -> [SchemaSkeleton] -> AbstractCover
specificAbstractionFromTypes env schemas = let
    abstrSkels = map (compactAbstractType . toAbstractType . shape . toMonotype) schemas
    base = HashMap.singleton rootNode Set.empty
    in
        foldr (updateCover (env ^. boundTypeVars)) base abstrSkels

