module HooglePlus.Abstraction where

import HooglePlus.Refinement
import PetriNet.AbstractType
import PetriNet.Util
import Synquid.Logic (ftrue)
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Util
import Types.Abstract
import Types.Common
import Types.Environment
import Types.Solver
import Types.Type

import Control.Lens
import Control.Monad.State
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.List.Extra
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf

firstLvAbs :: Environment -> [RSchema] -> AbstractCover
firstLvAbs env schs = Set.foldr (updateCover tvs) initCover dts
  where
    tvs = env ^. boundTypeVars
    typs = map (shape . toMonotype) schs
    initCover = HashMap.singleton rootNode Set.empty
    dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars)) typs)

allAbstractDts :: [Id] -> SType -> Set AbstractSkeleton
allAbstractDts bound t@(ScalarT (TypeVarT _ v) _)
    | v `elem` bound = Set.singleton (AScalar (ATypeVarT v))
allAbstractDts bound t@(ScalarT (DatatypeT id args _) _) = dt `Set.insert` argDts
  where
    newArgs = map (\i -> AScalar (ATypeVarT (varName ++ show i))) [1 .. (length args)]
    dt = AScalar (ADatatypeT id newArgs)
    argDts = Set.unions (map (allAbstractDts bound) args)
allAbstractDts bound (FunctionT _ tArg tRes) =
    allAbstractDts bound tArg `Set.union` allAbstractDts bound tRes
allAbstractDts _ _ = Set.empty

-- Produce the most specific abstraction possible from the given types.
specificAbstractionFromTypes :: Environment -> [RSchema] -> AbstractCover
specificAbstractionFromTypes env schemas =
    let abstrSkels = map (compactAbstractType . toAbstractType . shape . toMonotype) schemas
        base = HashMap.singleton rootNode Set.empty
     in foldr (updateCover (env ^. boundTypeVars)) base abstrSkels
