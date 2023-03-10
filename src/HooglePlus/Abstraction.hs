module HooglePlus.Abstraction
  ( firstLvAbs
  , allAbstractDts
  , specificAbstractionFromTypes
  ) where

import qualified Data.Char                     as Char
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           HooglePlus.Refinement
import           Types.Common
import           Types.Environment
import           Types.Type
import           Utility.Utils

firstLvAbs :: Environment -> [SchemaSkeleton] -> AbstractCover
firstLvAbs env schs = Set.foldr (updateCover bvs) initCover dts
 where
  bvs       = getBoundTypeVars env
  typs      = map toMonotype schs
  initCover = Map.singleton rootNode Set.empty
  dts       = Set.unions (map (allAbstractDts bvs) typs)

allAbstractDts :: [Id] -> TypeSkeleton -> Set TypeSkeleton
allAbstractDts bound t@(TypeVarT _ v) | v `elem` bound = Set.singleton (vart v)
allAbstractDts bound t@(DatatypeT id args) = dt `Set.insert` argDts
 where
  newArgs = map (vart . appendIndex varName) [1 .. (length args)]
  dt      = DatatypeT id newArgs
  argDts  = Set.unions (map (allAbstractDts bound) args)
allAbstractDts bound (FunctionT _ tArg tRes) =
  allAbstractDts bound tArg `Set.union` allAbstractDts bound tRes
allAbstractDts _ _ = Set.empty

-- Produce the most specific abstraction possible from the given types.
specificAbstractionFromTypes
  :: Environment -> [SchemaSkeleton] -> AbstractCover
specificAbstractionFromTypes env schemas =
  let abstrSkels = map (toAbstractFun . toAbstractType . toMonotype) schemas
      base       = Map.singleton rootNode Set.empty
  in  foldr (updateCover (getBoundTypeVars env)) base abstrSkels

