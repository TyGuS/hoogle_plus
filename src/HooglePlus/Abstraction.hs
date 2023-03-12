module HooglePlus.Abstraction
  ( firstLvAbs
  , allAbstractDts
  , specificAbstractionFromTypes
  , typesAtDepth
  , typesOfSize
  , abstractTypesUptoSize
  ) where

import Control.Monad.State
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List

import HooglePlus.Refinement
import Types.Common
import Types.Environment
import Types.Type
import Types.Pretty
import Utility.Utils

firstLvAbs :: Environment -> [SchemaSkeleton] -> AbstractCover
firstLvAbs env schs = Set.foldr (updateCover bvs) initCover dts
 where
  bvs       = getBoundTypeVars env
  typs      = map toMonotype schs
  initCover = Map.singleton rootNode Set.empty
  dts       = Set.unions (map (allAbstractDts bvs) typs)

typesAtDepth :: Set (Id, Int) -> Int -> Map Int [TypeSkeleton]
typesAtDepth dts lv
  | lv == 0 = Map.singleton 0 $ Set.toList $ Set.map (nullDatatype . fst) $ Set.filter ((== 0) . snd) dts
  | otherwise = let typeBank = typesAtDepth dts (lv-1)
                 in Map.insert lv (concat $ Set.toList $ Set.map (datatypeAt typeBank lv) (Set.filter ((> 0) . snd) dts)) typeBank
  where
    argLevels :: Int -> Int -> [[Int]]
    argLevels lv arity
      | arity == 1 = [[lv-1]]
      | otherwise = concatMap (\i -> map (insertAt i (lv-1)) (sequence (replicate (arity-1) [0..(lv-1)]))) [0..(arity-1)]

    datatypeAt :: Map Int [TypeSkeleton] -> Int -> (Id, Int) -> [TypeSkeleton]
    datatypeAt typeBank lv (dt, arity) = map (DatatypeT dt)
                                        $ (if lv == 1 then [map (vart . appendIndex varName) [1 .. arity]] else [])
                                        ++ concatMap (sequence . map (typeBank Map.!)) (argLevels lv arity)

abstractTypesUptoSize :: Set (Id, Int) -> Int -> Set TypeSkeleton
abstractTypesUptoSize dts sz =
  let typeBank = typesOfSize dts sz
   in Set.fromList (map ((`evalState` 0) . variablize) (concat $ Map.elems typeBank) ++ concat (Map.elems typeBank))

typesOfSize :: Set (Id, Int) -> Int -> Map Int [TypeSkeleton]
typesOfSize dts sz
  | sz <= 1 = Map.singleton 1 $ Set.toList $ Set.map (nullDatatype . fst) $ Set.filter ((== 0) . snd) dts
  | otherwise = let typeBank = typesOfSize dts (sz-1)
                 in Map.insert sz (concat $ Set.toList $ Set.map (datatypeAt typeBank sz) (Set.filter ((> 0) . snd) dts)) typeBank
  where
    argSizes :: Int -> Int -> [[Int]]
    argSizes sz arity
      | sz < arity = []
      | arity == 1 = [[sz]]
      | otherwise = concatMap (\i -> map (i:) $ argSizes (sz-i) (arity-1)) [1..(sz-1)]

    datatypeAt :: Map Int [TypeSkeleton] -> Int -> (Id, Int) -> [TypeSkeleton]
    datatypeAt typeBank sz (dt, arity) = map (DatatypeT dt)
                                        $ concatMap (sequence . map (typeBank Map.!)) (argSizes (sz-1) arity)

variablize :: TypeSkeleton -> State Int TypeSkeleton
variablize (TypeVarT {}) = get >>= \i -> modify (+1) >> return (vart $ appendIndex varName i)
variablize (DatatypeT _ []) = get >>= \i -> modify (+1) >> return (vart $ appendIndex varName i)
variablize (DatatypeT dt args) = DatatypeT dt <$> mapM variablize args
variablize (FunctionT x tArg tRes) = liftM2 (FunctionT x) (variablize tArg) (variablize tRes)
variablize t = error $ "unsupported type " ++ plainShow t

datatypeWithArity :: TypeSkeleton -> Set (Id, Int)
datatypeWithArity (DatatypeT dt args) = Set.insert (dt, length args) $ Set.unions (map datatypeWithArity args)
datatypeWithArity (FunctionT _ tArg tRes) = datatypeWithArity tArg `Set.union` datatypeWithArity tRes
datatypeWithArity _ = Set.empty

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
specificAbstractionFromTypes :: Environment -> [SchemaSkeleton] -> AbstractCover
specificAbstractionFromTypes env schemas =
  let abstrSkels = map (toAbstractFun . toAbstractType . toMonotype) schemas
      base       = Map.singleton rootNode Set.empty
  in  foldr (updateCover (getBoundTypeVars env)) base abstrSkels

