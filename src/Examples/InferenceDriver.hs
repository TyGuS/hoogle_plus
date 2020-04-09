module Examples.InferenceDriver(
    
    ) where

import Database.Util
import Types.Type
import Types.IOFormat
import Types.InfConstraint
import Examples.Utils
import Synquid.Type
import PetriNet.Util
import Synquid.Pretty

import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC (exprType, typeToLHsType) hiding (Id)
import GHC.Paths
import TcRnDriver
import Outputable
import Control.Monad.State
import Data.List.Extra (groupOn)

parseExample :: [String] -> String -> IO (Either RSchema ErrorMessage)
parseExample mdls mkFun = catch (do
    typ <- askGhc mdls $ exprType TM_Inst mkFun
    let hsType = typeToLHsType typ
    return (Left $ toInt $ resolveType hsType))
    (\(e :: SomeException) -> return (Right $ show e))
    where
        toInt (ForallT x t) = ForallT x (toInt t)
        toInt (Monotype t) = Monotype (integerToInt t)

-- turn a GHC type into Hoogle+ type
resolveType :: LHsType GhcPs -> RSchema
resolveType (L _ (HsForAllTy bs t)) = foldr ForallT (resolveType t) vs
    where
        vs = map vname bs
        vname (L _ (UserTyVar (L _ id))) = showSDocUnsafe (ppr id)
        vname (L _ (KindedTyVar (L _ id) _)) = showSDocUnsafe (ppr id)
resolveType (L _ (HsFunTy f _)) = Monotype (resolveType' f)
resolveType (L _ (HsQualTy ctx body)) = Monotype bodyWithTcArgs
    where
        unlocatedCtx = let L _ c = ctx in c
        tyConstraints = map resolveType' unlocatedCtx

        toClassConstraint tc@(ScalarT (DatatypeT name args rs) r) = 
            case head args of
              ScalarT (TypeVarT _ v) -> (v, name)
              _ -> error $ "Unsupported type class " ++ show tc
        toClassConstraint tc = error $ "Unsupported type class " ++ show tc

        tcConstraints = map toClassConstraint tyConstraints
        sortedTyclass = groupOn fst $ sortOn fst tcConstraints
        tcMap = Map.fromList $ map (\p -> over _1 head $ unzip p) sortedTyclass
        bodyWithTcArgs = (resolveType' body)
resolveType t = error (showSDocUnsafe $ ppr t)

addTcConstraint :: Map Id (Set Id) -> TypeSkeleton r -> TypeSkeleton r
addTcConstraint tcMap (ScalarT (TypeVarT m v) r) =
    case Map.lookup m tcMap of
      Just s -> ScalarT (TypeVarT (Set.foldr (\tc -> Map.insert tc AnyT) m s) v) r
      Nothing -> ScalarT (TypeVarT m v) r
addTcConstraint tcMap (ScalarT (DatatypeT name args rs) r) =
    let argsWithTc = map (addTcConstraint tcMap) args
     in ScalarT (DatatypeT name argsWithTc rs) r
addTcConstraint tcMap (FunctionT x tArg tRes) =
    let tArgWithTc = addTcConstraint tcMap tArg
        tResWithTc = addTcConstraint tcMap tRes
     in FunctionT x tArgWithTc tResWithTc
addTcConstraint tcMap t = t

resolveType' :: LHsType GhcPs -> RType
resolveType' (L _ (HsFunTy f r)) = FunctionT "" (resolveType' f) (resolveType' r)
resolveType' (L _ (HsQualTy _ t)) = resolveType' t
resolveType' (L _ (HsTyVar _ (L _ v))) = 
    if isLower (head name)
       then ScalarT (TypeVarT Map.empty name) ftrue
       else ScalarT (DatatypeT name [] []) ftrue
    where
        name = showSDocUnsafe $ ppr v
resolveType' t@(L _ HsAppTy{}) = ScalarT (DatatypeT dtName dtArgs []) ftrue
    where
        dtName = case datatypeOf t of
                   "[]" -> "List"
                   "(,)" -> "Pair"
                   n -> n
        dtArgs = datatypeArgs t

        datatypeOf (L _ (HsAppTy f _)) = datatypeOf f
        datatypeOf (L _ (HsTyVar _ (L _ v))) = showSDocUnsafe (ppr v)

        datatypeArgs (L _ (HsAppTy (L _ HsTyVar {}) a)) = [resolveType' a]
        datatypeArgs (L _ (HsAppTy f a)) = datatypeArgs f ++ datatypeArgs a
        datatypeArgs t = [resolveType' t]

resolveType' (L _ (HsListTy t)) = ScalarT (DatatypeT "List" [resolveType' t] []) ftrue
resolveType' (L _ (HsTupleTy _ ts)) = foldr mkPair basePair otherTyps
    where
        mkPair acc t = ScalarT (DatatypeT "Pair" [acc, t] []) ftrue
        resolveTyps = map resolveType' ts
        (baseTyps, otherTyps) = splitAt (length ts - 2) resolveTyps
        basePair = ScalarT (DatatypeT "Pair" baseTyps []) ftrue
resolveType' (L _ (HsParTy t)) = resolveType' t
resolveType' t = error $ showSDocUnsafe (ppr t)

