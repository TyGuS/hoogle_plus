module Utility.GHCUtils where

import Data.Char (isLower)
import qualified Data.Text as Text
import GHC hiding (Id)
import Outputable

import Types.Type

-- turn a GHC type into Hoogle+ type
resolveType :: LHsType GhcPs -> SchemaSkeleton
resolveType (L _ (HsForAllTy _ _ bs t)) = foldr (ForallT . Text.pack . vname) (resolveType t) bs
 where
  vname :: LHsTyVarBndr GhcPs -> String
  vname (L _ (UserTyVar _ (L _ id))) = showSDocUnsafe (ppr id)
  vname (L _ (KindedTyVar _ (L _ id) _)) = showSDocUnsafe (ppr id)
  vname _ = error "not implemented"

-- TODO (zhg): why do we need this case?
-- resolveType t@(L _ (HsAppTy _ fun arg)) =
--   let typs          = tail $ map resolveType' $ breakApp t
--       (args, [res]) = splitAt (length typs - 1) typs
--   in  Monotype $ foldr (FunctionT "") res args
--  where
--   breakApp (L _ (HsAppTy _ fun arg)) = breakApp fun ++ [arg]
--   breakApp t                         = [t]

resolveType (L _ (HsQualTy _ ctx body)) = Monotype bodyWithTcArgs
 where
  unlocatedCtx  = let L _ c = ctx in c
  tyConstraints = map (prefixTyclass . resolveType') unlocatedCtx

  prefixTyclass (DatatypeT name args) =
    DatatypeT (tyclassPrefix `Text.append` name) args
  prefixTyclass tc = error $ "Unsupported type class " ++ show tc

  bodyWithTcArgs =
    foldr (FunctionT "") (toMonotype $ resolveType body) tyConstraints
resolveType t = Monotype $ resolveType' t

resolveType' :: LHsType GhcPs -> TypeSkeleton
resolveType' (L _ (HsFunTy _ f r)) =
  FunctionT "" (resolveType' f) (resolveType' r)
resolveType' (L _ (HsQualTy _ _ t      )) = resolveType' t
resolveType' (L _ (HsTyVar  _ _ (L _ v))) = if isLower (head name)
  then exists (Text.pack name)
  else DatatypeT (Text.pack name) []
  where name = showSDocUnsafe $ ppr v
resolveType' t@(L _ HsAppTy{}) = DatatypeT (Text.pack dtName) dtArgs
 where
  dtName = case datatypeOf t of
    "[]"  -> "List"
    "(,)" -> "Pair"
    n     -> n
  dtArgs = datatypeArgs t

  datatypeOf :: LHsType GhcPs -> String
  datatypeOf (L _ (HsAppTy _ f _      )) = datatypeOf f
  datatypeOf (L _ (HsTyVar _ _ (L _ v))) = showSDocUnsafe (ppr v)
  datatypeOf _                           = error "unexpected datatype"

  datatypeArgs :: LHsType GhcPs -> [TypeSkeleton]
  datatypeArgs (L _ (HsAppTy _ (L _ HsTyVar{}) a)) = [resolveType' a]
  datatypeArgs (L _ (HsAppTy _ f a)) = datatypeArgs f ++ datatypeArgs a
  datatypeArgs t = [resolveType' t]

resolveType' (L _ (HsListTy _ t    )) = listType (resolveType' t)
resolveType' (L _ (HsTupleTy _ _ ts)) = foldr pairType basePair otherTyps
 where
  resolveTyps           = map resolveType' ts
  (baseTyps, otherTyps) = splitAt (length ts - 2) resolveTyps
  basePair              = pairType (head baseTyps) (last baseTyps)
resolveType' (L _ (HsParTy _ t)) = resolveType' t
resolveType' t                   = error $ showSDocUnsafe (ppr t)