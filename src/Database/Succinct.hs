module Database.Succinct where

import Language.Haskell.Exts
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List
import Data.Maybe
import Control.Lens as Lens
import Control.Monad.State

import Synquid.Type
-- import Synquid.Succinct

nameStr name = case name of
    Ident _ var -> var
    Symbol _ var -> var

moduleNameStr (ModuleName _ name) = name

allTypeVars (TyForall _ _ _ typ) = allTypeVars typ
allTypeVars (TyFun _ arg ret) = allTypeVars arg `Set.union` allTypeVars ret
allTypeVars (TyTuple _ _ typs) = foldr (\t vars -> vars `Set.union` allTypeVars t) Set.empty typs
allTypeVars (TyUnboxedSum _ typs) = foldr (\t vars -> vars `Set.union` allTypeVars t) Set.empty typs
allTypeVars (TyList _ typs) = allTypeVars typs
allTypeVars (TyApp _ fun arg) = allTypeVars fun `Set.union` allTypeVars arg
allTypeVars (TyVar _ name) = Set.singleton $ nameStr name
allTypeVars (TyCon _ name) = Set.empty
allTypeVars (TyParen _ typ) = allTypeVars typ
allTypeVars (TyInfix _ typ1 _ typ2) = allTypeVars typ1 `Set.union` allTypeVars typ2
allTypeVars (TyKind _ typ _) = allTypeVars typ
allTypeVars (TyPromoted _ _) = Set.empty
allTypeVars (TyEquals _ ltyp rtyp) = allTypeVars ltyp `Set.union` allTypeVars rtyp
allTypeVars _ = Set.empty

toSynquidSchema :: Type () -> State Int SSchema
toSynquidSchema typ = do
    typs <- toSynquidSkeleton typ
    typ' <- return $ head typs
    if Set.null $ allTypeVars typ
        then  return $ Monotype typ'
        else return $ foldr ForallT (Monotype typ') $ allTypeVars typ

toSynquidSkeleton :: Type () -> State Int [SType]
toSynquidSkeleton (TyForall _ _ _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyFun _ arg ret) = do
    counter <- get
    put (counter + 1)
    arg' <- toSynquidSkeleton arg
    ret' <- toSynquidSkeleton ret
    return $ [FunctionT ("x"++show counter) (head arg') (head ret')]
toSynquidSkeleton (TyParen _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyKind _ typ _) = toSynquidSkeleton typ
toSynquidSkeleton t@(TyCon _ name) = case name of
    Qual _ moduleName consName -> return [ScalarT (TypeVarT Map.empty ((moduleNameStr moduleName) ++ "." ++ (nameStr consName))) ()]
    UnQual _ name -> case nameStr name of
        "Int" -> return [ScalarT IntT ()]
        "Bool" -> return [ScalarT BoolT ()]
        xarg -> return [ScalarT (TypeVarT Map.empty xarg) ()]
    Special _ _ -> return [ScalarT (TypeVarT Map.empty "_") ()]
toSynquidSkeleton (TyApp _ fun arg) 
    | (TyCon _ name) <- fun = do
        args <- toSynquidSkeleton arg
        case name of
            Qual _ moduleName consName -> return [ScalarT (DatatypeT ((moduleNameStr moduleName) ++ "." ++ (nameStr consName)) args []) ()]
            UnQual _ name -> return [ScalarT (DatatypeT (nameStr name) args []) ()]
            Special _ _ -> return [ScalarT (DatatypeT ("_") args []) ()]
    | otherwise = do
        funs <- toSynquidSkeleton fun
        args <- toSynquidSkeleton arg
        return $ funs ++ args
toSynquidSkeleton (TyVar _ name) = return [ScalarT (TypeVarT Map.empty $ nameStr name) ()]
toSynquidSkeleton (TyList _ typ) = do
    typ' <- toSynquidSkeleton typ
    return [ScalarT (DatatypeT ("List") typ' []) ()]
toSynquidSkeleton (TyTuple _ _ typs) = do
    fst <- toSynquidSkeleton (head typs)
    snd <- toSynquidSkeleton ((!!) typs 1)
    return [ScalarT (DatatypeT ("Pair") (fst++snd) []) ()]
toSynquidSkeleton _ = return [AnyT]