module Database.Convert where

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
import Synquid.Program (refineTop, emptyEnv, BareDeclaration, Environment)
import qualified Synquid.Program as SP
-- import Synquid.Succinct

prependName prefix name  = case name of
    Ident _ var -> Ident () (prefix ++ "." ++ var)
    Symbol _ var -> Symbol () (prefix ++ "." ++ var)

nameStr name = case name of
    Ident _ var -> var
    Symbol _ sym -> sym

isIdentity (Ident _ _) = True
isIdentity (Symbol _ _) = False

moduleNameStr (ModuleName _ name) = name

consStr (TyCon _ name) = case name of
    Qual _ moduleName consName -> (moduleNameStr moduleName) ++ "." ++ (nameStr consName)
    UnQual _ name -> nameStr name
    Special _ name -> specialConsStr name
consStr (TyApp _ fun arg) = consStr fun
consStr (TyFun _ arg ret) = (consStr arg) ++ "To" ++ (consStr ret)
consStr (TyList _ typ) = "List"++(consStr typ)
consStr _ = "_"

specialConsStr (UnitCon _) = "Unit"
specialConsStr (ListCon _) = "List"
specialConsStr (FunCon _) = "Fun"
specialConsStr _ = "_"

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

datatypeOf (TyForall _ _ _ typ) = datatypeOf typ
datatypeOf (TyFun _ arg ret) = datatypeOf arg `Set.union` datatypeOf ret
datatypeOf (TyTuple _ _ typs) = foldr (\t vars -> vars `Set.union` datatypeOf t) (Set.singleton "Pair") typs
datatypeOf (TyUnboxedSum _ typs) = foldr (\t vars -> vars `Set.union` datatypeOf t) Set.empty typs
datatypeOf (TyList _ typs) = Set.singleton "List" `Set.union` datatypeOf typs
datatypeOf (TyApp _ fun arg) = datatypeOf fun `Set.union` datatypeOf arg
datatypeOf (TyVar _ name) = Set.empty
datatypeOf t@(TyCon _ name) = Set.singleton $ consStr t
datatypeOf (TyParen _ typ) = datatypeOf typ
datatypeOf (TyInfix _ typ1 _ typ2) = datatypeOf typ1 `Set.union` datatypeOf typ2
datatypeOf (TyKind _ typ _) = datatypeOf typ
datatypeOf (TyPromoted _ _) = Set.empty
datatypeOf (TyEquals _ ltyp rtyp) = datatypeOf ltyp `Set.union` datatypeOf rtyp
datatypeOf _ = Set.empty

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
    Qual _ moduleName consName -> return [ScalarT (DatatypeT ((moduleNameStr moduleName) ++ "." ++ (nameStr consName)) [] []) ()]
    UnQual _ name -> case nameStr name of
        "Int" -> return [ScalarT IntT ()]
        "Bool" -> return [ScalarT BoolT ()]
        xarg -> return [ScalarT (DatatypeT xarg [] []) ()]
    Special _ name -> return [ScalarT (DatatypeT (specialConsStr name) [] []) ()]
toSynquidSkeleton (TyApp _ fun arg) 
    | (TyCon _ name) <- fun = do
        ScalarT (DatatypeT id tys _) _ <- head <$> toSynquidSkeleton fun
        args <- toSynquidSkeleton arg
        return [ScalarT (DatatypeT id (args++tys) []) ()]
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
    snd <- toSynquidSkeleton (typs !! 1)
    return [ScalarT (DatatypeT ("Pair") (fst++snd) []) ()]
toSynquidSkeleton _ = return [AnyT]

varsFromBind (KindedVar _ name _) = nameStr name
varsFromBind (UnkindedVar _ name) = nameStr name

decomposeDH :: DeclHead () -> (Maybe String, [String])
decomposeDH (DHead _ name) = (Just $ nameStr name, [])
decomposeDH (DHInfix _ varBind name) = (Nothing, [varsFromBind varBind]++(if isIdentity name then [nameStr name] else []))
decomposeDH (DHParen _ dh) = decomposeDH dh
decomposeDH (DHApp _ funHead varBind) = let (name, vars) = decomposeDH funHead in (name, (varsFromBind varBind):vars)

toSynquidRType env typ = do
    typ' <- toSynquidSkeleton typ
    return $ refineTop env $ head typ'
toSynquidRSchema env (ForallT name sch) = ForallT name (toSynquidRSchema env sch)
toSynquidRSchema env (Monotype typ) = Monotype (refineTop env typ)

processConDecls :: Environment -> [QualConDecl ()] -> State Int [SP.ConstructorSig]
processConDecls env [] = return []
processConDecls env (decl:decls) = let QualConDecl _ _ _ conDecl = decl in 
    case conDecl of
        ConDecl _ name typs -> do
            typ <- toSynquidRType env $ head typs
            (:) (SP.ConstructorSig (nameStr name) typ) <$> (processConDecls env decls)
        InfixConDecl _ typl name typr -> do
            typl' <- toSynquidRType env typl
            typr' <- toSynquidRType env typr
            (:) (SP.ConstructorSig (nameStr name) (FunctionT "arg0" typl' typr')) <$> (processConDecls env decls)
        RecDecl _ name fields -> error "record declaration is not supported"

toSynquidDecl env (TypeDecl _ head typ) = case decomposeDH head of
    (Nothing, vars) -> error "is this possible?"
    (Just hd, vars) -> (SP.TypeDecl hd vars) <$> toSynquidRType env typ
toSynquidDecl env (DataDecl _ _ _ head conDecls _) = case decomposeDH head of
    (Nothing, _) -> error "No data name"
    (Just hd, vars) -> do
        constructors <- processConDecls env conDecls
        return $ SP.DataDecl hd vars [] constructors
toSynquidDecl env (TypeSig _ names typ) = do
    sch <- toSynquidSchema typ
    return $ SP.FuncDecl (head (map nameStr names)) (toSynquidRSchema env sch)
toSynquidDecl env decl = return $ SP.QualifierDecl [] -- [TODO] a fake conversion