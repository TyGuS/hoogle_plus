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
import Data.Either
import Data.List.Split
import Control.Lens as Lens
import Control.Monad.State
import Text.Parsec.Pos

import Synquid.Type
import Synquid.Program (refineTop, emptyEnv, BareDeclaration, Environment)
import qualified Synquid.Program as SP
import Synquid.Util
import Synquid.Error
import Database.Generate
import Database.Util
import Database.Download
-- import Synquid.Succinct

prependName prefix name  = case name of
    Ident var -> Ident (prefix ++ "." ++ var)
    Symbol var -> Symbol (prefix ++ "." ++ var)

nameStr name = case name of
    Ident var -> var
    Symbol sym -> sym

isIdentity (Ident _) = True
isIdentity (Symbol _) = False

moduleNameStr (ModuleName name) = name

consStr (TyCon name) = case name of
    Qual moduleName consName -> (moduleNameStr moduleName) ++ "." ++ (nameStr consName)
    UnQual name -> nameStr name
    Special name -> specialConsStr name
consStr (TyApp fun arg) = consStr fun
consStr (TyFun arg ret) = (consStr arg) ++ "To" ++ (consStr ret)
consStr (TyList typ) = "List"++(consStr typ)
consStr _ = "_"

specialConsStr (UnitCon) = "Unit"
specialConsStr (ListCon) = "List"
specialConsStr (FunCon) = "Fun"
specialConsStr _ = "_"

allTypeVars (TyForall _ _ typ) = allTypeVars typ
allTypeVars (TyFun arg ret) = allTypeVars arg `Set.union` allTypeVars ret
allTypeVars (TyTuple _ typs) = foldr (\t vars -> vars `Set.union` allTypeVars t) Set.empty typs
-- allTypeVars (TyUnboxedSum _ typs) = foldr (\t vars -> vars `Set.union` allTypeVars t) Set.empty typs
allTypeVars (TyList typ) = allTypeVars typ
allTypeVars (TyApp fun arg) = allTypeVars fun `Set.union` allTypeVars arg
allTypeVars (TyVar name) = Set.singleton $ nameStr name
allTypeVars (TyCon name) = Set.empty
allTypeVars (TyParen typ) = allTypeVars typ
allTypeVars (TyInfix typ1 _ typ2) = allTypeVars typ1 `Set.union` allTypeVars typ2
allTypeVars (TyKind typ _) = allTypeVars typ
allTypeVars (TyEquals ltyp rtyp) = allTypeVars ltyp `Set.union` allTypeVars rtyp
allTypeVars _ = Set.empty

datatypeOf (TyForall _ _ typ) = datatypeOf typ
datatypeOf (TyFun arg ret) = datatypeOf arg `Set.union` datatypeOf ret
datatypeOf (TyTuple _ typs) = foldr (\t vars -> vars `Set.union` datatypeOf t) (Set.singleton "Pair") typs
-- datatypeOf (TyUnboxedSum _ typs) = foldr (\t vars -> vars `Set.union` datatypeOf t) Set.empty typs
datatypeOf (TyList typ) = Set.singleton "List" `Set.union` datatypeOf typ
datatypeOf (TyApp fun arg) = datatypeOf fun `Set.union` datatypeOf arg
datatypeOf (TyVar name) = Set.empty
datatypeOf t@(TyCon name) = Set.singleton $ consStr t
datatypeOf (TyParen typ) = datatypeOf typ
datatypeOf (TyInfix typ1 _ typ2) = datatypeOf typ1 `Set.union` datatypeOf typ2
datatypeOf (TyKind typ _) = datatypeOf typ
datatypeOf (TyEquals ltyp rtyp) = datatypeOf ltyp `Set.union` datatypeOf rtyp
datatypeOf _ = Set.empty

toSynquidSchema :: Type -> State Int SSchema
toSynquidSchema typ = do
    typs <- toSynquidSkeleton typ
    typ' <- return $ head typs
    if Set.null $ allTypeVars typ
        then  return $ Monotype typ'
        else return $ foldr ForallT (Monotype typ') $ allTypeVars typ

toSynquidSkeleton :: Type -> State Int [SType]
toSynquidSkeleton (TyForall _ _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyFun arg ret) = do
    counter <- get
    put (counter + 1)
    arg' <- toSynquidSkeleton arg
    ret' <- toSynquidSkeleton ret
    return $ [FunctionT ("x"++show counter) (head arg') (head ret')]
toSynquidSkeleton (TyParen typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyKind typ _) = toSynquidSkeleton typ
toSynquidSkeleton t@(TyCon name) = case name of
    Qual moduleName consName -> return [ScalarT (DatatypeT ((moduleNameStr moduleName) ++ "." ++ (nameStr consName)) [] []) ()]
    UnQual name -> case nameStr name of
        "Int" -> return [ScalarT IntT ()]
        "Bool" -> return [ScalarT BoolT ()]
        xarg -> return [ScalarT (DatatypeT xarg [] []) ()]
    Special name -> return [ScalarT (DatatypeT (specialConsStr name) [] []) ()]
toSynquidSkeleton (TyApp fun arg) 
    | (TyCon name) <- fun = do
        ScalarT (DatatypeT id tys _) _ <- head <$> toSynquidSkeleton fun
        args <- toSynquidSkeleton arg
        return [ScalarT (DatatypeT id (args++tys) []) ()]
    | otherwise = do
        funs <- toSynquidSkeleton fun
        args <- toSynquidSkeleton arg
        return $ funs ++ args
toSynquidSkeleton (TyVar name) = return [ScalarT (TypeVarT Map.empty $ nameStr name) ()]
toSynquidSkeleton (TyList typ) = do
    typ' <- toSynquidSkeleton typ
    return [ScalarT (DatatypeT ("List") typ' []) ()]
toSynquidSkeleton (TyTuple _ typs) = do
    fst <- toSynquidSkeleton (head typs)
    snd <- toSynquidSkeleton (typs !! 1)
    return [ScalarT (DatatypeT ("Pair") (fst++snd) []) ()]
toSynquidSkeleton _ = return [AnyT]

varsFromBind (KindedVar name _) = nameStr name
varsFromBind (UnkindedVar name) = nameStr name

-- decomposeDH :: DeclHead () -> (Maybe String, [String])
-- decomposeDH (DHead _ name) = (Just $ nameStr name, [])
-- decomposeDH (DHInfix _ varBind name) = (Nothing, [varsFromBind varBind]++(if isIdentity name then [nameStr name] else []))
-- decomposeDH (DHParen _ dh) = decomposeDH dh
-- decomposeDH (DHApp _ funHead varBind) = let (name, vars) = decomposeDH funHead in (name, (varsFromBind varBind):vars)

toSynquidRType env typ = do
    typ' <- toSynquidSkeleton typ
    return $ refineTop env $ head typ'
toSynquidRSchema env (ForallT name sch) = ForallT name (toSynquidRSchema env sch)
toSynquidRSchema env (Monotype typ) = Monotype (refineTop env typ)

processConDecls :: Environment -> [QualConDecl] -> State Int [SP.ConstructorSig]
processConDecls env [] = return []
processConDecls env (decl:decls) = let QualConDecl _ _ _ conDecl = decl in 
    case conDecl of
        ConDecl name typs -> do
            typ <- toSynquidRType env $ head typs
            (:) (SP.ConstructorSig (nameStr name) typ) <$> (processConDecls env decls)
        InfixConDecl typl name typr -> do
            typl' <- toSynquidRType env typl
            typr' <- toSynquidRType env typr
            (:) (SP.ConstructorSig (nameStr name) (FunctionT "arg0" typl' typr')) <$> (processConDecls env decls)
        RecDecl name fields -> error "record declaration is not supported"

toSynquidDecl env (EDecl (TypeDecl _ name bvars typ)) = Pos (initialPos (nameStr name)) . SP.TypeDecl (nameStr name) (map varsFromBind bvars) <$> toSynquidRType env typ
toSynquidDecl env (EDecl (DataDecl _ _ _ name bvars conDecls _)) = do
    constructors <- processConDecls env conDecls
    let vars = map varsFromBind bvars
    return $ Pos (initialPos (nameStr name)) $ SP.DataDecl (nameStr name) vars [] constructors
toSynquidDecl env (EDecl (TypeSig _ names typ)) = do
    sch <- toSynquidSchema typ
    return $ Pos (initialPos (nameStr $ names !! 0)) $ SP.FuncDecl (head (map nameStr names)) (toSynquidRSchema env sch)
toSynquidDecl env decl = return $ Pos (initialPos "") $ SP.QualifierDecl [] -- [TODO] a fake conversion


renameSigs :: String -> [Entry] -> [Entry]
renameSigs _ [] = []
renameSigs currModule (decl:decls) = case decl of
    EModule mdl -> decl:(renameSigs mdl decls)
    EPackage _ -> decl:(renameSigs currModule decls)
    EDecl (TypeSig loc names ty) -> (EDecl (TypeSig loc (map (prependName currModule) names) ty)):(renameSigs currModule decls)
    _ -> decl:(renameSigs currModule decls)

addSynonym :: [Entry] -> [Entry]
addSynonym [] = []
addSynonym (decl:decls) = case decl of
    EDecl (TypeDecl loc name _ typ) -> let typ' = TyFun (TyCon (UnQual name)) typ
        in (EDecl (TypeSig loc [Ident ((nameStr name)++"To"++(consStr typ))] typ')):(addSynonym decls)
    _ -> decl:(addSynonym decls)

readDeclations :: PkgName -> Maybe Version -> IO [Entry]
readDeclations pkg version = do
    vpkg <- do 
        case version of
            Nothing -> return pkg
            Just v -> ifM (checkVersion pkg v) (return $ pkg ++ "-" ++ v) (return pkg)
    s   <- readFile $ downloadDir ++ vpkg ++ ".txt"
    let code = concat . rights . (map parseLine) $ splitOn "\n" s
    return $ renameSigs "" $ addSynonym code