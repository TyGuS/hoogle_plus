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
import Data.Ord
import qualified Data.Sort as Sort
import Data.List.Split
import Control.Lens as Lens
import Control.Monad.State
import Text.Parsec.Pos
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Package
import Debug.Trace

import Synquid.Type
import Synquid.Logic
import Synquid.Program (emptyEnv, BareDeclaration, Declaration, Environment)
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
datatypeOf t@(TyCon name) | Special _ <- name = Set.empty
datatypeOf t@(TyCon name) | otherwise = Set.singleton $ consStr t
datatypeOf (TyParen typ) = datatypeOf typ
datatypeOf (TyInfix typ1 _ typ2) = datatypeOf typ1 `Set.union` datatypeOf typ2
datatypeOf (TyKind typ _) = datatypeOf typ
datatypeOf (TyEquals ltyp rtyp) = datatypeOf ltyp `Set.union` datatypeOf rtyp
datatypeOf _ = Set.empty

matchDtWithCons :: [Entry] -> [Entry]
matchDtWithCons [] = []
matchDtWithCons (decl:decls) = case decl of
    EDecl (DataDecl a b c name bvars conDecls d) -> case decls of
        [] -> decl : matchDtWithCons decls
        decl':decls' | EDecl (TypeSig _ names typ) <- decl' -> if names !! 0 == name 
                                                                   then let conDecl = QualConDecl a [] c (ConDecl name [typ]) 
                                                                        in (EDecl (DataDecl a b c name bvars (conDecl:conDecls) d)): matchDtWithCons decls'
                                                                   else decl : matchDtWithCons decls
                     | otherwise -> decl : matchDtWithCons decls
    _ -> decl : matchDtWithCons decls


toSynquidSchema :: Monad m => Type -> StateT Int m SSchema
toSynquidSchema typ = do
    typs <- toSynquidSkeleton typ
    typ' <- return $ head typs
    return $ Monotype typ'
    -- if Set.null $ allTypeVars typ
        -- then  return $ Monotype typ'
        -- else return $ foldr ForallT (Monotype typ') $ allTypeVars typ

toSynquidSkeleton :: Monad m => Type -> StateT Int m [SType]
toSynquidSkeleton (TyForall _ _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyFun arg ret) = do
    counter <- get
    -- traceShow ((show counter)) $ return ()
    put (counter + 1)
    ret' <- toSynquidSkeleton ret
    arg' <- toSynquidSkeleton arg
    return [FunctionT ("arg"++show counter) (head arg') (head ret')]
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
    | (TyApp fun' arg') <- fun = do
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

-- | Add true as the refinement to convert all types into RType
addTrue (ScalarT (DatatypeT name tArgs pArgs) _) = ScalarT (DatatypeT name (map addTrue tArgs) []) ftrue
addTrue (ScalarT IntT _) = ScalarT IntT ftrue
addTrue (ScalarT BoolT _) = ScalarT BoolT ftrue
addTrue (ScalarT (TypeVarT vSubst a) _) = ScalarT (TypeVarT vSubst a) ftrue
addTrue (FunctionT x tArg tFun) = FunctionT x (addTrue tArg) (addTrue tFun)

toSynquidRType typ = do
    typ' <- toSynquidSkeleton typ
    return $ addTrue $ head typ'
-- toSynquidRSchema env (ForallT name sch) = ForallT name (toSynquidRSchema env sch)
toSynquidRSchema (Monotype typ) = Monotype $ addTrue typ

processConDecls :: Monad m => [QualConDecl] -> StateT Int m [SP.ConstructorSig]
processConDecls [] = return []
processConDecls (decl:decls) = let QualConDecl _ _ _ conDecl = decl in 
    case conDecl of
        ConDecl name typs -> do
            typ <- toSynquidRType $ head typs
            (:) (SP.ConstructorSig (nameStr name) typ) <$> (processConDecls decls)
        InfixConDecl typl name typr -> do
            typl' <- toSynquidRType typl
            typr' <- toSynquidRType typr
            (:) (SP.ConstructorSig (nameStr name) (FunctionT "arg0" typl' typr')) <$> (processConDecls decls)
        RecDecl name fields -> error "record declaration is not supported"

datatypeOfCon :: [QualConDecl] -> Set Id
datatypeOfCon [] = Set.empty
datatypeOfCon (decl:decls) = let QualConDecl _ _ _ conDecl = decl in
    case conDecl of
        ConDecl name typs -> Set.unions $ map datatypeOf typs
        InfixConDecl typl name typr -> datatypeOf typl `Set.union` datatypeOf typr
        RecDecl name fields -> error "record declaration is not supported"

toSynquidDecl (EDecl (TypeDecl _ name bvars typ)) = Pos (initialPos (nameStr name)) . SP.TypeDecl (nameStr name) (map varsFromBind bvars) <$> toSynquidRType typ
toSynquidDecl (EDecl (DataDecl _ _ _ name bvars conDecls _)) = do
    constructors <- processConDecls conDecls
    let vars = map varsFromBind bvars
    return $ Pos (initialPos (nameStr name)) $ SP.DataDecl (nameStr name) vars [] constructors
toSynquidDecl (EDecl (TypeSig _ names typ)) = do
    sch <- toSynquidSchema typ
    return $ Pos (initialPos (nameStr $ names !! 0)) $ SP.FuncDecl (head (map nameStr names)) (toSynquidRSchema sch)
toSynquidDecl decl = return $ Pos (initialPos "") $ SP.QualifierDecl [] -- [TODO] a fake conversion

reorderDecls :: [Declaration] -> [Declaration]
reorderDecls decls = Sort.sortOn toInt decls
  where
    toInt (Pos _ (SP.TypeDecl {})) = 0
    toInt (Pos _ (SP.DataDecl {})) = 0
    toInt (Pos _ (SP.QualifierDecl {})) = 98
    toInt (Pos _ (SP.FuncDecl {})) = 99
    toInt (Pos _ (SP.SynthesisGoal {})) = 100

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

readDeclarations :: PkgName -> Maybe Version -> IO [Entry]
readDeclarations pkg version = do
    vpkg <- do 
        case version of
            Nothing -> return pkg
            Just v -> ifM (checkVersion pkg v) (return $ pkg ++ "-" ++ v) (return pkg)
    s   <- readFile $ downloadDir ++ vpkg ++ ".txt"
    let code = concat . rights . (map parseLine) $ splitOn "\n" s
    return $ renameSigs "" code

type DependsOn = Map PkgName [Id]

packageDependencies :: PkgName -> StateT Int IO [Declaration]
packageDependencies pkg = do
    gPackageDesc <- lift $ readPackageDescription verbose $ downloadDir ++ pkg ++ ".cabal"
    case condLibrary gPackageDesc of
        Nothing -> return []
        Just (CondNode _ dependencies _) -> do
            let dpkgs = map dependentPkg dependencies
            mapM_ (\fname -> lift $ downloadFile fname Nothing >> downloadCabal fname Nothing) dpkgs
            myDts <- lift $ packageDtNames pkg
            myDtDefs <- lift $ packageDtDefs pkg
            myDefinedDts <- lift $ definedDts pkg
            theirDts <- lift $ mapM packageDtDefs dpkgs
            decls <- dependencyClosure myDefinedDts myDts theirDts
            let allDecls = decls ++ (snd $ unzip myDtDefs)
            let sortedIds = topoSort $ dependencyGraph allDecls
            let decls' = matchDtWithCons $ map (fromJust . (flip Map.lookup $ declMap allDecls)) $ nub $ sortedIds >.> ["List", "Pair"]
            mapM toSynquidDecl decls'
            
            -- let foreignDts = concatMap (filter ((flip elem (myDts >.> myDefinedDts)) . fst)) theirDts
            -- -- lift $ print foreignDts
            -- mapM toSynquidDecl $ nub $ snd $ unzip foreignDts
  where
    dependentPkg (Dependency name _) = unPackageName name
    dependencyClosure definedDts allDts theirDts = do
        let undefinedDts = allDts >.> definedDts
        if length undefinedDts /= 0 
            then do
                let foreignDts = concatMap (filter ((flip elem undefinedDts) . fst)) theirDts
                let newDecls = nub $ snd $ unzip foreignDts
                let newAddedDts = Set.toList $ Set.unions $ map getDeclTy newDecls
                (++) newDecls <$> dependencyClosure allDts newAddedDts theirDts
            else return []
    declMap decls = foldr (\d -> Map.insert (getDeclName d) d) Map.empty $ filter isDataDecl decls
    dependsOn decl = case decl of
        EDecl (DataDecl _ _ _ name bvars conDecls _) -> (nameStr name, datatypeOfCon conDecls)
        EDecl (TypeDecl _ name _ ty) -> (nameStr name, datatypeOf ty)
        _ -> error "[In `dependsOn`] Please filter before calling this function"
    dependencyGraph decls = foldr (uncurry Map.insert) Map.empty $ map dependsOn $ filter isDataDecl decls
    nodesOf graph = nub $ (Map.keys graph) ++ (Set.toList $ Set.unions $ Map.elems graph)
    topoSort graph = reverse $ topoSortHelper (nodesOf graph) Set.empty graph
    topoSortHelper [] _ graph = []
    topoSortHelper (v:vs) visited graph = if Set.member v visited
        then topoSortHelper vs visited graph
        else topoSortHelper vs (Set.insert v visited) graph ++ v:(topoSortHelper (Set.toList (Map.findWithDefault Set.empty v graph)) visited graph)

isDataDecl :: Entry -> Bool
isDataDecl decl = case decl of
    EDecl (DataDecl {}) -> True
    EDecl (TypeDecl {}) -> True
    _ -> False

packageDtDefs :: PkgName -> IO [(Id, Entry)]
packageDtDefs pkg = do
    decls <- readDeclarations pkg Nothing
    -- It relies on the order of definitions exist in the source file
    return $ foldr (\decl -> ((dtNameOf decl, decl):)) [] $ filter isDataDecl decls
  where
    dtNameOf (EDecl (DataDecl _ _ _ name bvars conDecls _)) = nameStr name
    dtNameOf (EDecl (TypeDecl _ name _ _)) = nameStr name

getDeclTy :: Entry -> Set Id
getDeclTy (EDecl (TypeSig _ names ty)) = datatypeOf ty
getDeclTy (EDecl (TypeDecl _ _ _ ty)) = datatypeOf ty
getDeclTy _ = Set.empty

getDeclName :: Entry -> Id
getDeclName (EDecl (DataDecl _ _ _ name bvars conDecls _)) = nameStr name
getDeclName (EDecl (TypeDecl _ name _ ty)) = nameStr name

packageDtNames :: PkgName -> IO [Id]
packageDtNames pkg = do
    decls <- readDeclarations pkg Nothing
    return $ Set.toList $ Set.unions $ map getDeclTy decls

definedDts :: PkgName -> IO [Id]
definedDts pkg = do
    decls <- readDeclarations pkg Nothing
    let dtDecls = filter isDataDecl decls
    return $ map dtNameOf dtDecls
  where
    dtNameOf (EDecl (DataDecl _ _ _ name bvars conDecls _)) = nameStr name
    dtNameOf (EDecl (TypeDecl _ name _ _)) = nameStr name