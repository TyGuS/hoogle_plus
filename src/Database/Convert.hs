module Database.Convert where

import Language.Haskell.Exts hiding (PApp)
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
import Data.Foldable
import qualified Data.Sort as Sort
import Data.List.Split
import Control.Monad.State
import Text.Parsec.Pos
import Distribution.Verbosity
import Distribution.PackageDescription hiding (Var)
import Distribution.PackageDescription.Parsec
import Distribution.Package
import System.Directory
import System.IO

import Database.Download
import Database.Generate
import Database.Util
import Synquid.Error
import Synquid.Logic hiding (Var)
import Synquid.Type
import Synquid.Util
import Types.Common
import Types.Generate
import Types.Environment
import Types.Program (BareDeclaration, Declaration, BareProgram(..), UProgram, Program(..))
import Types.Type
import qualified Types.Program as TP

prependName prefix name  = case name of
    Ident l var -> Ident l (prefix ++ "." ++ var)
    Symbol l var -> Ident l ("(" ++ prefix ++ "." ++ var ++ ")")

nameStr name = case name of
    Ident _ var -> var
    Symbol _ sym -> sym

isIdentity (Ident _ _) = True
isIdentity (Symbol _ _) = False

moduleNameStr (ModuleName _ name) = name

declHeadName (DHead _ name) = nameStr name
declHeadName (DHInfix _ bvar name) = nameStr name
declHeadName (DHParen _ head) = declHeadName head
declHeadName (DHApp _ head _) = declHeadName head

declHeadVars (DHead _ _) = []
declHeadVars (DHInfix _ bvar name) = [varsFromBind bvar]
declHeadVars (DHParen _ head) = declHeadVars head
declHeadVars (DHApp _ head bvar) = varsFromBind bvar : (declHeadVars head)

qnameStr name = case name of
    Qual _ moduleName consName -> (moduleNameStr moduleName) ++ "." ++ (nameStr consName)
    UnQual _ name -> nameStr name
    Special _ name -> specialConsStr name

consStr (TyCon _ name) = qnameStr name
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
-- allTypeVars (TyUnboxedSum _ typs) = foldr (\t vars -> vars `Set.union` allTypeVars t) Set.empty typs
allTypeVars (TyList _ typ) = allTypeVars typ
allTypeVars (TyApp _ fun arg) = allTypeVars fun `Set.union` allTypeVars arg
allTypeVars (TyVar _ name) = Set.singleton $ nameStr name
allTypeVars (TyCon _ name) = Set.empty
allTypeVars (TyParen _ typ) = allTypeVars typ
allTypeVars (TyInfix _ typ1 _ typ2) = allTypeVars typ1 `Set.union` allTypeVars typ2
allTypeVars (TyKind _ typ _) = allTypeVars typ
allTypeVars (TyEquals _ ltyp rtyp) = allTypeVars ltyp `Set.union` allTypeVars rtyp
allTypeVars _ = Set.empty

datatypeOf (TyForall _ _ _ typ) = datatypeOf typ
datatypeOf (TyFun _ arg ret) = datatypeOf arg `Set.union` datatypeOf ret
datatypeOf (TyTuple _ _ typs) = foldr (\t vars -> vars `Set.union` datatypeOf t) (Set.singleton "Pair") typs
-- datatypeOf (TyUnboxedSum _ typs) = foldr (\t vars -> vars `Set.union` datatypeOf t) Set.empty typs
datatypeOf (TyList _ typ) = Set.singleton "List" `Set.union` datatypeOf typ
datatypeOf (TyApp _ fun arg) = datatypeOf fun `Set.union` datatypeOf arg
datatypeOf (TyVar _ name) = Set.empty
datatypeOf t@(TyCon _ name) | Special _ _ <- name = Set.empty
datatypeOf t@(TyCon _ name) | otherwise = Set.singleton $ consStr t
datatypeOf (TyParen _ typ) = datatypeOf typ
datatypeOf (TyInfix _ typ1 _ typ2) = datatypeOf typ1 `Set.union` datatypeOf typ2
datatypeOf (TyKind _ typ _) = datatypeOf typ
datatypeOf (TyEquals _ ltyp rtyp) = datatypeOf ltyp `Set.union` datatypeOf rtyp
datatypeOf _ = Set.empty

matchDtWithCons :: [Entry] -> [Entry]
matchDtWithCons [] = []
matchDtWithCons (decl:decls) = case decl of
    EDecl (DataDecl a b c hd conDecls d) -> case decls of
        [] -> decl : matchDtWithCons decls
        decl':decls' | EDecl (TypeSig _ names typ) <- decl' -> if nameStr (head names) == declHeadName hd
                                                                   then let conDecl = QualConDecl a Nothing c (ConDecl a (head names) [typ])
                                                                        in (EDecl (DataDecl a b c hd (conDecl:conDecls) d)): matchDtWithCons decls'
                                                                   else decl : matchDtWithCons decls
                     | otherwise -> decl : matchDtWithCons decls
    _ -> decl : matchDtWithCons decls

resolveContext :: (MonadIO m) => Context () -> StateT Int m [(Id, [Id])]
resolveContext (CxSingle _ asst) = resolveAsst asst
resolveContext (CxTuple _ assts) = groupTuples . concat <$> mapM resolveAsst assts
resolveContext (CxEmpty _)       = return []

resolveAsst :: (MonadIO m) => Asst () -> StateT Int m [(Id, [Id])]
resolveAsst a@(ClassA _ qname typs) = if Set.null tyVars then return [] else return [(Set.findMin tyVars, [qnameStr qname])]
  where
    tyVars = Set.unions $ map allTypeVars typs
resolveAsst (ParenA _ asst) = resolveAsst asst
resolveAsst a = error $ "Unknown " ++ show a

toSynquidSchema :: (MonadIO m) => Type () -> StateT Int m (Maybe SSchema)
toSynquidSchema (TyForall _ _ (Just ctx) typ) = do -- if this type has some context
    typs <- toSynquidSkeleton typ
    case typs of
        [] -> return Nothing
        _  -> do
            let typ' = head typs
            return (Just (Monotype typ'))
            -- classQuals <- resolveContext ctx
            -- return $ Just $ foldr ForallT (Monotype typ') classQuals
toSynquidSchema typ = do
    typs <- toSynquidSkeleton typ
    case typs of
        [] -> return Nothing
        _  -> return $ Just . Monotype $ head typs

toSynquidSkeleton :: (MonadIO m) => Type () -> StateT Int m [SType]
toSynquidSkeleton t@(TyForall _ _ _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyFun _ arg ret) = do
    counter <- get
    -- traceShow ((show counter)) $ return ()
    put (counter + 1)
    ret' <- toSynquidSkeleton ret
    arg' <- toSynquidSkeleton arg
    if null ret' || null arg' then return []
                              else return [FunctionT ("arg"++show counter) (head arg') (head ret')]
toSynquidSkeleton (TyParen _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyKind _ typ _) = toSynquidSkeleton typ
toSynquidSkeleton t@(TyCon _ name) = case name of
    Qual _ moduleName consName -> return [ScalarT (DatatypeT ((moduleNameStr moduleName) ++ "." ++ (nameStr consName)) [] []) ()]
    UnQual _ name -> return [ScalarT (DatatypeT (nameStr name) [] []) ()]
    Special _ name -> return [ScalarT (DatatypeT (specialConsStr name) [] []) ()]
toSynquidSkeleton (TyApp _ fun arg)
    | (TyCon _ name) <- fun = do
        ScalarT (DatatypeT id tys _) _ <- head <$> toSynquidSkeleton fun
        args <- toSynquidSkeleton arg
        return [ScalarT (DatatypeT id (tys ++ args) []) ()]
    | (TyApp _ fun' arg') <- fun = do
        ScalarT (DatatypeT id tys _) _ <- head <$> toSynquidSkeleton fun
        args <- toSynquidSkeleton arg
        return [ScalarT (DatatypeT id (tys ++ args) []) ()]
    | (TyVar _ _) <- fun = return [] -- this is a wrapped type variable, do not support now
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
toSynquidSkeleton t = return [] -- error $ "Unhandled case " ++ show t

varsFromBind (KindedVar _ name _) = nameStr name
varsFromBind (UnkindedVar _ name) = nameStr name

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
addTrue AnyT = AnyT

toSynquidRType :: (MonadIO m) => Type () -> StateT Int m RType
toSynquidRType typ = do
    typ' <- toSynquidSkeleton typ
    if null typ' then return AnyT else (return $ addTrue $ head typ')

toSynquidRSchema :: SSchema -> RSchema
toSynquidRSchema (Monotype typ) = Monotype $ addTrue typ
toSynquidRSchema (ForallT a typ) = ForallT a (toSynquidRSchema typ)

addPrelude :: [Entry] -> [Entry]
addPrelude [] = []
addPrelude (decl:decls) = case decl of
    EModule mdl -> if mdl == "GHC.OldList" then takeWhile (not . isModule) decls else addPrelude decls
    _ -> addPrelude decls

processConDecls :: (MonadIO m) => [QualConDecl ()] -> StateT Int m [TP.ConstructorSig]
processConDecls [] = return []
processConDecls (decl:decls) = let QualConDecl _ _ _ conDecl = decl in
    case conDecl of
        ConDecl _ name typs -> do
            typ <- toSynquidRType $ head typs
            if hasAny typ then processConDecls decls
                          else (:) (TP.ConstructorSig (nameStr name) typ) <$> (processConDecls decls)
        InfixConDecl _ typl name typr -> do
            typl' <- toSynquidRType typl
            typr' <- toSynquidRType typr
            if hasAny typl' || hasAny typr'
                then processConDecls decls
                else (:) (TP.ConstructorSig (nameStr name) (FunctionT "arg0" typl' typr')) <$> (processConDecls decls)
        RecDecl _ name fields -> error "record declaration is not supported"

datatypeOfCon :: [QualConDecl ()] -> Set Id
datatypeOfCon [] = Set.empty
datatypeOfCon (decl:decls) = let QualConDecl _ _ _ conDecl = decl in
    case conDecl of
        ConDecl _ name typs -> Set.unions $ map datatypeOf typs
        InfixConDecl _ typl name typr -> datatypeOf typl `Set.union` datatypeOf typr
        RecDecl _ name fields -> error "record declaration is not supported"

toSynquidDecl :: (MonadIO m) => Entry -> StateT Int m Declaration
toSynquidDecl (EDecl (TypeDecl _ head typ)) = do
    typ' <- toSynquidRType typ
    if hasAny typ' then return $ Pos (initialPos "") $ TP.QualifierDecl [] -- a fake conversion
                   else return $ Pos (initialPos $ declHeadName head) $ TP.TypeDecl (declHeadName head) (declHeadVars head) typ'
toSynquidDecl (EDecl (DataFamDecl a b head c)) = toSynquidDecl (EDecl (DataDecl a (DataType a) b head [] []))
toSynquidDecl (EDecl (DataDecl _ _ _ head conDecls _)) = do
    constructors <- processConDecls conDecls
    let name = declHeadName head
    let vars = declHeadVars head
    return $ Pos (initialPos name) $ TP.DataDecl name vars [] constructors
toSynquidDecl (EDecl (TypeSig _ names typ)) = do
    maybeSch <- toSynquidSchema typ
    case maybeSch of
        Nothing  -> return $ Pos (initialPos "") $ TP.QualifierDecl [] -- a fake conversion
        Just sch -> return $ Pos (initialPos (nameStr $ names !! 0)) $ TP.FuncDecl (nameStr $ head names) (toSynquidRSchema sch)
toSynquidDecl decl = return $ Pos (initialPos "") $ TP.QualifierDecl [] -- [TODO] a fake conversion


reorderDecls :: [Declaration] -> [Declaration]
reorderDecls decls = Sort.sortOn toInt decls
  where
    toInt (Pos _ (TP.TypeDecl "String" _ _)) = 1
    toInt (Pos _ (TP.TypeDecl _ [] _)) = 2
    toInt (Pos _ (TP.TypeDecl _ _ _)) = 3
    toInt (Pos _ (TP.DataDecl "List" _ _ _)) = 0
    toInt (Pos _ (TP.DataDecl "Char" _ _ _)) = 0
    toInt (Pos _ (TP.DataDecl _ [] _ _)) = 2
    toInt (Pos _ (TP.DataDecl _ _ _ _)) = 3
    toInt (Pos _ (TP.QualifierDecl {})) = 98
    toInt (Pos _ (TP.FuncDecl {})) = 99
    toInt (Pos _ (TP.SynthesisGoal {})) = 100

renameSigs :: String -> [Entry] -> Map Id [Entry]
renameSigs _ [] = Map.empty
renameSigs currModule (decl:decls) = case decl of
    EModule mdl -> Map.insertWith (++) mdl [decl] (renameSigs mdl decls)
    EPackage _ -> renameSigs currModule decls
    EDecl (TypeSig loc names ty) -> Map.insertWith (++) currModule [EDecl (TypeSig loc (map (prependName currModule) names) ty)] (renameSigs currModule decls)
    _ -> Map.insertWith (++) currModule [decl] (renameSigs currModule decls)

readDeclarations :: PkgName -> Maybe Version -> IO (Map Id [Entry])
readDeclarations pkg version = do
    vpkg <- do
        case version of
            Nothing -> return pkg
            Just v -> ifM (checkVersion pkg v) (return $ pkg ++ "-" ++ v) (return pkg)
    let filePath = downloadDir ++ vpkg ++ ".txt"
    readDeclarationsFromFile filePath

readDeclarationsFromFile :: FilePath -> IO (Map MdlName [Entry])
readDeclarationsFromFile fp = do
    fileLines <- lines <$> readFile fp
    let code = concat $ rights $ map parseLine fileLines
    return $ renameSigs "" code


packageDependencies :: PkgName -> Bool -> IO [PkgName]
packageDependencies pkg toDownload = do
    gPackageDesc <- readGenericPackageDescription silent $ downloadDir ++ pkg ++ ".cabal"
    case condLibrary gPackageDesc of
        Nothing -> return []
        Just (CondNode _ dependencies _) -> do
            let dps = map dependentPkg dependencies
            -- download necessary files to resolve package dependencies
            foldrM (\fname existDps ->
                ifM (if toDownload
                        then do
                          gotFile <- isJust <$> (downloadFile fname Nothing)
                          gotCabal <- isJust <$> (downloadCabal fname Nothing)
                          return (gotFile && gotCabal)
                        else doesFileExist $ downloadDir ++ fname ++ ".txt")
                    (return $ fname:existDps)
                    (return existDps)) [] dps
  where
    dependentPkg (Dependency name _) = unPackageName name

-- entryDependencies will look for the missing type declarations in `ourEntries`
-- by first checking `allEntries`, then looking at `dpDecls`
entryDependencies :: Map Id [Entry] -> [Entry] -> [Entry] -> [Entry]
entryDependencies allEntries ourEntries dpDecls = let
    myDtDefs = (dtDefsIn . concat . Map.elems) allEntries
    closedDecls = dependencyClosure myDefinedDts myDts (theirDts ++ myDtDefs)
    allDecls = closedDecls -- ++ (snd $ unzip myDtDefs)
    sortedIds = topoSort $ dependencyGraph allDecls
    in
    matchDtWithCons $ map (\id -> case Map.lookup id $ declMap allDecls of
                                             Nothing -> error $ "cannot find " ++ id
                                             Just v -> v) $ nub $ sortedIds >.> ["List", "Pair"]
  where
    myDts = dtNamesIn ourEntries
    myDefinedDts = definedDtsIn ourEntries
    theirDts = dtDefsIn dpDecls
    dependencyClosure definedDts allDts theirDts = let
        undefinedDts = allDts >.> definedDts
        in if length undefinedDts /= 0
            then let
                foreignDts = filter ((flip elem undefinedDts) . fst) theirDts
                newDecls = nub $ snd $ unzip foreignDts
                newAddedDts = Set.toList $ Set.unions $ map getDeclTy newDecls
                in newDecls ++ dependencyClosure allDts newAddedDts theirDts
            else []
    declMap decls = foldr (\d -> Map.insert (getDeclName d) d) Map.empty $ filter isDataDecl decls
    dependsOn decl = case decl of
        EDecl (DataDecl _ _ _ head conDecls _) -> (declHeadName head, datatypeOfCon conDecls)
        EDecl (TypeDecl _ head ty) -> (declHeadName head, datatypeOf ty)
        _ -> error "[In `dependsOn`] Please filter before calling this function"
    dependencyGraph decls = foldr (uncurry Map.insert) Map.empty $ map dependsOn $ filter isDataDecl decls
    nodesOf graph = nub $ (Map.keys graph) ++ (Set.toList $ Set.unions $ Map.elems graph)
    topoSort graph = reverse $ topoSortHelper (nodesOf graph) Set.empty graph
    topoSortHelper [] _ graph = []
    topoSortHelper (v:vs) visited graph = if Set.member v visited
        then topoSortHelper vs visited graph
        else topoSortHelper vs (Set.insert v visited) graph ++ v:(topoSortHelper (Set.toList (Map.findWithDefault Set.empty v graph)) visited graph)

declDependencies :: Id -> [Entry] -> [Entry] -> IO [Entry]
declDependencies pkgName decls dpDecls = do
    myDtDefs <- dtDefsIn . concat . Map.elems <$> readDeclarations pkgName Nothing
    let closedDecls = dependencyClosure myDefinedDts myDts (theirDts ++ myDtDefs)
    let allDecls = closedDecls -- ++ (snd $ unzip myDtDefs)
    let sortedIds = topoSort $ dependencyGraph allDecls
    return $ matchDtWithCons $ map (\id -> case Map.lookup id $ declMap allDecls of
                                             Nothing -> error $ "cannot find " ++ id
                                             Just v -> v) $ nub $ sortedIds >.> ["List", "Pair"]
  where
    myDts = dtNamesIn decls
    myDefinedDts = definedDtsIn decls
    theirDts = dtDefsIn dpDecls
    dependencyClosure definedDts allDts theirDts = let
        undefinedDts = allDts >.> definedDts
        in if length undefinedDts /= 0
            then let
                foreignDts = filter ((flip elem undefinedDts) . fst) theirDts
                newDecls = nub $ snd $ unzip foreignDts
                newAddedDts = Set.toList $ Set.unions $ map getDeclTy newDecls
                in newDecls ++ dependencyClosure allDts newAddedDts theirDts
            else []
    declMap decls = foldr (\d -> Map.insert (getDeclName d) d) Map.empty $ filter isDataDecl decls
    dependsOn decl = case decl of
        EDecl (DataDecl _ _ _ head conDecls _) -> (declHeadName head, datatypeOfCon conDecls)
        EDecl (TypeDecl _ head ty) -> (declHeadName head, datatypeOf ty)
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

isModule :: Entry -> Bool
isModule (EModule _) = True
isModule _ = False

dtNameOf :: Entry -> String
dtNameOf (EDecl (DataDecl _ _ _ head conDecls _)) = declHeadName head
dtNameOf (EDecl (TypeDecl _ head _)) = declHeadName head

packageDtDefs :: PkgName -> IO [(Id, Entry)]
packageDtDefs pkg = do
    decls <- concat . Map.elems <$> readDeclarations pkg Nothing
    -- It relies on the order of definitions exist in the source file
    return $ foldr (\decl -> ((dtNameOf decl, decl):)) [] $ filter isDataDecl decls

dtDefsIn :: [Entry] -> [(Id, Entry)]
dtDefsIn decls = foldr (\decl -> ((dtNameOf decl, decl):)) [] $ filter isDataDecl decls

getDeclTy :: Entry -> Set Id
getDeclTy (EDecl (TypeSig _ names ty)) = datatypeOf ty
getDeclTy (EDecl (TypeDecl _ _ ty)) = datatypeOf ty
getDeclTy _ = Set.empty

getDeclName :: Entry -> Id
getDeclName (EDecl (DataDecl _ _ _ head conDecls _)) = declHeadName head
getDeclName (EDecl (TypeDecl _ head ty)) = declHeadName head
getDeclName (EDecl (TypeSig _ names ty)) = nameStr $ head names
getDeclName _ = ""

-- packageDtNames :: PkgName -> IO [Id]
-- packageDtNames pkg = do
--     decls <- readDeclarations pkg Nothing
--     return $ Set.toList $ Set.unions $ map getDeclTy decls

dtNamesIn :: [Entry] -> [Id]
dtNamesIn decls = Set.toList $ Set.unions $ map getDeclTy decls

definedDtsIn :: [Entry] -> [Id]
definedDtsIn decls = map dtNameOf $ filter isDataDecl decls

-- definedDts :: [Entry] -> IO [Id]
-- definedDts decls = map dtNameOf $ filter isDataDecl decls

toSynquidProgram :: Exp SrcSpanInfo -> UProgram
toSynquidProgram (Lambda _ pats body) =
    foldr (\(PVar _ name) p -> Program (PFun (nameStr name) p) AnyT) (toSynquidProgram body) pats
toSynquidProgram (Var _ qname) = Program (PSymbol (qnameStr qname)) AnyT
toSynquidProgram (App _ fun arg) = Program (PApp (toSynquidProgram fun) (toSynquidProgram arg)) AnyT
toSynquidProgram (Paren _ e) = toSynquidProgram e
toSynquidProgram (Con _ qname) = Program (PSymbol (qnameStr qname)) AnyT
toSynquidProgram e = error $ show e
