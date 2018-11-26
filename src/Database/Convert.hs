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
import Debug.Trace
import System.Directory

import Synquid.Type
import Synquid.Logic hiding (Var)
import Synquid.Program (emptyEnv, BareDeclaration, Declaration, Environment, BareProgram(..), UProgram, Program(..))
import qualified Synquid.Program as SP
import Synquid.Util
import Synquid.Error
import Database.Generate
import Database.Util
import Database.Download
-- import Synquid.Succinct

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
resolveAsst (ClassA _ qname typs) = return [(Set.findMin . Set.unions $ map allTypeVars typs, [qnameStr qname])]
resolveAsst (ParenA _ asst) = resolveAsst asst
resolveAsst a = error $ "Unknown " ++ show a

toSynquidSchema :: (MonadIO m) => Type () -> StateT Int m (Maybe SSchema)
toSynquidSchema (TyForall _ _ (Just ctx) typ) = do -- if this type has some context
    typs <- toSynquidSkeleton typ
    case typs of
        Nothing -> return Nothing
        Just ty -> do
            classQuals <- resolveContext ctx
            return $ Just $ foldr ForallT (Monotype ty) classQuals
toSynquidSchema typ = do
    typs <- toSynquidSkeleton typ
    case typs of
        Nothing -> return Nothing
        Just ty  -> return $ Just $ Monotype ty

-- toSynquidSchema converts a haskell type into an unrefined type
-- This is a lossy operation.
toSynquidSkeleton :: (MonadIO m) => Type () -> StateT Int m (Maybe SType)
toSynquidSkeleton t@(TyForall _ _ _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyFun _ arg ret) = do
    counter <- get
    -- traceShow ((show counter)) $ return ()
    put (counter + 1)
    ret' <- toSynquidSkeleton ret
    arg' <- toSynquidSkeleton arg
    if isNothing ret' || isNothing arg'
        then return Nothing
        else let
            arg'' = fromJust arg'
            ret'' = fromJust ret'
            in
            return . Just $ FunctionT ("arg"++show counter) (arg'') (ret'')
toSynquidSkeleton (TyParen _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyKind _ typ _) = toSynquidSkeleton typ
toSynquidSkeleton t@(TyCon _ name) = case name of
    Qual _ moduleName consName -> return . Just $ ScalarT (TypeConT ((moduleNameStr moduleName) ++ "." ++ (nameStr consName))) ()
    UnQual _ name -> case nameStr name of
        "Int" -> return . Just $ ScalarT IntT ()
        "Bool" -> return . Just $ ScalarT BoolT ()
        xarg -> return . Just $ ScalarT (TypeConT xarg) ()
    Special _ name -> return . Just $ ScalarT (TypeConT (specialConsStr name)) ()
toSynquidSkeleton t@(TyApp _ fun arg) = do
    Just (ScalarT baseTyFun _) <- toSynquidSkeleton fun
    Just tyargs <- toSynquidSkeleton arg
    return . Just $ ScalarT (TypeAppT baseTyFun tyargs) ()
toSynquidSkeleton (TyVar _ name) = return . Just $ ScalarT (TypeVarT Map.empty $ nameStr name) ()
toSynquidSkeleton (TyList _ typ) = do
    Just typ' <- toSynquidSkeleton typ
    return . Just $ ScalarT (TypeAppT (TypeConT "List") typ') ()
toSynquidSkeleton (TyTuple _ _ typs) = do
    fst <- toSynquidSkeleton (head typs)
    snd <- toSynquidSkeleton (typs !! 1)
    if (isNothing fst && isNothing snd)
        then (return Nothing)
        else let
            fst' = fromJust fst
            snd' = fromJust snd
            in
            return . Just $ ScalarT (TypeAppT (TypeAppT (TypeConT "Pair") fst') snd') ()
toSynquidSkeleton t = error $ "Unhandled case " ++ show t

varsFromBind (KindedVar _ name _) = nameStr name
varsFromBind (UnkindedVar _ name) = nameStr name

-- decomposeDH :: DeclHead () -> (Maybe String, [String])
-- decomposeDH (DHead _ name) = (Just $ nameStr name, [])
-- decomposeDH (DHInfix _ varBind name) = (Nothing, [varsFromBind varBind]++(if isIdentity name then [nameStr name] else []))
-- decomposeDH (DHParen _ dh) = decomposeDH dh
-- decomposeDH (DHApp _ funHead varBind) = let (name, vars) = decomposeDH funHead in (name, (varsFromBind varBind):vars)

-- | Add true as the refinement to convert all types into RType
addTrue :: TypeSkeleton a -> RType
addTrue (FunctionT x tArg tFun) = FunctionT x (addTrue tArg) (addTrue tFun)
addTrue (LetT id bindings bound) = LetT id (addTrue bindings) (addTrue bound)
addTrue AnyT = AnyT
addTrue (ScalarT baseTy _) = ScalarT (addTrueBase baseTy) ftrue
    where
        addTrueBase :: BaseType r -> BaseType Formula
        addTrueBase BoolT = BoolT
        addTrueBase IntT = IntT
        addTrueBase (TypeConT id) = TypeConT id
        addTrueBase (TypeVarT sub name) = TypeVarT sub name
        addTrueBase (TypeAppT l r) = TypeAppT (addTrueBase l) (addTrue r)

toSynquidRType :: (MonadIO m) => Type () -> StateT Int m RType
toSynquidRType typ = do
    typ' <- toSynquidSkeleton typ
    return $ fromMaybe AnyT (addTrue <$> typ')

toSynquidRSchema :: SSchema -> RSchema
toSynquidRSchema (Monotype typ) = Monotype $ addTrue typ
toSynquidRSchema (ForallT a typ) = ForallT a (toSynquidRSchema typ)

addPrelude :: [Entry] -> [Entry]
addPrelude [] = []
addPrelude (decl:decls) = case decl of
    EModule mdl -> if mdl == "GHC.OldList" then takeWhile (not . isModule) decls else addPrelude decls
    _ -> addPrelude decls

processConDecls :: (MonadIO m) => [QualConDecl ()] -> StateT Int m [SP.ConstructorSig]
processConDecls [] = return []
processConDecls (decl:decls) = let QualConDecl _ _ _ conDecl = decl in
    case conDecl of
        ConDecl _ name typs -> do
            typ <- toSynquidRType $ head typs
            if hasAny typ then processConDecls decls
                          else (:) (SP.ConstructorSig (nameStr name) typ) <$> (processConDecls decls)
        InfixConDecl _ typl name typr -> do
            typl' <- toSynquidRType typl
            typr' <- toSynquidRType typr
            if hasAny typl' || hasAny typr'
                then processConDecls decls
                else (:) (SP.ConstructorSig (nameStr name) (FunctionT "arg0" typl' typr')) <$> (processConDecls decls)
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
    if hasAny typ' then return $ Pos (initialPos "") $ SP.QualifierDecl [] -- a fake conversion
                   else return $ Pos (initialPos $ declHeadName head) $ SP.TypeDecl (declHeadName head) (declHeadVars head) typ'
toSynquidDecl (EDecl (DataFamDecl a b head c)) = toSynquidDecl (EDecl (DataDecl a (DataType a) b head [] []))
toSynquidDecl (EDecl (DataDecl _ _ _ head conDecls _)) = do
    constructors <- processConDecls conDecls
    let name = declHeadName head
    let vars = declHeadVars head
    return $ Pos (initialPos name) $ SP.DataDecl name vars [] constructors
toSynquidDecl (EDecl (TypeSig _ names typ)) = do
    maybeSch <- toSynquidSchema typ
    case maybeSch of
        Nothing  -> return $ Pos (initialPos "") $ SP.QualifierDecl [] -- a fake conversion
        Just sch -> return $ Pos (initialPos (nameStr $ names !! 0)) $ SP.FuncDecl (nameStr $ head names) (toSynquidRSchema sch)
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

readDeclarations :: PkgName -> Maybe Version -> IO [Entry]
readDeclarations pkg version = do
    vpkg <- do
        case version of
            Nothing -> return pkg
            Just v -> ifM (checkVersion pkg v) (return $ pkg ++ "-" ++ v) (return pkg)
    s   <- readFile $ downloadDir ++ vpkg ++ ".txt"
    let declEntries = map parseLine $ splitOn "\n" s
    let code = concat $ rights declEntries
    -- mapM_ putStrLn (lefts declEntries)
    return $ renameSigs "" code

readDeclarationsFromFile :: FilePath -> IO [Entry]
readDeclarationsFromFile filePath = do
    s <- readFile filePath
    let declEntries = map parseLine $ splitOn "\n" s
    let code = concat $ rights declEntries
    return $ renameSigs "" code

type DependsOn = Map PkgName [Id]

packageDependencies :: PkgName -> Bool -> IO [PkgName]
packageDependencies pkg toDownload = do
    gPackageDesc <- readGenericPackageDescription silent $ downloadDir ++ pkg ++ ".cabal"
    case condLibrary gPackageDesc of
        Nothing -> return []
        Just (CondNode _ dependencies _) -> do
            let allDeps = map dependentPkg dependencies
            let deps = filter (\x -> not $ x `elem` bannedBuildDeps) allDeps
            -- download necessary files to resolve package dependencies
            foldrM (\fname existDps ->
                ifM (if toDownload
                        then downloadFile fname Nothing >> downloadCabal fname Nothing
                        else doesFileExist $ downloadDir ++ fname ++ ".txt")
                    (return $ fname:existDps)
                    (return existDps)) [] deps
  where
    dependentPkg (Dependency name _) = unPackageName name

declDependencies :: Id -> [Entry] -> [Entry] -> IO [Entry]
declDependencies pkgName decls dpDecls = do
    myDtDefs <- dtDefsIn <$> readDeclarations pkgName Nothing
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
    decls <- readDeclarations pkg Nothing
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

packageDtNames :: PkgName -> IO [Id]
packageDtNames pkg = do
    decls <- readDeclarations pkg Nothing
    return $ Set.toList $ Set.unions $ map getDeclTy decls

dtNamesIn :: [Entry] -> [Id]
dtNamesIn decls = Set.toList $ Set.unions $ map getDeclTy decls

definedDtsIn :: [Entry] -> [Id]
definedDtsIn decls = map dtNameOf $ filter isDataDecl decls

definedDts :: PkgName -> IO [Id]
definedDts pkg = do
    decls <- readDeclarations pkg Nothing
    let dtDecls = filter isDataDecl decls
    return $ map dtNameOf dtDecls

toSynquidProgram :: Exp SrcSpanInfo -> UProgram
toSynquidProgram (Lambda _ pats body) =
    foldr (\(PVar _ name) p -> Program (PFun (nameStr name) p) AnyT) (toSynquidProgram body) pats
toSynquidProgram (Var _ qname) = Program (PSymbol (qnameStr qname)) AnyT
toSynquidProgram (App _ fun arg) = Program (PApp (toSynquidProgram fun) (toSynquidProgram arg)) AnyT
toSynquidProgram (Paren _ e) = toSynquidProgram e
toSynquidProgram (Con _ qname) = Program (PSymbol (qnameStr qname)) AnyT
toSynquidProgram e = error $ show e
