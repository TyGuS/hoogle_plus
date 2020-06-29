{-# LANGUAGE ScopedTypeVariables #-}
module Database.Convert where

import Database.Download
import Database.Generate
import Database.Utils
import qualified Types.Program as TP
import Synquid.Error
import Synquid.Type
import Synquid.Utils
import Types.Common
import Types.Environment
import Types.Generate
import Types.Program (BareDeclaration, Declaration, BareProgram(..), UProgram, Program(..))
import Types.Type

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.State
import Data.Either
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Debug.Trace
import Distribution.Package
import Distribution.PackageDescription hiding (Var)
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Language.Haskell.Exts hiding (PApp)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sort as Sort
import qualified Data.Text as Text
import System.Directory
import System.IO
import Text.Parsec.Pos

prependName :: Id -> Name l -> Name l
prependName prefix name  = case name of
    Ident l var -> Ident l (prefix ++ "." ++ var)
    Symbol l var -> Ident l ("(" ++ prefix ++ "." ++ var ++ ")")

nameStr :: Name l -> Id
nameStr name = case name of
    Ident _ var -> var
    Symbol _ sym -> sym

isIdentity :: Name l -> Bool
isIdentity (Ident _ _) = True
isIdentity (Symbol _ _) = False

moduleNameStr :: ModuleName l -> Id
moduleNameStr (ModuleName _ name) = name

declHeadName :: DeclHead l -> Id
declHeadName (DHead _ name) = nameStr name
declHeadName (DHInfix _ bvar name) = nameStr name
declHeadName (DHParen _ head) = declHeadName head
declHeadName (DHApp _ head _) = declHeadName head

declHeadVars :: DeclHead l -> [Id]
declHeadVars (DHead _ _) = []
declHeadVars (DHInfix _ bvar name) = [varsFromBind bvar]
declHeadVars (DHParen _ head) = declHeadVars head
declHeadVars (DHApp _ head bvar) = varsFromBind bvar : declHeadVars head

qnameStr :: QName l -> Id
qnameStr name = case name of
    Qual _ moduleName consName -> moduleNameStr moduleName ++ "." ++ nameStr consName
    UnQual _ name -> nameStr name
    Special _ name -> specialConsStr name

consStr :: Type l -> Id
consStr (TyCon _ name) = qnameStr name
consStr (TyApp _ fun arg) = consStr fun
consStr (TyFun _ arg ret) = consStr arg ++ "To" ++ consStr ret
consStr (TyList _ typ) = "List" ++ consStr typ
consStr _ = "_"

specialConsStr :: SpecialCon l -> Id
specialConsStr (UnitCon _) = "Unit"
specialConsStr (ListCon _) = "Nil"
specialConsStr (FunCon _) = "Fun"
specialConsStr (TupleCon _ _ _) = "Pair"
specialConsStr (Language.Haskell.Exts.Cons _) = "Cons"
specialConsStr _ = "_"

allTypeVars :: Type l -> Set Id
allTypeVars (TyForall _ _ _ typ) = allTypeVars typ
allTypeVars (TyFun _ arg ret) = allTypeVars arg `Set.union` allTypeVars ret
allTypeVars (TyTuple _ _ typs) = foldr (Set.union . allTypeVars) Set.empty typs
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

datatypeOf :: Type l -> Set Id
datatypeOf (TyForall _ _ _ typ) = datatypeOf typ
datatypeOf (TyFun _ arg ret) = datatypeOf arg `Set.union` datatypeOf ret
datatypeOf (TyTuple _ _ typs) = foldr (\t vars -> vars `Set.union` datatypeOf t) (Set.singleton "Pair") typs
-- datatypeOf (TyUnboxedSum _ typs) = foldr (\t vars -> vars `Set.union` datatypeOf t) Set.empty typs
datatypeOf (TyList _ typ) = Set.singleton "List" `Set.union` datatypeOf typ
datatypeOf (TyApp _ fun arg) = datatypeOf fun `Set.union` datatypeOf arg
datatypeOf (TyVar _ name) = Set.empty
datatypeOf t@(TyCon _ name) | Special _ _ <- name = Set.empty
datatypeOf t@(TyCon _ name) = Set.singleton $ consStr t
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
        decl':decls' 
            | EDecl (TypeSig _ names typ) <- decl' -> 
                if nameStr (head names) == declHeadName hd
                    then let conDecl = QualConDecl a Nothing c (ConDecl a (head names) [typ])
                        in EDecl (DataDecl a b c hd (conDecl:conDecls) d) : matchDtWithCons decls'
                    else decl : matchDtWithCons decls
            | otherwise -> decl : matchDtWithCons decls
    _ -> decl : matchDtWithCons decls

resolveContext :: MonadIO m => Context () -> StateT Int m [TypeSkeleton]
resolveContext (CxSingle _ asst) = (:[]) <$> resolveAsst asst
resolveContext (CxTuple _ assts) = mapM resolveAsst assts
resolveContext (CxEmpty _)       = return []

resolveAsst :: MonadIO m => Asst () -> StateT Int m TypeSkeleton
resolveAsst a@(TypeA _ t) = do
    tc <- toSynquidSkeleton t
    let (dt, args) = collectArgs tc
    let dt' = fixTCName dt
    return (foldl' TyAppT (DatatypeT dt') args)
resolveAsst (ParenA _ asst) = resolveAsst asst
resolveAsst a = error $ "Unknown " ++ show a

toSynquidSchema :: MonadIO m => Type () -> StateT Int m SchemaSkeleton
toSynquidSchema (TyForall _ _ (Just ctx) typ) = do -- if this type has some context
    t <- toSynquidSkeleton typ
    classQuals <- resolveContext ctx
    return $ Monotype (foldr (FunctionT "ATypeClassDict") t (reverse classQuals))
toSynquidSchema typ = Monotype <$> toSynquidSkeleton typ

toSynquidSkeleton :: MonadIO m => Type () -> StateT Int m TypeSkeleton
toSynquidSkeleton t@(TyForall _ _ _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyFun _ arg ret) = do
    counter <- get
    put (counter + 1)
    ret' <- toSynquidSkeleton ret
    arg' <- toSynquidSkeleton arg
    return $ FunctionT ("arg"++show counter) arg' ret'
toSynquidSkeleton (TyParen _ typ) = toSynquidSkeleton typ
toSynquidSkeleton (TyKind _ typ _) = toSynquidSkeleton typ
toSynquidSkeleton t@(TyCon _ name) = case name of
    Qual _ moduleName consName -> let
        qualName = moduleNameStr moduleName ++ "." ++ nameStr consName
        in return $ DatatypeT qualName
    UnQual _ name -> return $ DatatypeT (nameStr name)
    Special _ name -> return $ DatatypeT (specialConsStr name)
toSynquidSkeleton (TyApp _ fun arg) = do
    f <- toSynquidSkeleton fun
    a <- toSynquidSkeleton arg
    return $ TyAppT f a
toSynquidSkeleton (TyVar _ name) = 
    return $ TypeVarT (nameStr name)
toSynquidSkeleton (TyList _ typ) = do
    typ' <- toSynquidSkeleton typ
    return $ TyAppT (DatatypeT "List") typ'
toSynquidSkeleton (TyTuple _ _ (f:s:ts)) = do
    f' <- toSynquidSkeleton f
    s' <- toSynquidSkeleton s
    pts <- mapM toSynquidSkeleton ts
    let base = TyAppT (TyAppT (DatatypeT "Pair") f') s'
    let mkPair a = TyAppT (TyAppT (DatatypeT "Pair") a)
    let res = foldl' mkPair base pts
    return res
toSynquidSkeleton t =
    error $ "[toSynquidSkeleton] unhandled case, ignoring: " ++ show t

varsFromBind :: TyVarBind l -> Id
varsFromBind (KindedVar _ name _) = nameStr name
varsFromBind (UnkindedVar _ name) = nameStr name

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
            typ <- toSynquidSkeleton $ head typs
            if hasAny typ 
                then processConDecls decls
                else (:) (TP.ConstructorSig (nameStr name) typ) <$> processConDecls decls
        InfixConDecl _ typl name typr -> do
            typl' <- toSynquidSkeleton typl
            typr' <- toSynquidSkeleton typr
            if hasAny typl' || hasAny typr'
                then processConDecls decls
                else (:) (TP.ConstructorSig (nameStr name) (FunctionT "arg0" typl' typr')) <$> processConDecls decls
        RecDecl _ name fields -> error "record declaration is not supported"

datatypeOfCon :: [QualConDecl ()] -> Set Id
datatypeOfCon [] = Set.empty
datatypeOfCon (decl:decls) = let QualConDecl _ _ _ conDecl = decl in
    case conDecl of
        ConDecl _ name typs -> Set.unions $ map datatypeOf typs
        InfixConDecl _ typl name typr -> datatypeOf typl `Set.union` datatypeOf typr
        RecDecl _ name fields -> error "record declaration is not supported"

toSynquidDecl :: MonadIO m => Entry -> StateT Int m Declaration
toSynquidDecl (EDecl (TypeDecl _ head typ)) = do
    typ' <- toSynquidSkeleton typ
    let dt = DatatypeT (declHeadName head)
    let synonym = foldl' (\a t -> TyAppT t a) dt (map TypeVarT $ declHeadVars head)
    return $ Pos (initialPos $ declHeadName head) $ TP.TypeDecl synonym typ'
toSynquidDecl (EDecl (DataFamDecl a b head c)) = 
    toSynquidDecl (EDecl (DataDecl a (DataType a) b head [] []))
toSynquidDecl (EDecl (DataDecl _ _ _ head conDecls _)) = do
    constructors <- processConDecls conDecls
    let name = declHeadName head
    let vars = declHeadVars head
    return $ Pos (initialPos name) $ TP.DataDecl name vars constructors
toSynquidDecl (EDecl (TypeSig _ names typ)) = do
    sch <- toSynquidSchema typ
    return $ Pos (initialPos (nameStr $ names !! 0)) 
           $ TP.FuncDecl (nameStr $ head names) sch
toSynquidDecl (EDecl (ClassDecl _ _ head _ _)) = do
    let name = fixTCName (declHeadName head)
    let vars = declHeadVars head
    return $ Pos (initialPos "") $ TP.DataDecl name vars []
toSynquidDecl decl =
    return $ Pos (initialPos "") $ TP.TypeDecl (DatatypeT "Int") (DatatypeT "Int") -- [TODO] a fake conversion

isInstance :: Entry -> Bool
isInstance (EDecl (InstDecl _ _ _ _)) = True
isInstance _ = False

instHeadName :: InstHead l -> [Char]
instHeadName (IHCon _ name) = qnameStr name
instHeadName (IHInfix _ bvar name) = qnameStr name
instHeadName (IHParen _ head) = instHeadName head
instHeadName (IHApp _ head _) = instHeadName head

fixDataType :: TypeSkeleton -> TypeSkeleton
fixDataType (DatatypeT name) =
    let (_, name') = breakLast name
        in DatatypeT name'
fixDataType (TyAppT fun arg) = TyAppT fun' arg'
    where
        fun' = fixDataType fun
        arg' = fixDataType arg
fixDataType x = x

resolveInstHead :: MonadIO m => InstHead () -> StateT Int m TypeSkeleton
resolveInstHead (IHCon _ qname) = return (DatatypeT (qnameStr qname))
resolveInstHead (IHInfix _ t qname) = TyAppT (DatatypeT (qnameStr qname)) <$> toSynquidSkeleton t
resolveInstHead (IHParen _ h) = resolveInstHead h
resolveInstHead (IHApp _ h t) = do
    dt <- resolveInstHead h
    typ <- toSynquidSkeleton t
    return (TyAppT dt typ)

-- FIRST KIND: instance Show Int              >>> __hplusTCTransition__Show Int
-- SECOND KIND: instance (Show a) => Show [a] >> __hplusTCTrransition__Show a -> __hplusTCTransition__Show (List a) -> ...
-- THIRD KIND: instance (Show a, Show b) => Show (Either a b) >> ......
instanceToFunction :: MonadIO m => InstRule () -> Int -> StateT Int m Declaration
instanceToFunction (IParen _ inst) n = instanceToFunction inst n
instanceToFunction (IRule _ _ ctx h) n = do
    tyclass <- resolveInstHead h
    let (name, tyVars) = collectArgs tyclass
    let tyVars' = map fixDataType tyVars
    let base = foldl' TyAppT (DatatypeT name) tyVars'
    let toDecl' = toDecl . Text.unpack $ Text.replace (Text.pack tyclassPrefix) (Text.pack "") (Text.pack name)
    case ctx of
        Nothing -> toDecl' base
        Just (CxTuple _ tyclassConds) -> toDecl' =<< foldrM go base tyclassConds
        Just (CxSingle _ tyclassCond) -> toDecl' =<< foldrM go base [tyclassCond]
        _ -> error "instanceToFunction: Unhandled case"
    where
        go e acc = do
            arg <- resolveAsst e
            return $ FunctionT "" arg acc

        toDecl :: (MonadIO m) => String -> TypeSkeleton -> StateT Int m Declaration
        toDecl y x = return $ Pos (initialPos "") $ TP.FuncDecl (tyclassInstancePrefix ++ show n ++ y) $ Monotype x

fixTCName :: String -> String
fixTCName str =
    let (_, end) = breakLast str
        end' = tyclassPrefix ++ end
        in end'

breakLast :: String -> (String, String)
breakLast str = (reverse (drop 1 y), reverse x) 
    where 
        (x, y) = break (== '.') $ reverse str

getInstanceRule :: Entry -> InstRule ()
getInstanceRule (EDecl (InstDecl x1 x2 (IParen _ instanceRule) x3)) = getInstanceRule (EDecl (InstDecl x1 x2 instanceRule x3))
getInstanceRule (EDecl (InstDecl _ _ instanceRule@(IRule _ _ _ _) _)) = instanceRule
getInstanceRule _ =  error "getInstanceRule: unexpected case"

reorderDecls :: [Declaration] -> [Declaration]
reorderDecls = Sort.sortOn toInt
  where
    toInt (Pos _ (TP.TypeDecl (DatatypeT "String") _)) = 1
    toInt (Pos _ TP.TypeDecl {}) = 3
    toInt (Pos _ (TP.DataDecl "List" _ _)) = 0
    toInt (Pos _ (TP.DataDecl "Char" _ _)) = 0
    toInt (Pos _ (TP.DataDecl "Pair" _ _)) = 0
    toInt (Pos _ (TP.DataDecl _ [] _)) = 1
    toInt (Pos _ TP.DataDecl {}) = 2
    toInt (Pos _ TP.FuncDecl {}) = 99
    toInt (Pos _ TP.SynthesisGoal {}) = 100

renameSigs :: Bool -> String -> [Entry] -> Map Id [Entry]
renameSigs _ _ [] = Map.empty
renameSigs renameFunc currModule (decl:decls) = case decl of
    EModule mdl -> Map.insertWith (++) mdl [decl] (renameSigs renameFunc mdl decls)
    EPackage _ -> renameSigs renameFunc currModule decls
    EDecl (TypeSig loc names ty) -> let
      newNames = if renameFunc then map (prependName currModule) names else names
      in Map.insertWith (++) currModule [EDecl (TypeSig loc newNames ty)] (renameSigs renameFunc currModule decls)
    _ -> Map.insertWith (++) currModule [decl] (renameSigs renameFunc currModule decls)

readDeclarations :: PkgName -> Maybe Version -> IO (Map Id [Entry])
readDeclarations pkg version = do
    downloadDir <- getTmpDir
    vpkg <-
        case version of
            Nothing -> return pkg
            Just v -> ifM (checkVersion pkg v) (return $ pkg ++ "-" ++ v) (return pkg)
    let filePath = downloadDir ++ vpkg ++ ".txt"
    readDeclarationsFromFile filePath True

readDeclarationsFromFile :: FilePath -> Bool -> IO (Map MdlName [Entry])
readDeclarationsFromFile fp renameFunc = do
    h   <- openFile fp ReadMode
    hSetEncoding h utf8
    s   <- hGetContents h
    let fileLines = lines s
    let code = concat $ rights $ map parseLine fileLines
    return $ renameSigs renameFunc "" code


packageDependencies :: PkgName -> Bool -> IO [PkgName]
packageDependencies pkg toDownload = do
    downloadDir <- getTmpDir
    gPackageDesc <- readGenericPackageDescription silent $ downloadDir ++ pkg ++ ".cabal"
    case condLibrary gPackageDesc of
        Nothing -> return []
        Just (CondNode _ dependencies _) -> do
            let dps = map dependentPkg dependencies
            -- download necessary files to resolve package dependencies
            foldrM (\fname existDps ->
                ifM (if toDownload
                        then do
                          gotFile <- isJust <$> downloadFile fname Nothing
                          gotCabal <- isJust <$> downloadCabal fname Nothing
                          return (gotFile && gotCabal)
                        else doesFileExist $ downloadDir ++ fname ++ ".txt")
                    (return $ fname:existDps)
                    (return existDps)) [] dps
  where
    dependentPkg (Dependency name _ _) = unPackageName name

declMap :: [Entry] -> Map Id Entry
declMap decls = foldr (\d -> Map.insert (getDeclName d) d) Map.empty $ filter isDataDecl decls

dependsOn :: Entry -> (Id, Set Id)
dependsOn decl = case decl of
    EDecl (DataDecl _ _ _ head conDecls _) -> (declHeadName head, datatypeOfCon conDecls)
    EDecl (TypeDecl _ head ty) -> (declHeadName head, datatypeOf ty)
    _ -> error "[In `dependsOn`] Please filter before calling this function"

dependencyGraph :: [Entry] -> Map Id (Set Id)
dependencyGraph = foldr (uncurry Map.insert . dependsOn) Map.empty . filter isDataDecl

nodesOf :: Map Id (Set Id) -> [Id]
nodesOf graph = nub $ Map.keys graph ++ Set.toList (Set.unions $ Map.elems graph)

topoSort :: Map Id (Set Id) -> [Id]
topoSort graph = reverse $ topoSortHelper (nodesOf graph) Set.empty graph
  where
    topoSortHelper [] _ graph = []
    topoSortHelper (v:vs) visited graph = if Set.member v visited
        then topoSortHelper vs visited graph
        else topoSortHelper vs (Set.insert v visited) graph ++
                v:topoSortHelper (Set.toList (Map.findWithDefault Set.empty v graph)) visited graph

dependencyClosure :: [Id] -> [Id] -> [(Id, Entry)] -> [Entry]
dependencyClosure definedDts allDts theirDts = let
    undefinedDts = allDts >.> definedDts
    in if not (null undefinedDts)
        then let
            foreignDts = filter ((`elem` undefinedDts) . fst) theirDts
            newDecls = nub $ map snd foreignDts
            newAddedDts = Set.toList $ Set.unions $ map getDeclTy newDecls
            in newDecls ++ dependencyClosure allDts newAddedDts theirDts
        else []

-- entryDependencies will look for the missing type declarations in `ourEntries`
-- by first checking `allEntries`, then looking at `dpDecls`
entryDependencies :: Map Id [Entry] -> [Entry] -> [Entry] -> [Entry]
entryDependencies allEntries ourEntries dpDecls = let
    myDtDefs = (dtDefsIn . concat . Map.elems) allEntries
    closedDecls = dependencyClosure myDefinedDts myDts (theirDts ++ myDtDefs)
    allDecls = closedDecls
    sortedIds = topoSort $ dependencyGraph allDecls
    in
    matchDtWithCons $ map (\id -> fromMaybe (error $ "cannot find " ++ id) (Map.lookup id $ declMap allDecls))
                    $ nub $ sortedIds >.> ["List", "Pair", "Char"]
  where
    myDts = dtNamesIn ourEntries
    myDefinedDts = definedDtsIn ourEntries
    theirDts = dtDefsIn dpDecls

declDependencies :: Id -> [Entry] -> [Entry] -> IO [Entry]
declDependencies pkgName decls dpDecls = do
    entries <- readDeclarations pkgName Nothing
    return $ entryDependencies entries decls dpDecls

isDataDecl :: Entry -> Bool
isDataDecl decl = case decl of
    EDecl DataDecl {} -> True
    EDecl TypeDecl {} -> True
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
toSynquidProgram (App _ fun arg) =
    let synFun = toSynquidProgram fun
     in case content synFun of
          PSymbol f -> Program (PApp f [toSynquidProgram arg]) AnyT
          PApp f args -> Program (PApp f (args ++ [toSynquidProgram arg])) AnyT
          _ -> error "unrecognized function type"
toSynquidProgram (Paren _ e) = toSynquidProgram e
toSynquidProgram (Con _ qname) = Program (PSymbol (qnameStr qname)) AnyT
toSynquidProgram (List _ elmts) = let
    args = map toSynquidProgram elmts
    mkCons e acc = Program (PApp "Cons" [e, acc]) AnyT
    in foldr mkCons (Program (PSymbol "Nil") AnyT) args
toSynquidProgram e = error $ show e
