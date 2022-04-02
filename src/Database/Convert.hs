module Database.Convert
  ( readDeclarationsFromFile
  , toTypeSkeleton
  , toSchemaSkeleton
  , toDeclaration
  , reorderDecls
  ) where

import           Control.Monad.State            ( MonadState(get, put)
                                                , State
                                                , foldM
                                                )
import           Data.Either                    ( rights )
import           Data.List                      ( foldl'
                                                , sortOn
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text

import           Language.Haskell.Exts   hiding ( PApp )
import           System.IO                      ( IOMode(ReadMode)
                                                , hGetContents
                                                , hSetEncoding
                                                , openFile
                                                , utf8
                                                )

import           Database.Generate
import           Types.Common
import           Types.Fresh
import           Types.Generate
import           Types.Program           hiding ( DataDecl
                                                , TypeDecl
                                                )
import qualified Types.Program                 as TP
import           Types.Type
import           Utility.Utils


--------------------------------------------------------------------------------
------------------------------ Names and Vars ----------------------------------
--------------------------------------------------------------------------------

prependName :: String -> Name l -> Name l
prependName prefix name = case name of
  Ident  l var -> Ident l (prefix ++ "." ++ var)
  Symbol l var -> Ident l ("(" ++ prefix ++ "." ++ var ++ ")")

nameStr :: Name l -> String
nameStr name = case name of
  Ident  _ var -> var
  Symbol _ sym -> sym

isIdentity :: Name l -> Bool
isIdentity (Ident  _ _) = True
isIdentity (Symbol _ _) = False

moduleNameStr :: ModuleName l -> String
moduleNameStr (ModuleName _ name) = name

declHeadName :: DeclHead l -> Id
declHeadName (DHead _ name       ) = Text.pack $ nameStr name
declHeadName (DHInfix _ bvar name) = Text.pack $ nameStr name
declHeadName (DHParen _ head     ) = declHeadName head
declHeadName (DHApp _ head _     ) = declHeadName head

declHeadVars :: DeclHead l -> [Id]
declHeadVars (DHead _ _          ) = []
declHeadVars (DHInfix _ bvar name) = [varsFromBind bvar]
declHeadVars (DHParen _ head     ) = declHeadVars head
declHeadVars (DHApp _ head bvar  ) = varsFromBind bvar : declHeadVars head

qnameStr :: QName l -> Id
qnameStr name = case name of
  Qual _ moduleName consName ->
    Text.pack $ moduleNameStr moduleName ++ "." ++ nameStr consName
  UnQual  _ name -> Text.pack (nameStr name)
  Special _ name -> specialConsStr name

specialConsStr :: SpecialCon l -> Id
specialConsStr (UnitCon _)                    = "Unit"
specialConsStr (ListCon _)                    = "Nil"
specialConsStr (FunCon  _)                    = "Fun"
specialConsStr TupleCon{}                     = "Pair"
specialConsStr (Language.Haskell.Exts.Cons _) = "Cons"
specialConsStr _                              = "_"

allTypeVars :: Type l -> Set String
allTypeVars (TyForall _ _ _ typ) = allTypeVars typ
allTypeVars (TyFun   _ arg ret ) = allTypeVars arg `Set.union` allTypeVars ret
allTypeVars (TyTuple _ _ typs) = foldr (Set.union . allTypeVars) Set.empty typs
allTypeVars (TyList _ typ      ) = allTypeVars typ
allTypeVars (TyApp _ fun arg   ) = allTypeVars fun `Set.union` allTypeVars arg
allTypeVars (TyVar   _ name    ) = Set.singleton $ nameStr name
allTypeVars (TyCon   _ name    ) = Set.empty
allTypeVars (TyParen _ typ     ) = allTypeVars typ
allTypeVars (TyInfix _ typ1 _ typ2) =
  allTypeVars typ1 `Set.union` allTypeVars typ2
allTypeVars (TyKind _ typ _) = allTypeVars typ
allTypeVars (TyEquals _ ltyp rtyp) =
  allTypeVars ltyp `Set.union` allTypeVars rtyp
allTypeVars _ = Set.empty

datatypeOf :: Type l -> Set Id
datatypeOf (TyForall _ _ _ typ) = datatypeOf typ
datatypeOf (TyFun _ arg ret   ) = datatypeOf arg `Set.union` datatypeOf ret
datatypeOf (TyTuple _ _ typs) =
  foldr (\t vars -> vars `Set.union` datatypeOf t) (Set.singleton "Pair") typs
datatypeOf (TyList _ typ   ) = Set.singleton "List" `Set.union` datatypeOf typ
datatypeOf (TyApp _ fun arg) = datatypeOf fun `Set.union` datatypeOf arg
datatypeOf (TyVar   _ name ) = Set.empty
datatypeOf (TyCon   _ name ) = Set.singleton $ qnameStr name
datatypeOf (TyParen _ typ  ) = datatypeOf typ
datatypeOf (TyInfix _ typ1 _ typ2) =
  datatypeOf typ1 `Set.union` datatypeOf typ2
datatypeOf (TyKind   _ typ  _   ) = datatypeOf typ
datatypeOf (TyEquals _ ltyp rtyp) = datatypeOf ltyp `Set.union` datatypeOf rtyp
datatypeOf _                      = Set.empty

varsFromBind :: TyVarBind l -> Id
varsFromBind (KindedVar _ name _) = Text.pack $ nameStr name
varsFromBind (UnkindedVar _ name) = Text.pack $ nameStr name

datatypeOfCon :: [QualConDecl ()] -> Set Id
datatypeOfCon [] = Set.empty
datatypeOfCon (decl : decls) =
  let QualConDecl _ _ _ conDecl = decl
  in  case conDecl of
        ConDecl _ name typs -> Set.unions $ map datatypeOf typs
        InfixConDecl _ typl name typr ->
          datatypeOf typl `Set.union` datatypeOf typr
        RecDecl _ name fields -> error "record declaration is not supported"

instHeadName :: InstHead l -> Id
instHeadName (IHCon _ name       ) = qnameStr name
instHeadName (IHInfix _ bvar name) = qnameStr name
instHeadName (IHParen _ head     ) = instHeadName head
instHeadName (IHApp _ head _     ) = instHeadName head

toTyclassName :: Id -> Id
toTyclassName name = Text.append tyclassPrefix $ last $ Text.splitOn "." name

getTyclassDictName :: InstHead l -> Id
getTyclassDictName (IHApp _ typeclass _) = instHeadName typeclass
getTyclassDictName (IHParen _ head) = getTyclassDictName head
getTyclassDictName _ = error "getTyclassDictName: case to be implemented"

--------------------------------------------------------------------------------
---------------------------- Convert Declarations ------------------------------
--------------------------------------------------------------------------------
instance Monad m => Fresh Int m where
  nextCounter _ = do
    i <- get
    put (i + 1)
    return i

matchDtWithCons :: [Entry] -> [Entry]
matchDtWithCons []             = []
matchDtWithCons (decl : decls) = case decl of
  EDecl (DataDecl a b c hd conDecls d) -> case decls of
    [] -> decl : matchDtWithCons decls
    decl' : decls'
      | EDecl (TypeSig _ names typ) <- decl'
      -> if Text.pack (nameStr (head names)) == declHeadName hd
        then
          let conDecl = QualConDecl a Nothing c (ConDecl a (head names) [typ])
          in  EDecl (DataDecl a b c hd (conDecl : conDecls) d)
                : matchDtWithCons decls'
        else decl : matchDtWithCons decls
      | otherwise
      -> decl : matchDtWithCons decls
  _ -> decl : matchDtWithCons decls

resolveContext :: Context () -> State Int [TypeSkeleton]
resolveContext (CxSingle _ asst ) = (: []) <$> resolveAsst asst
resolveContext (CxTuple  _ assts) = mapM resolveAsst assts
resolveContext (CxEmpty _       ) = return []

resolveAsst :: Asst () -> State Int TypeSkeleton
resolveAsst (TypeA  _ typ ) = toTyclassDatatype <$> toTypeSkeleton typ
resolveAsst (ParenA _ asst) = resolveAsst asst
resolveAsst a               = error $ "Unknown " ++ show a

toSchemaSkeleton :: Type () -> State Int SchemaSkeleton
toSchemaSkeleton (TyForall _ _ (Just ctx) typ) = do
  typ'       <- toTypeSkeleton typ
  classQuals <- resolveContext ctx
  Monotype <$> foldM addTyclass typ' classQuals
 where
  addTyclass :: TypeSkeleton -> TypeSkeleton -> State Int TypeSkeleton
  addTyclass typ classQual = do
    tcarg <- freshId [] "tcarg"
    return $ FunctionT tcarg classQual typ
toSchemaSkeleton typ = Monotype <$> toTypeSkeleton typ

toTypeSkeleton :: Type () -> State Int TypeSkeleton
toTypeSkeleton t@(TyForall _ _ _ typ) = toTypeSkeleton typ
toTypeSkeleton (  TyFun _ arg ret   ) = do
  argName <- freshId [] "arg"
  ret'    <- toTypeSkeleton ret
  arg'    <- toTypeSkeleton arg
  return $ FunctionT argName arg' ret'
toTypeSkeleton (TyParen _ typ ) = toTypeSkeleton typ
toTypeSkeleton (TyKind _ typ _) = toTypeSkeleton typ
toTypeSkeleton t@(TyCon _ name) =
  let dtName = case name of
        Qual _ moduleName consName ->
          moduleNameStr moduleName ++ "." ++ nameStr consName
        UnQual  _ name -> nameStr name
        Special _ name -> Text.unpack $ specialConsStr name
  in  return $ DatatypeT (Text.pack dtName) []
toTypeSkeleton (TyApp _ fun arg) = do
  fun' <- toTypeSkeleton fun
  arg' <- toTypeSkeleton arg
  case fun' of
    DatatypeT dtName args -> return $ DatatypeT dtName (args ++ [arg'])
    TypeVarT _ ->
      error "toTypeSkeleton: higher-kinded type variable is not supported"
    _ -> error "toTypeSkeleton: neither datatype nor type variable"
toTypeSkeleton (TyVar  _ name ) = return $ TypeVarT (Text.pack $ nameStr name)
toTypeSkeleton (TyList _ typ  ) = listType <$> toTypeSkeleton typ
toTypeSkeleton (TyTuple _ _ ts) = do
  ts' <- mapM toTypeSkeleton ts
  case ts' of
    (t1 : t2 : remains) -> return $ foldl' pairType (pairType t1 t2) remains
    _ -> error "toTypeSkeleton: tuple must have at least two elements"
toTypeSkeleton _ = return TopT

processConDecls :: [QualConDecl ()] -> State Int [ConstructorSig]
processConDecls = mapM processConDecl
 where
  processConDecl (QualConDecl _ _ _ conDecl) = case conDecl of
    ConDecl _ name typs -> do
      -- Note: I don't know why Haskell library design this to be a list
      -- Ordinarily, a constructor should have only one type signature,
      -- so we extract the head of the signature list.
      typ <- toTypeSkeleton $ head typs
      if hasAny typ
        then error "processConDecls: constructor type is not supported"
        else return $ ConstructorSig (Text.pack $ nameStr name) typ
    InfixConDecl _ typl name typr -> do
      typl' <- toTypeSkeleton typl
      typr' <- toTypeSkeleton typr
      if hasAny typl' || hasAny typr'
        then error "processConDecls: infix constructor type is not supported"
        else return $ ConstructorSig (Text.pack $ nameStr name)
                                     (FunctionT "arg0" typl' typr')
    RecDecl _ name fields -> error "record declaration is not supported"

toDeclaration :: Entry -> State Int Declaration
toDeclaration (EDecl (TypeDecl _ head typ)) = do
  typ' <- toTypeSkeleton typ
  if hasAny typ'
    then return dummyDecl -- a fake conversion
    else return $ TP.TypeDecl (declHeadName head) (declHeadVars head) typ'
toDeclaration (EDecl (DataFamDecl a b head c)) =
  toDeclaration (EDecl (DataDecl a (DataType a) b head [] []))
toDeclaration (EDecl (DataDecl _ _ _ head conDecls _)) = do
  constructors <- processConDecls conDecls
  let name = declHeadName head
  let vars = declHeadVars head
  return $ TP.DataDecl name vars constructors
toDeclaration (EDecl (TypeSig _ names typ)) =
  FuncDecl (Text.pack $ nameStr $ head names) <$> toSchemaSkeleton typ
toDeclaration d@(EDecl (ClassDecl _ _ head _ _)) = do
  let name = toTyclassName (declHeadName head)
  let vars = declHeadVars head
  return $ TP.DataDecl name vars []
toDeclaration (EDecl (InstDecl _ _ rule _)) = instanceToFunction (getInstanceRule rule)
toDeclaration decl = return dummyDecl -- a fake conversion

isInstance :: Entry -> Bool
isInstance (EDecl InstDecl{}) = True
isInstance _                  = False

getInstanceRule :: InstRule l -> InstRule l
getInstanceRule (IParen _ instanceRule) = instanceRule
getInstanceRule instanceRule@IRule{}    = instanceRule

getTyclassVars :: InstHead () -> State Int [TypeSkeleton]
getTyclassVars (IHApp _ hd typeVar) = do
  typeSkeletonArr <- getTyclassVars hd
  typeSkeleton    <- toTypeSkeleton typeVar
  return (typeSkeletonArr ++ [typeSkeleton])
getTyclassVars (IHParen _ hd) = getTyclassVars hd
getTyclassVars (IHCon   _ _ ) = return []
getTyclassVars _              = error "getTyclassVars: case to be implemented"

unqualDataType :: TypeSkeleton -> TypeSkeleton
unqualDataType (DatatypeT name vars) =
  let name' = last $ Text.splitOn "." name in DatatypeT name' vars
unqualDataType x = x

-- FIRST KIND: instance Show Int              >>> __hplusTCTransition__Show Int
-- SECOND KIND: instance (Show a) => Show [a] >> __hplusTCTrransition__Show a -> __hplusTCTransition__Show (List a) -> ...
-- THIRD KIND: instance (Show a, Show b) => Show (Either a b) >> ......
instanceToFunction :: InstRule () -> State Int Declaration
instanceToFunction (IParen _ inst     ) = instanceToFunction inst
instanceToFunction (IRule _ _ ctx head) = do
  let name = getTyclassDictName head
  -- Note: remove this check to include higher kinded type instances
  if any (`Text.isInfixOf` name) higherOrderNames
    then return dummyDecl
    else do
      tyVars <- getTyclassVars head
      let tyVars' = map unqualDataType tyVars
      let base    = DatatypeT (toTyclassName name) tyVars'
      instanceTyp <- case ctx of
        Nothing                        -> return base
        Just (CxTuple  _ tyclassConds) -> foldM mkInstanceType base tyclassConds
        Just (CxSingle _ tyclassCond ) -> mkInstanceType base tyclassCond
        _ -> error "instanceToFunction: Unhandled case"
      mkInstanceDecl name instanceTyp
 where
  mkInstanceType :: TypeSkeleton -> Asst () -> State Int TypeSkeleton
  mkInstanceType typ asst = do
    tcArg <- resolveAsst asst
    case tcArg of
      DatatypeT {} -> do
        tcArgName <- freshId [] "tcarg"
        return $ FunctionT tcArgName tcArg typ
      _ -> return typ -- TODO: we ignore the weird type classes for now

  mkInstanceDecl :: Id -> TypeSkeleton -> State Int Declaration
  mkInstanceDecl dtName x = do
    n <- nextCounter "inst"
    return $ FuncDecl
      (appendIndex tyclassInstancePrefix n `Text.append` dtName)
      (Monotype x)

toTyclassDatatype :: TypeSkeleton -> TypeSkeleton
toTyclassDatatype (DatatypeT dt tArgs) = DatatypeT (toTyclassName dt) tArgs
toTyclassDatatype TopT = TopT
toTyclassDatatype x = error "toTyclassDatatype: not a datatype"

reorderDecls :: [Declaration] -> [Declaration]
reorderDecls = sortOn toInt
 where
  toInt (TP.TypeDecl "String" _  _) = 1
  toInt (TP.TypeDecl _        [] _) = 2
  toInt TP.TypeDecl{}               = 3
  toInt (TP.DataDecl "List" _  _)   = 0
  toInt (TP.DataDecl "Char" _  _)   = 0
  toInt (TP.DataDecl "Pair" _  _)   = 0
  toInt (TP.DataDecl _      [] _)   = 2
  toInt TP.DataDecl{}               = 3
  toInt TP.FuncDecl{}               = 99

higherOrderNames :: [Id]
higherOrderNames = ["Applicative", "Functor", "Monad"]

renameSigs :: Bool -> String -> [Entry] -> Map String [Entry]
renameSigs _          _          []             = Map.empty
renameSigs renameFunc currModule (decl : decls) = case decl of
  EModule mdl ->
    Map.insertWith (++) mdl [decl] (renameSigs renameFunc mdl decls)
  EPackage _ -> renameSigs renameFunc currModule decls
  EDecl (TypeSig loc names ty) ->
    let newNames =
          if renameFunc then map (prependName currModule) names else names
    in  Map.insertWith (++)
                       currModule
                       [EDecl (TypeSig loc newNames ty)]
                       (renameSigs renameFunc currModule decls)
  _ -> Map.insertWith (++)
                      currModule
                      [decl]
                      (renameSigs renameFunc currModule decls)

readDeclarationsFromFile :: FilePath -> Bool -> IO (Map MdlName [Entry])
readDeclarationsFromFile fp renameFunc = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  s <- hGetContents h
  let fileLines = lines s
  let code = concat $ rights $ map parseLine fileLines
  return $ renameSigs renameFunc "" code

--------------------------------------------------------------------------------
------------------------------- Miscellaneous ----------------------------------
--------------------------------------------------------------------------------

isDataDecl :: Entry -> Bool
isDataDecl decl = case decl of
  EDecl DataDecl{} -> True
  EDecl TypeDecl{} -> True
  _                -> False

isModule :: Entry -> Bool
isModule (EModule _) = True
isModule _           = False
