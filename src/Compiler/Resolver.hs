-- | Functions for processing the AST created by the Parser (eg filling in unknown types, verifying that refinement formulas evaluate to a boolean, etc.)
module Compiler.Resolver
  ( resolveDecls
  , substituteTypeSynonym
  , ResolverState(..)
  , initResolverState
  , resolveSchema
  , resolveType
  ) where

import Control.Monad.Except
import Control.Monad.State (StateT, runStateT, gets, modify)
import Data.List (find)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)

import Text.PrettyPrint.ANSI.Leijen   ( string )

import Compiler.Error
import Types.Common  hiding ( varName )
import Types.Environment
import Types.Pretty
import Types.Program
import Types.Resolver
import Types.Type
import Utility.Utils
import Types.Substitution


--------------------------------------------------------------------------------
------------------------------- Resolver State ---------------------------------
--------------------------------------------------------------------------------

type Resolver a = StateT ResolverState (Except ErrorMessage) a

------ Operations on resolver state

initResolverState :: ResolverState
initResolverState = ResolverState [] [] 0

addSynonym :: TypeSynonym -> Resolver ()
addSynonym syn = modify $ \s -> s { getSynonyms = syn : getSynonyms s }

addDatatype :: DatatypeDef -> Resolver ()
addDatatype dt = modify $ \s -> s { getDatatypes = dt : getDatatypes s }

incCounter :: Resolver ()
incCounter = modify $ \s -> s { getIdCount = getIdCount s + 1 }


--------------------------------------------------------------------------------
------------------------------- Resolve Decls  ---------------------------------
--------------------------------------------------------------------------------

-- | Convert a parsed program AST into a list of synthesis goals and qualifier maps
resolveDecls :: [Declaration] -> Either ErrorMessage (Environment, ResolverState)
resolveDecls declarations =
  runExcept (runStateT go initResolverState) >>= Right
 where
  go :: Resolver Environment
  go = do
    -- Pass 1: collect all declarations and resolve sorts, but do not resolve refinement types yet
    env <- foldM (flip resolveDeclaration) emptyEnv declarations
    -- Pass 2: resolve refinement types in signatures
    foldM (flip resolveSignatures) env declarations

throwResError :: Doc -> Resolver a
throwResError descr = throwError $ ErrorMessage ResolutionError descr

resolveDeclaration :: Declaration -> Environment -> Resolver Environment
resolveDeclaration (TypeDecl typeName typeVars typeBody) env = do
  typeBody' <- resolveType (getBoundTypeVars env) typeBody
  let extraTypeVars = freeVars typeBody' Set.\\ Set.fromList typeVars
  if Set.null extraTypeVars
    then addSynonym (TypeSynonym typeName typeVars typeBody') >> return env
    else throwResError
      (   text "Type variable(s)"
      <+> hsep (map text $ Set.toList extraTypeVars)
      <+> text "in the definition of type synonym"
      <+> text typeName
      <+> text "are undefined"
      )
resolveDeclaration (FuncDecl funcName typeSchema) env =
  return $ addComponent funcName typeSchema env
resolveDeclaration (DataDecl dtName tParams ctors) env = do
  addDatatype (DatatypeDef dtName tParams (map constructorName ctors))
  return $ foldr
    (\(ConstructorSig name typ) -> addComponent name (Monotype typ))
    env
    ctors

resolveSignatures :: Declaration -> Environment -> Resolver Environment
resolveSignatures (FuncDecl name _) env =
  case lookupSymbol name env of
    Nothing  -> error "resolveSignatures: function not found"
    Just sch -> do
      sch' <- resolveSchema (getBoundTypeVars env) sch
      return $ addComponent name sch' $ removeVariable name env
resolveSignatures (DataDecl dtName tParams ctors) env = foldM resolveConstructorSignature env ctors
 where
  resolveConstructorSignature e (ConstructorSig name _) =
    case lookupSymbol name e of
      Nothing -> throwResError (string "resolveConstructorSignature: constructor not found")
      Just sch -> do
        sch' <- resolveSchema (getBoundTypeVars e) sch
        let nominalType = DatatypeT dtName (map vart tParams)
        let returnType  = lastType (toMonotype sch')
        if nominalType == returnType
          then return $ addComponent name sch' $ removeVariable name e
          else throwResError (text "Constructor" <+> text name <+> text "must return type" <+> pretty nominalType <+> text "," <+> text "got" <+> pretty returnType)
resolveSignatures _ env = return env

{- Types and sorts -}

resolveSchema :: [Id] -> SchemaSkeleton -> Resolver SchemaSkeleton
resolveSchema bvs sch = do
  let typ = toMonotype sch
  let tvs = Set.toList $ freeVars typ
  sch <- resolveType (bvs ++ tvs) typ
  return $ foldr ForallT (Monotype sch) tvs

resolveType :: [Id] -> TypeSkeleton -> Resolver TypeSkeleton
resolveType bvs (DatatypeT name tArgs) = do
  dts <- gets getDatatypes
  case lookupDatatype dts name of
    Nothing -> substituteTypeSynonym name tArgs >>= resolveType bvs
    Just (DatatypeDef _ tParams _) -> do
      when (length tArgs /= length tParams)
        $   throwResError
        $   text "Datatype"
        <+> text name
        <+> text "expected"
        <+> pretty (length tParams)
        <+> text "type arguments and got"
        <+> pretty (length tArgs)
      tArgs' <- mapM (resolveType bvs) tArgs
      return $ DatatypeT name tArgs'

resolveType bvs (FunctionT x tArg tRes) = do
  tArg' <- resolveType bvs tArg
  tRes' <- resolveType bvs tRes
  return $ FunctionT x tArg' tRes'

resolveType _ t = return t

--------------------------------------------------------------------------------
------------------------------- Miscellaneous ----------------------------------
--------------------------------------------------------------------------------

substituteTypeSynonym :: Text -> [TypeSkeleton] -> Resolver TypeSkeleton
substituteTypeSynonym name tArgs = do
  tss <- gets getSynonyms
  case lookupSynonym tss name of
    Nothing ->
      throwResError $ text "Datatype or synonym" <+> text name <+> text
        "is undefined"
    Just (TypeSynonym _ tVars t) -> do
      when (length tArgs /= length tVars)
        $   throwResError
        $   text "Type synonym"
        <+> text name
        <+> text "expected"
        <+> pretty (length tVars)
        <+> text "type arguments and got"
        <+> pretty (length tArgs)
      return $ apply (Map.fromList $ zip tVars tArgs) t

lookupDatatype :: [DatatypeDef] -> Id -> Maybe DatatypeDef
lookupDatatype dts name = find (\(DatatypeDef dt _ _) -> dt == name) dts

lookupSynonym :: [TypeSynonym] -> Id -> Maybe TypeSynonym
lookupSynonym synonyms name =
  find (\(TypeSynonym ts _ _) -> ts == name) synonyms
