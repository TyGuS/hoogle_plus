{-# LANGUAGE TupleSections, FlexibleContexts, TemplateHaskell #-}

-- | Functions for processing the AST created by the Parser (eg filling in unknown types, verifying that refinement formulas evaluate to a boolean, etc.)
module Synquid.Resolver (
    resolveDecls
  , addAllVariables
  , substituteTypeSynonym
  , ResolverState (..)
  , instantiateSorts
  , initResolverState
  , resolveSchema) where

import Synquid.Error
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Util
import Types.Common hiding (varName)
import Types.Generate
import Types.Environment
import Types.Program
import Types.Type
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Maybe
import Data.Either hiding (fromRight)
import Data.List
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable


{- Interface -}

data ResolverState = ResolverState {
    _environment :: Environment,
    _goals :: [(Id, (UProgram, SourcePos))],
    _currentPosition :: SourcePos,
    _idCount :: Int
}

makeLenses ''ResolverState

initResolverState = ResolverState {
    _environment = emptyEnv,
    _goals = [],
    _currentPosition = noPos,
    _idCount = 0
}

-- | Convert a parsed program AST into a list of synthesis goals and qualifier maps
resolveDecls :: [Declaration] -> [MdlName] -> Either ErrorMessage Environment
resolveDecls declarations moduleNames =
    runExcept (execStateT go initResolverState) >>= (\s -> Right (s ^. environment))
    where
        go = do
            -- Pass 1: collect all declarations and resolve sorts, but do not resolve refinement types yet
            mapM_ (extractPos resolveDeclaration) declarations
            -- Pass 2: resolve refinement types in signatures
            mapM_ (extractPos resolveSignatures) declarations
        extractPos pass (Pos pos decl) = do
            currentPosition .= pos
            pass decl

addAllVariables :: [Formula] -> Environment -> Environment
addAllVariables = flip (foldr (\(Var s x) -> addVariable x (fromSort s)))

{- Implementation -}

type Resolver a = StateT ResolverState (Except ErrorMessage) a

throwResError descr = do
    pos <- use currentPosition
    throwError $ ErrorMessage ResolutionError pos descr

resolveDeclaration :: BareDeclaration -> Resolver ()
resolveDeclaration (TypeDecl typeName typeVars typeBody) = do
    typeBody' <- resolveType typeBody
    let extraTypeVars = typeVarsOf typeBody' Set.\\ Set.fromList typeVars
    if Set.null extraTypeVars
        then environment %= addTypeSynonym typeName typeVars typeBody'
        else throwResError (text "Type variable(s)" <+> hsep (map text $ Set.toList extraTypeVars) <+>
                text "in the definition of type synonym" <+> text typeName <+> text "are undefined")
resolveDeclaration (FuncDecl funcName typeSchema) = addNewSignature funcName typeSchema
resolveDeclaration (DataDecl dtName tParams ctors) = do
    let
        datatype = DatatypeDef {
        _typeParams = tParams,
        _constructors = map constructorName ctors
        }
    environment %= addDatatype dtName datatype
    mapM_ (\(ConstructorSig name typ) -> addNewSignature name typ) ctors
resolveDeclaration (SynthesisGoal name impl) = do
    syms <- uses environment allSymbols
    pos <- use currentPosition
    if Map.member name syms
        then goals %= (++ [(name, (impl, pos))])
        else throwResError (text "No specification found for synthesis goal" <+> text name)
resolveDeclaration (MutualDecl names) = mapM_ addMutuals names
    where
        addMutuals name = do
        goalMb <- uses goals (lookup name)
        case goalMb of
            Just _ -> mutuals %= Map.insert name (delete name names)
            Nothing -> throwResError (text "Synthesis goal" <+> text name <+> text "in a mutual clause is undefined")

resolveSignatures :: BareDeclaration -> Resolver ()
resolveSignatures (FuncDecl name _)  = do
    sch <- uses environment ((Map.! name) . allSymbols)
    sch' <- resolveSchema sch
    environment %= removeVariable name
    environment %= addPolyConstant name sch'
resolveSignatures (DataDecl dtName tParams ctors) = mapM_ resolveConstructorSignature ctors
    where
        resolveKind i
            | i == 0 = KnStar
            | i > 0 = KnArr KnStar (resolveKind (i - 1))
        resolveConstructorSignature (ConstructorSig name _) = do
            sch <- uses environment ((Map.! name) . allSymbols)
            sch' <- resolveSchema sch
            let kn = resolveKind (length tParams)
            let nominalType = foldr TyAppT (DatatypeT dtName kn) (map TypeVarT tParams)
            let returnType = lastType (toMonotype sch')
            if nominalType == returnType
                then environment %= addPolyConstant name sch'
                else throwResError (commaSep [text "Constructor" <+> text name <+> text "must return type" <+> pretty nominalType, text "got" <+> pretty returnType])
resolveSignatures (MeasureDecl measureName _ _ post defCases _) = error "measurewhat"
resolveSignatures _                      = return ()

{- Types -}

resolveSchema :: SchemaSkeleton -> Resolver SchemaSkeleton
resolveSchema sch = do
    let tvs = Set.toList $ typeVarsOf (toMonotype sch)
    sch' <- withLocalEnv $ do
        environment . boundTypeVars %= (tvs ++)
        resolveSchema' sch
    return $ Foldable.foldl (flip ForallT) sch' tvs
    where
        resolveSchema' (Monotype t) = Monotype <$> resolveType t

resolveType :: TypeSkeleton -> Resolver TypeSkeleton
resolveType t@(TypeVarT v k) = do
    return t
resolveType (DatatypeT name) = do
    ds <- use $ environment . datatypes
    case Map.lookup name ds of
        Nothing -> substituteTypeSynonym name >>= resolveType -- type synonyms should be substituted before this checking
        Just (DatatypeDef tParams _) -> return $ DatatypeT name
resolveType (TyAppT tFun tArg) = do
    f <- resolveType tFun
    a <- resolveType tArg
    return $ TyAppT f a
resolveType (TyFunT tArg tRes) = do
    a <- resolveType tArg
    r <- resolveType tRes
    return $ TyFunT a r
resolveType (FunctionT x tArg tRes) =
    if x == valueVarName
        then throwResError $ text valueVarName <+> text "is a reserved variable name"
        else if x == dontCare
        then error $ unwords ["resolveType: blank in function type", show (FunctionT x tArg tRes)] -- Should never happen
        else do
            tArg' <- resolveType tArg
            tRes' <- withLocalEnv $ do
                when (not $ isFunctionType tArg') (environment %= addVariable x tArg')
                resolveType tRes
            return $ FunctionT x tArg' tRes'
resolveType AnyT = return AnyT

{- Misc -}

addNewSignature name sch = do
    ifM (Set.member name <$> use (environment . constants)) (throwResError $ text "Duplicate declaration of function" <+> text name) (return ())
    environment %= addPolyConstant name sch
    environment %= addUnresolvedConstant name sch

substituteTypeSynonym name = do
    tss <- use $ environment . typeSynonyms
    case Map.lookup name tss of
        Nothing -> throwResError $ text "Datatype or synonym" <+> text name <+> text "is undefined"
        Just (tVars, t) -> return t

-- | Perform an action and restore the initial environment
withLocalEnv :: Resolver a -> Resolver a
withLocalEnv c = do
  oldEnv <- use environment
  res <- c
  environment .= oldEnv
  return res
