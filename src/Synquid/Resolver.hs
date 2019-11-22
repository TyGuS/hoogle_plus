{-# LANGUAGE TupleSections, FlexibleContexts, TemplateHaskell #-}

-- | Functions for processing the AST created by the Parser (eg filling in unknown types, verifying that refinement formulas evaluate to a boolean, etc.)
module Synquid.Resolver (
    resolveDecls
  , substituteTypeSynonym
  , ResolverState (..)
  , initResolverState
  , resolveSchema) where

import Synquid.Error
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Util
import Types.Common
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
import Debug.Trace

{- Interface -}

data ResolverState = ResolverState {
    _environment :: Environment,
    _goals :: [(Id, (UProgram, SourcePos))],
    _currentPosition :: SourcePos,
    _idCount :: Int,
    -- temporary state
    _kindAssignment :: Map Id Kind,
    _kindConstraints :: [(Kind, Kind)]
}

makeLenses ''ResolverState

initResolverState = ResolverState {
    _environment = emptyEnv,
    _goals = [],
    _currentPosition = noPos,
    _idCount = 0,
    _kindAssignment = Map.empty,
    _kindConstraints = []
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

{- Implementation -}

type Resolver a = StateT ResolverState (Except ErrorMessage) a

throwResError descr = do
    pos <- use currentPosition
    throwError $ ErrorMessage ResolutionError pos descr

resolveDeclaration :: BareDeclaration -> Resolver ()
resolveDeclaration (TypeDecl typeSynonym typeBody) = do
    typeBody' <- resolveKindAndType typeBody (KnVar "K")
    environment %= addTypeSynonym typeSynonym typeBody'
resolveDeclaration (FuncDecl funcName typeSchema) = addNewSignature funcName typeSchema
resolveDeclaration (DataDecl dtName tParams ctors) = do
    let
        datatype = DatatypeDef {
        _typeParams = tParams,
        _constructors = map constructorName ctors
        }
    environment %= addDatatype dtName datatype
    mapM_ (\(ConstructorSig name typ) -> addNewSignature name $ Monotype typ) ctors
resolveDeclaration (SynthesisGoal name impl) = do
    syms <- uses environment allSymbols
    pos <- use currentPosition
    if Map.member name syms
        then goals %= (++ [(name, (impl, pos))])
        else throwResError (text "No specification found for synthesis goal" <+> text name)

resolveSignatures :: BareDeclaration -> Resolver ()
resolveSignatures (FuncDecl name _)  = do
    sch <- uses environment ((Map.! name) . allSymbols)
    sch' <- resolveSchema sch
    environment %= removeVariable name
    environment %= addPolyConstant name sch'
resolveSignatures (DataDecl dtName tParams ctors) = mapM_ resolveConstructorSignature ctors
    where
        resolveConstructorSignature (ConstructorSig name _) = do
            sch <- uses environment ((Map.! name) . allSymbols)
            sch' <- resolveSchema sch
            environment %= addPolyConstant name sch'
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
        resolveSchema' (Monotype t) = Monotype <$> resolveKindAndType t KnStar

-- we need to resolveType twice so that the kind information can be propagated
-- is there any more efficient algorithm for this?
resolveType :: TypeSkeleton -> Kind -> Resolver TypeSkeleton
resolveType t@(TypeVarT v _) k = do
    kindConstraints %= ((KnVar v, k):)
    return $ TypeVarT v k
resolveType t@(DatatypeT name _) k = do
    ds <- use $ environment . datatypes
    case Map.lookup name ds of
        Nothing -> do
            trySynonym <- substituteTypeSynonym t
            case trySynonym of
                Nothing -> throwResError $ text name <+> text "is not a defined datatype"
                Just t' -> resolveType t' k
        Just (DatatypeDef tParams _) -> do
            k' <- mkVarKind (length tParams) KnStar
            kindConstraints %= ((k, k'):)
            return $ DatatypeT name k
    where
        mkVarKind 0 acc = return acc
        mkVarKind i acc = do
            k <- freshK
            mkVarKind (i-1) (KnArr (KnVar k) acc)
resolveType t@(TyAppT tFun tArg _) k = do
    trySynonym <- substituteTypeSynonym t
    case trySynonym of
        Nothing -> do
            kn <- case tArg of
                TypeVarT v _ -> return (KnVar v)
                _ -> KnVar <$> freshK
            f <- resolveType tFun (KnArr kn k)
            a <- resolveType tArg kn
            return $ TyAppT f a k
        Just t' -> resolveType t' k
resolveType (TyFunT tArg tRes) KnStar = do
    a <- resolveType tArg KnStar
    r <- resolveType tRes KnStar
    return $ TyFunT a r
resolveType (FunctionT x tArg tRes) KnStar =
    if x == varName
        then throwResError $ text varName <+> text "is a reserved variable name"
        else do
            tArg' <- resolveType tArg KnStar
            tRes' <- withLocalEnv $ do
                when (not $ isFunctionType tArg') (environment %= addVariable x tArg')
                resolveType tRes KnStar
            return $ FunctionT x tArg' tRes'
resolveType AnyT _ = return AnyT
resolveType t k = kindConstraints %= ((k, KnStar):) >> return t
    --throwResError $ pretty t <+> text "should not have kind" <+> pretty k

{- Misc -}

addNewSignature name sch = do
    ifM (Set.member name <$> use (environment . constants)) (throwResError $ text "Duplicate declaration of function" <+> text name) (return ())
    environment %= addPolyConstant name sch
    environment %= addUnresolvedConstant name sch

substituteTypeSynonym name = do
    tss <- use $ environment . typeSynonyms
    return $ Map.lookup name tss 

-- | Perform an action and restore the initial environment
withLocalEnv :: Resolver a -> Resolver a
withLocalEnv c = do
  oldEnv <- use environment
  res <- c
  environment .= oldEnv
  return res

freshK :: Resolver String
freshK = do
    k <- ("K" ++) . show <$> gets (view idCount)
    modify $ over idCount (+1)
    return k

resolveKindAndType :: TypeSkeleton -> Kind -> Resolver TypeSkeleton
resolveKindAndType t k = do
    -- traceShow t (return ())
    t' <- resolveType t k
    solveAllKind
    solveAllAssignment
    kass <- use kindAssignment
    let t = substituteKindInType kass t'
    -- traceShow t (return ())
    kindAssignment .= Map.empty
    return t

solveAllKind :: Resolver ()
solveAllKind = do
    kass <- use kindAssignment
    kcs <- use kindConstraints
    -- traceShow kcs (return ())
    kindConstraints .= []
    mapM_ solveKind kcs
    -- if we get new type assignments during the constraint solving
    kass' <- use kindAssignment
    -- traceShow kass' (return ())
    when (Map.size kass' > Map.size kass) solveAllKind

solveAllAssignment :: Resolver ()
solveAllAssignment = do
    kass <- use kindAssignment
    mapM_ (uncurry solveAssignment) (Map.toList kass)

solveAssignment :: Id -> Kind -> Resolver ()
solveAssignment v k = do
    kass <- use kindAssignment
    kindAssignment %= Map.insert v (substituteKind kass k)

solveKind :: (Kind, Kind) -> Resolver ()
solveKind (k1, k2) | k1 == k2 = return ()
solveKind (KnVar v1, KnVar v2) = do
    kass <- use kindAssignment
    if v1 `Map.member` kass
        then solveKind (KnVar v2, kass Map.! v1)
        else if v2 `Map.member` kass 
            then solveKind (KnVar v1, kass Map.! v2)
            else kindConstraints %= ((KnVar v1, KnVar v2):)
solveKind (KnVar v1, k) = do
    kass <- use kindAssignment
    if v1 `Map.member` kass 
        then solveKind (kass Map.! v1, k)
        else kindAssignment %= Map.insert v1 k
solveKind (k, KnVar k2) = solveKind (KnVar k2, k)
solveKind (KnStar, KnStar) = return ()
solveKind (KnArr k1 k2, KnArr k1' k2') = do
    solveKind (k1, k1')
    solveKind (k2, k2')
solveKind (k1, k2) = throwResError $ pretty k1 <+> text "cannot unify with" <+> pretty k2