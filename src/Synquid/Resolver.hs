{-# LANGUAGE TupleSections, FlexibleContexts, TemplateHaskell #-}

-- | Functions for processing the AST created by the Parser (eg filling in unknown types, verifying that refinement formulas evaluate to a boolean, etc.)
module Synquid.Resolver
    ( resolveDecls
    , addAllVariables
    , substituteTypeSynonym
    , ResolverState(..)
    , instantiateSorts
    , initResolverState
    , resolveSchema
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Either hiding (fromRight)
import qualified Data.Foldable as Foldable
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import Synquid.Error
import Synquid.Logic
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Util
import Types.Common hiding (varName)
import Types.Environment
import Types.Generate
import Types.Program
import Types.Type

{- Interface -}
data ResolverState =
    ResolverState
        { _environment :: Environment
        , _goals :: [(Id, (UProgram, SourcePos))]
        , _condQualifiers :: [Formula]
        , _typeQualifiers :: [Formula]
        , _mutuals :: Map Id [Id]
        , _inlines :: Map Id ([Id], Formula)
        , _sortConstraints :: [SortConstraint]
        , _currentPosition :: SourcePos
        , _idCount :: Int
        }

makeLenses ''ResolverState

initResolverState =
    ResolverState
        { _environment = emptyEnv
        , _goals = []
        , _condQualifiers = []
        , _typeQualifiers = []
        , _mutuals = Map.empty
        , _inlines = Map.empty
        , _sortConstraints = []
        , _currentPosition = noPos
        , _idCount = 0
        }

-- | Convert a parsed program AST into a list of synthesis goals and qualifier maps
resolveDecls :: [Declaration] -> [MdlName] -> Either ErrorMessage Environment
resolveDecls declarations moduleNames =
    runExcept (execStateT go initResolverState) >>= (\s -> Right (s ^. environment))
  where
    go
      -- Pass 1: collect all declarations and resolve sorts, but do not resolve refinement types yet
     = do
        mapM_ (extractPos resolveDeclaration) declarations
      -- Pass 2: resolve refinement types in signatures
        mapM_ (extractPos resolveSignatures) declarations
    extractPos pass (Pos pos decl) = do
        currentPosition .= pos
        pass decl

instantiateSorts :: [Sort] -> [Sort]
instantiateSorts sorts = fromRight $ runExcept (evalStateT (instantiate sorts) (initResolverState))

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
        else throwResError
                 (text "Type variable(s)" <+> hsep (map text $ Set.toList extraTypeVars) <+>
                  text "in the definition of type synonym" <+>
                  text typeName <+>
                  text "are undefined")
resolveDeclaration (FuncDecl funcName typeSchema) = addNewSignature funcName typeSchema
resolveDeclaration (DataDecl mdl dtName tParams pVarParams ctors) = do
    let (pParams, pVariances) = unzip pVarParams
        datatype =
            DatatypeDef
                { _typeParams = tParams
                , _predVariances = pVariances
                , _constructors = map constructorName ctors
                , _srcModule = mdl
                }
    environment %= addDatatype dtName datatype
    let addPreds typ = foldl (flip ForallP) (Monotype typ) pParams
    mapM_ (\(ConstructorSig name typ) -> addNewSignature name $ addPreds typ) ctors
resolveDeclaration (MeasureDecl measureName inSort outSort post defCases isTermination) =
    error "resolveDeclaration"
resolveDeclaration (PredDecl (PredSig name argSorts resSort)) = error "resolveDecl"
resolveDeclaration (SynthesisGoal name impl) = do
    syms <- uses environment allSymbols
    pos <- use currentPosition
    if Map.member name syms
        then goals %= (++ [(name, (impl, pos))])
        else throwResError (text "No specification found for synthesis goal" <+> text name)
resolveDeclaration (QualifierDecl quals) = mapM_ resolveQualifier quals
  where
    resolveQualifier q =
        if Set.member valueVarName (Set.map varName $ varsOf q)
            then typeQualifiers %= (q :)
            else condQualifiers %= (q :)
resolveDeclaration (MutualDecl names) = mapM_ addMutuals names
  where
    addMutuals name = do
        goalMb <- uses goals (lookup name)
        case goalMb of
            Just _ -> mutuals %= Map.insert name (delete name names)
            Nothing ->
                throwResError
                    (text "Synthesis goal" <+> text name <+> text "in a mutual clause is undefined")
resolveDeclaration (InlineDecl name args body) =
    ifM (uses inlines (Map.member name))
        (throwResError (text "Duplicate definition of inline" <+> text name))
        (do let extraVars = Set.map varName (varsOf body) `Set.difference` Set.fromList args
            if not $ Set.null extraVars
                then throwResError
                         (text "Variables" <+> hsep (map text $ Set.toList extraVars) <+>
                          text "undefined in the body of inline" <+>
                          text name)
                else inlines %= Map.insert name (args, body))

resolveSignatures :: BareDeclaration -> Resolver ()
resolveSignatures (FuncDecl name _) = do
    sch <- uses environment ((Map.! name) . allSymbols)
    sch' <- resolveSchema sch
    environment %= removeVariable name
    environment %= addPolyConstant name sch'
resolveSignatures (DataDecl _ dtName tParams pParams ctors) = mapM_ resolveConstructorSignature ctors
  where
    resolveConstructorSignature (ConstructorSig name _) = do
        sch <- uses environment ((Map.! name) . allSymbols)
        sch' <- resolveSchema sch
        let nominalType =
                ScalarT
                    (DatatypeT dtName (map vartAll tParams) (map nominalPredApp (map fst pParams)))
                    ftrue
        let returnType = lastType (toMonotype sch')
        if nominalType == returnType
            then do
                let nominalSort = toSort $ baseTypeOf nominalType
                let sch'' =
                        addRefinementToLastSch
                            sch'
                            (Var nominalSort valueVarName |=|
                             Cons nominalSort name (allArgs (toMonotype sch')))
                environment %= addPolyConstant name sch''
            else throwResError
                     (commaSep
                          [ text "Constructor" <+> text name <+> text "must return type" <+>
                            pretty nominalType
                          , text "got" <+> pretty returnType
                          ])
resolveSignatures (MeasureDecl measureName _ _ post defCases _) = error "measurewhat"
resolveSignatures _ = return ()

{- Types and sorts -}
resolveSchema :: RSchema -> Resolver RSchema
resolveSchema sch = do
    let tvs = Set.toList $ typeVarsOf (toMonotype sch)
    sch' <-
        withLocalEnv $ do
            environment . boundTypeVars %= (tvs ++)
            resolveSchema' sch
    return $ Foldable.foldl (flip ForallT) sch' tvs
  where
    resolveSchema' (ForallP sig@(PredSig predName argSorts resSort) sch) = do
        mapM_ resolveSort argSorts
        when (resSort /= BoolS) $
            (throwResError $ text "Bound predicate variable" <+> text predName <+>
             text "must return Bool")
        sch' <-
            withLocalEnv $
        -- environment %= addBoundPredicate sig
             do resolveSchema' sch
        let extraTypeVars =
                (Set.unions (map typeVarsOfSort argSorts)) Set.\\ typeVarsOf (toMonotype sch')
        when (not $ Set.null extraTypeVars) $
            (throwResError $ text "Unbound variables" <+>
             (commaSep $ map pretty $ Set.toList extraTypeVars) <+>
             text "in sort of bound predicate" <+>
             text predName)
        return $ ForallP sig sch'
    resolveSchema' (Monotype t) = Monotype <$> resolveType t

resolveType :: RType -> Resolver RType
resolveType (ScalarT (DatatypeT name tArgs pArgs) fml) = do
    ds <- use $ environment . datatypes
    case Map.lookup name ds of
        Nothing -> do
            t' <- substituteTypeSynonym name tArgs >>= resolveType
            fml' <- resolveTypeRefinement (toSort $ baseTypeOf t') fml
            return $ addRefinement t' fml'
        Just (DatatypeDef tParams _ _ _) -> do
            when (length tArgs /= length tParams) $ throwResError $ text "Datatype" <+> text name <+>
                text "expected" <+>
                pretty (length tParams) <+>
                text "type arguments and got" <+>
                pretty (length tArgs)
      -- Resolve type arguments:
            tArgs' <- mapM resolveType tArgs
      -- Resolve predicate arguments:
            let subst = noncaptureSortSubst tParams (map (toSort . baseTypeOf) tArgs')
            let baseT' = DatatypeT name tArgs' pArgs
      -- Resolve refinementL
            fml' <- resolveTypeRefinement (toSort baseT') fml
            return $ ScalarT baseT' fml'
  where
    resolvePredArg :: (Sort -> Sort) -> PredSig -> Formula -> Resolver Formula
    resolvePredArg subst (PredSig _ argSorts BoolS) fml =
        withLocalEnv $ do
            let argSorts' = map subst argSorts
            let vars = zipWith Var argSorts' deBrujns
            environment %= addAllVariables vars
            case fml of
                Pred _ p [] -> resolveTypeRefinement AnyS (Pred BoolS p vars)
                _ -> resolveTypeRefinement AnyS fml
resolveType (ScalarT baseT fml) = ScalarT baseT <$> resolveTypeRefinement (toSort baseT) fml
resolveType (FunctionT x tArg tRes) =
    if x == valueVarName
        then throwResError $ text valueVarName <+> text "is a reserved variable name"
        else if x == dontCare
                 then error $
                      unwords ["resolveType: blank in function type", show (FunctionT x tArg tRes)] -- Should never happen
                 else do
                     tArg' <- resolveType tArg
                     tRes' <-
                         withLocalEnv $ do
                             when (not $ isFunctionType tArg') (environment %= addVariable x tArg')
                             resolveType tRes
                     return $ FunctionT x tArg' tRes'
resolveType AnyT = return AnyT

-- | Check that sort has no unknown datatypes
resolveSort :: Sort -> Resolver ()
resolveSort (SetS elSort) = resolveSort elSort
resolveSort s@(DataS name sArgs) = do
    ds <- use $ environment . datatypes
    case Map.lookup name ds of
        Nothing ->
            throwResError $ text "Datatype" <+> text name <+> text "is undefined in sort" <+>
            pretty s
        Just (DatatypeDef tParams _ _ _) -> do
            let n = length tParams
            when (length sArgs /= n) $ throwResError $ text "Datatype" <+> text name <+>
                text "expected" <+>
                pretty n <+>
                text "type arguments and got" <+>
                pretty (length sArgs)
            mapM_ resolveSort sArgs
resolveSort s = return ()

{- Formulas -}
-- | 'resolveTypeRefinement' @valueSort fml@ : resolve @fml@ as a refinement with _v of sort @valueSort@;
-- when @valueSort@ is @AnyS@, _v must not occur
resolveTypeRefinement :: Sort -> Formula -> Resolver Formula
resolveTypeRefinement _ (BoolLit True) = return $ BoolLit True -- Special case to allow undefined value sort for function types
resolveTypeRefinement valueSort fml = error "resolveTypeRefinement"

resolveFormula :: Formula -> Resolver Formula
resolveFormula fml = return fml

{- Misc -}
nominalPredApp (PredSig pName argSorts resSort) = Pred resSort pName (zipWith Var argSorts deBrujns)

solveSortConstraints :: Resolver SortSubstitution
solveSortConstraints = do
    (unificationCs, typeClassCs) <- uses sortConstraints (partition isSameSortConstraint)
    tvs <- uses (environment . boundTypeVars) Set.fromList
    sortConstraints .= []
    idCount .= 0
    let (sls, srs) = unzip $ map (\(SameSort s1 s2) -> (s1, s2)) unificationCs
    subst <-
        case unifySorts tvs sls srs of
            Left (x, y) ->
                throwResError $ text "Cannot unify sorts" <+> pretty x <+> text "and" <+> pretty y
            Right subst -> return subst
    mapM_ (checkTypeClass subst) typeClassCs
    return subst
  where
    isSameSortConstraint (SameSort _ _) = True
    isSameSortConstraint _ = False
    checkTypeClass subst (IsOrd s) =
        let s' = sortSubstitute subst s
         in case s' of
                IntS -> return ()
                VarS _ -> return ()
                _ -> throwResError $ text "Sort" <+> pretty s' <+> text "is not ordered"

addNewSignature name sch = do
    ifM (Set.member name <$> use (environment . constants))
        (throwResError $ text "Duplicate declaration of function" <+> text name)
        (return ())
    environment %= addPolyConstant name sch
    environment %= addUnresolvedConstant name sch

substituteTypeSynonym name tArgs = do
    tss <- use $ environment . typeSynonyms
    case Map.lookup name tss of
        Nothing -> throwResError $ text "Datatype or synonym" <+> text name <+> text "is undefined"
        Just (tVars, t) -> do
            when (length tArgs /= length tVars) $ throwResError $ text "Type synonym" <+> text name <+>
                text "expected" <+>
                pretty (length tVars) <+>
                text "type arguments and got" <+>
                pretty (length tArgs)
            return $ noncaptureTypeSubst tVars tArgs t

-- | 'freshId' @prefix@ : fresh identifier starting with @prefix@
freshSort :: Resolver Sort
freshSort = do
    i <- use idCount
    idCount %= (+ 1)
    return $ VarS ("S" ++ show i)

-- | 'instantiate' @sorts@: replace all sort variables in @sorts@ with fresh sort variables
instantiate :: [Sort] -> Resolver [Sort]
instantiate sorts = do
    let tvs = Set.toList $ Set.unions (map typeVarsOfSort sorts)
    freshTvs <- replicateM (length tvs) freshSort
    return $ map (sortSubstitute $ Map.fromList $ zip tvs freshTvs) sorts

enforceSame :: Sort -> Sort -> Resolver ()
enforceSame sl sr
    | sl == sr = return ()
    | otherwise = sortConstraints %= (++ [SameSort sl sr])

-- | Perform an action and restore the initial environment
withLocalEnv :: Resolver a -> Resolver a
withLocalEnv c = do
    oldEnv <- use environment
    res <- c
    environment .= oldEnv
    return res
