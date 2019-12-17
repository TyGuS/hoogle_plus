{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.Refinement where

import Database.Convert
import Database.Util
import PetriNet.AbstractType
import PetriNet.Util
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Util
import Types.Abstract
import Types.Common
import Types.Environment
import Types.Program
import Types.Solver
import Types.Type

import Control.Lens
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Text.Printf

distinguish :: MonadIO m => Environment -> SType -> SType -> PNSolver m (Maybe (SType, SType))
distinguish env AnyT _ = return Nothing
distinguish env _ AnyT = return Nothing
distinguish env t1 t2 = do
    tass <- gets $ view typeAssignment
    let t1' = var2any env $ stypeSubstitute tass t1
    let t2' = var2any env $ stypeSubstitute tass t2
    distinguish' (env ^. boundTypeVars) t1' t2'

-- | t1 is the expected type, t2 is the actual type
distinguish' :: MonadIO m => [Id] -> SType -> SType -> PNSolver m (Maybe (SType, SType))
distinguish' _ t1 t2 | t1 == t2 = return Nothing
distinguish' _ t1@(ScalarT (DatatypeT id1 tArgs1 _) _) t2@(ScalarT (DatatypeT id2 tArgs2 _) _) | id1 /= id2 = do
    argNames1 <- mapM (\_ -> freshId "A") tArgs1
    let freshArgs1 = map (\n -> ScalarT (TypeVarT Map.empty n) ()) argNames1
    argNames2 <- mapM (\_ -> freshId "A") tArgs2
    let freshArgs2 = map (\n -> ScalarT (TypeVarT Map.empty n) ()) argNames2
    return (Just (ScalarT (DatatypeT id1 freshArgs1 []) (), ScalarT (DatatypeT id2 freshArgs2 []) ()))
distinguish' tvs t1@(ScalarT (DatatypeT id1 tArgs1 _) _) t2@(ScalarT (DatatypeT id2 tArgs2 _) _) | id1 == id2 = do
    diffs <- firstDifference tArgs1 tArgs2
    case diffs of
      ([], []) -> return Nothing
      (ds1, ds2) -> return (Just (ScalarT (DatatypeT id1 ds1 []) (), ScalarT (DatatypeT id2 ds2 []) ()))
  where
    firstDifference [] [] = return ([], [])
    firstDifference (arg:args) (arg':args') = do
        currDiff <- distinguish' tvs arg arg'
        case currDiff of
            Nothing -> do
                argsDiff <- firstDifference args args'
                case argsDiff of
                    ([],[]) -> return ([],[])
                    (diffs1, diffs2) -> do
                        a1 <- flip ScalarT () . TypeVarT Map.empty <$> freshId "A"
                        a2 <- flip ScalarT () . TypeVarT Map.empty <$> freshId "A"
                        return (a1:diffs1, a2:diffs2)
            Just (t1, t2) -> do
                argNames1 <- mapM (\_ -> freshId "A") args
                let freshArgs1 = map (\n -> ScalarT (TypeVarT Map.empty n) ()) argNames1
                argNames2 <- mapM (\_ -> freshId "A") args'
                let freshArgs2 = map (\n -> ScalarT (TypeVarT Map.empty n) ()) argNames2
                return (t1:freshArgs1, t2:freshArgs2)
distinguish' tvs t1@(ScalarT TypeVarT {} _) t2@(ScalarT (DatatypeT id args _) _) = do
    argNames <- mapM (\_ -> freshId "A") args
    let args' = map (\n -> ScalarT (TypeVarT Map.empty n) ()) argNames
    return (Just (t1, ScalarT (DatatypeT id args' []) ()))
distinguish' tvs t1@(ScalarT DatatypeT {} _) t2@(ScalarT (TypeVarT _ id) _) | id `elem` tvs = do
    diffs <- distinguish' tvs t2 t1
    case diffs of
      Nothing -> return Nothing
      Just d -> return (Just (swap d))
distinguish' tvs (ScalarT DatatypeT {} _) (ScalarT (TypeVarT _ id) _) = error "undecided actual type" -- return (Just (ScalarT (TypeVarT Map.empty id) ()))
distinguish' tvs t1@(ScalarT (TypeVarT _ id1) _) t2@(ScalarT (TypeVarT _ id2) _) | id1 `elem` tvs || id2 `elem` tvs = return (Just (t1, t2))
distinguish' tvs (ScalarT TypeVarT {} _) (ScalarT TypeVarT {} _) = return Nothing
distinguish' _ t1 t2 = error $ printf "unhandled case for distinguish %s and %s" (show t1) (show t2)

findSymbol :: MonadIO m => Environment -> Id -> PNSolver m RType
findSymbol env sym = do
    nameMap <- gets $ view nameMapping
    let name = fromMaybe sym (Map.lookup sym nameMap)
    case lookupSymbol name 0 env of
        Nothing ->
            case lookupSymbol ("(" ++ name ++ ")") 0 env of
                Nothing -> do
                    modify $ set isChecked False
                    writeLog 2 "findSymbol" $ text "cannot find symbol" <+> text name <+> text "in the current environment"
                    return AnyT
                Just sch -> freshType sch
        Just sch -> freshType sch

-- | add a new type into our cover and ensure all of them have proper lower bound
updateCover :: [Id] -> AbstractSkeleton -> AbstractCover -> AbstractCover
updateCover tvs t cover = let (_, cover') = updateCover' tvs cover [] t rootNode in cover'

updateCover' :: [Id] -> AbstractCover -> [AbstractSkeleton] -> AbstractSkeleton -> AbstractSkeleton -> ([AbstractSkeleton], AbstractCover)
updateCover' bound cover intscts t paren | equalAbstract bound t paren = (intscts, cover)
updateCover' bound cover intscts t paren | isSubtypeOf bound t paren =
    let children = HashMap.lookupDefault Set.empty paren cover
        child_fun c (ints, acc) = updateCover' bound acc ints t c
        (scts, updatedCover) = Set.foldr child_fun (intscts, cover) children
        lower c = isSubtypeOf bound t c || isSubtypeOf bound c t
        inSubtree = any lower (Set.toList children)
        baseCover = if inSubtree
                      then updatedCover
                      else HashMap.insertWith Set.union paren (Set.singleton t) updatedCover
        int_fun s (ints, acc) = updateCover' bound acc ints s rootNode
     in foldr int_fun ([], baseCover) scts
updateCover' bound cover intscts t paren | isSubtypeOf bound paren t =
    let parents = HashMap.keys $ HashMap.filter (Set.member paren) cover
        rmParen = HashMap.map (Set.delete paren) cover
        addCurr p = HashMap.insertWith Set.union p $ Set.singleton t
        addedCurr = foldr addCurr rmParen parents
        cover' = HashMap.insertWith Set.union t (Set.singleton paren) addedCurr
     in (intscts, cover')
updateCover' bound cover intscts t paren =
    let intsctMb = abstractIntersect bound t paren
     in if isJust intsctMb then (fromJust intsctMb : intscts, cover)
                           else (intscts, cover)

propagate :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m ()
-- | base case, when we reach the leaf of the AST
propagate env p@(Program (PSymbol sym) t) upstream = do
    writeLog 3 "propagate" $ text "propagate" <+> pretty upstream <+> text "into" <+> pretty p
    cover <- gets (view abstractionCover)
    let bound = env ^. boundTypeVars
    unless (existAbstract bound cover upstream)
           (do
                let newCover = updateCover bound upstream cover
                modify $ set abstractionCover newCover
                let newTyps = allTypesOf newCover \\ allTypesOf cover
                modify $ over splitTypes (Set.union $ Set.fromList newTyps)
           )
-- | starter case, when we start from a bottom type
-- find the most general abstraction that unifies with the concrete types
-- of the arguments, but not unify with the function args of its signature
propagate env p@(Program (PApp f args) _) upstream = do
    unless (isBot upstream) (propagate env (Program (PSymbol "x") AnyT) upstream)
    writeLog 3 "propagate" $ text "propagate" <+> pretty upstream <+> text "into" <+> pretty p
    t <- findSymbol env (removeLast '_' f)
    let closedArgs = map (shape . typeOf) args
    let argConcs = map toAbstractType closedArgs
    let absFun = toAbstractType $ shape t
    abstractArgs <- observeT $ mostGeneral argConcs absFun
    mapM_ (uncurry $ propagate env) (zip args abstractArgs)
  where
    mostGeneral cArgs t = do
        let bound = env ^. boundTypeVars
        absArgs <- mapM (generalize bound) cArgs
        lift $ writeLog 3 "propagate" $ text "get generalized types" <+> pretty absArgs <+> text "from" <+> pretty cArgs
        res <- lift $ applySemantic bound t absArgs
        lift $ writeLog 3 "propagate" $ text "apply" <+> pretty absArgs <+> text "to" <+> pretty t <+> text "gets" <+> pretty res
        guard (isSubtypeOf bound res upstream)
        return $ map compactAbstractType absArgs
-- | case for lambda functions
propagate env (Program (PFun x body) (FunctionT _ tArg tRet))
              (AFunctionT atArg atRet) =
    propagate (addVariable x (addTrue tArg) env) body atRet
propagate env (Program (PFun x body) t) (AFunctionT atArg atRet) = do
    id <- freshId "A"
    let tArg = addTrue (ScalarT (TypeVarT Map.empty id) ())
    propagate (addVariable x (addTrue tArg) env) body atRet
propagate _ prog t = return ()

-- bottom up check a program on the concrete type system
-- at the same time, keep track of the abstract type for each node
bottomUpCheck :: MonadIO m => Environment -> RProgram -> PNSolver m RProgram
bottomUpCheck env p@(Program (PSymbol sym) typ) = do
    -- lookup the symbol type in current scope
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    nameMap <- gets $ view nameMapping
    let sym' = removeLast '_' sym
    let name = replaceId hoPostfix "" $ fromMaybe sym' (Map.lookup sym' nameMap)
    t <- findSymbol env name
    return (Program (PSymbol sym) t)
bottomUpCheck env (Program (PApp f args) typ) = do
  argResult <- checkArgs args
  case argResult of
    Left err -> return err
    Right checkedArgs -> do
      t <- findSymbol env (removeLast '_' f)
      -- check function signature against each argument provided
      let argVars = map shape (allArgTypes t)
      let checkedArgTys = map (shape . typeOf) checkedArgs
      mapM_ (uncurry $ solveTypeConstraint env) (zip checkedArgTys argVars)
      -- we eagerly substitute the assignments into the return type of t
      tass <- gets (view typeAssignment)
      let ret = addTrue $ stypeSubstitute tass (shape $ lastType t)
      -- if any of these checks returned false, this function application
      -- would produce a bottom type
      ifM (gets $ view isChecked)
          (return $ Program (PApp f checkedArgs) ret)
          (return $ Program (PApp f checkedArgs) BotT)
  where
    checkArgs [] = return $ Right []
    checkArgs (arg:args) = do
        checkedArg <- bottomUpCheck env arg
        ifM (gets $ view isChecked)
            (do
               checkedArgs <- checkArgs args
               case checkedArgs of
                 Left err -> return $ Left err
                 Right args' -> return $ Right (checkedArg:args')
            )
            (return $ Left checkedArg)
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    body' <- bottomUpCheck (addVariable x (addTrue tArg) env) body
    let tBody = typeOf body'
    let t = FunctionT x tArg tBody
    ifM (gets $ view isChecked)
        (return $ Program (PFun x body') t)
        (return body')
bottomUpCheck env p@(Program (PFun x body) _) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    id <- freshId "A"
    id' <- freshId "A"
    let tArg = addTrue (ScalarT (TypeVarT Map.empty id) ())
    let tRet = addTrue (ScalarT (TypeVarT Map.empty id') ())
    bottomUpCheck env (Program (PFun x body)(FunctionT x tArg tRet))
bottomUpCheck _ p = error $ "unhandled case for checking "
                          ++ show p ++ "::" ++ show (typeOf p)

solveTypeConstraint :: MonadIO m => Environment -> SType -> SType -> PNSolver m ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _)
  | id == id' = return ()
  | isBound env id && isBound env id' = modify $ set isChecked False
  | isBound env id = do
    st <- get
    if id' `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty tv
            solveTypeConstraint env tv typ
        else unify env id' tv
  | otherwise = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty tv'
            solveTypeConstraint env typ tv'
        else if id' `Map.member` (st ^. typeAssignment)
            then do
                let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
                writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty typ
                solveTypeConstraint env tv typ
            else unify env id tv'
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    st <- get
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty t
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
            solveTypeConstraint env typ t
        else unify env id t
solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tArg <+> text "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tRet <+> text "==" <+> pretty tRet'
    solveTypeConstraint env tRet tRet'
solveTypeConstraint env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id /= id' =
    modify $ set isChecked False
solveTypeConstraint env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' =
    solveTypeConstraint' env tArgs tArgs'
  where
    solveTypeConstraint' _ []  [] = return ()
    solveTypeConstraint' env (ty:tys) (ty':tys') = do
        writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty ty <+> text "==" <+> pretty ty'
        solveTypeConstraint env ty ty'
        checked <- gets $ view isChecked
        -- if the checking between ty and ty' succeeds, proceed to others
        when checked $ solveTypeConstraint' env tys tys'
solveTypeConstraint env t1 t2 = do
    writeLog 3 "solveTypeConstraint" $ text "unmatched types" <+> pretty t1 <+> text "and" <+> pretty t2
    modify $ set isChecked False

-- | unify the type variable with some given type
-- add the type assignment to our state
unify :: MonadIO m => Environment -> Id -> SType -> PNSolver m ()
unify env v t =
    if v `Set.member` typeVarsOf t
      then modify $ set isChecked False
      else do
        tass' <- gets $ view typeAssignment
        writeLog 3 "unify" $ text (show tass')
        modify $ over typeAssignment (Map.map (stypeSubstitute (Map.singleton v t)))
        tass <- gets $ view typeAssignment
        modify $ over typeAssignment (Map.insert v (stypeSubstitute tass t))

-- | generalize a closed concrete type into an abstract one
generalize :: MonadIO m => [Id] -> AbstractSkeleton -> LogicT (PNSolver m) AbstractSkeleton
generalize bound t@(AScalar (ATypeVarT id))
  | id `notElem` bound = return t
  | otherwise = do
    v <- lift $ freshId "T"
    return (AScalar (ATypeVarT v)) `mplus` return t
-- for datatype, we define the generalization order as follows:
-- (1) v
-- (2) datatype with all fresh type variables
-- (3) datatype with incrementally generalized inner types
generalize bound t@(AScalar (ADatatypeT id args)) = do
    v <- lift $ freshId "T"
    return (AScalar (ATypeVarT v)) `mplus` freshVars `mplus` subsetTyps -- interleave
  where
    -- this search may explode when we have a large number of datatype parameters
    patternOfLen n
      | n == 0 = mzero
      | n == 1 = return [n]
      | n >  1 = do
          let nextNumber l = 1 + maximum l
          let candidates l = nextNumber l : nub l
          prevPat <- patternOfLen (n - 1)
          msum $ map (\c -> return (c:prevPat)) (candidates prevPat)

    freshVars = do
        let n = length args
        pat <- patternOfLen n
        let argNames = map (\i -> "T" ++ show i) pat
        let args' = map (AScalar . ATypeVarT) argNames
        absTy <- lift $ freshAbstract bound (AScalar (ADatatypeT id args'))
        guard (isSubtypeOf bound t absTy)
        lift $ writeLog 3 "generalize" $ text "generalize" <+> pretty t <+> text "into" <+> pretty absTy
        return absTy

    subsets [] = return []
    subsets (arg:args) = do
        args' <- subsets args
        arg' <- generalize bound arg
        return (arg':args')

    subsetTyps = do
        args' <- subsets args
        return (AScalar (ADatatypeT id args'))

generalize bound (AFunctionT tArg tRes) = do
    tArg' <- generalize bound tArg
    tRes' <- generalize bound tRes
    return (AFunctionT tArg' tRes')
