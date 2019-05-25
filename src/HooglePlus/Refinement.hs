module HooglePlus.Refinement where

import Types.Abstract
import Types.Common
import HooglePlus.Abstraction
import PetriNet.AbstractType
import Types.Environment
import Types.Type
import Types.Solver
import Types.Program
import PetriNet.Util
import Synquid.Type
import Synquid.Program
import Synquid.Pretty
import Synquid.Util
import Database.Convert

import Control.Monad.State
import qualified Data.Set as Set
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Data.List
import Data.Tuple (swap)
import Data.Maybe

distinguish :: MonadIO m => Environment -> SType -> SType -> PNSolver m (Maybe (SType, SType))
distinguish env AnyT _ = return Nothing
distinguish env _ AnyT = return Nothing
distinguish env t1 t2 = do
    tass <- view typeAssignment <$> get
    let t1' = var2any env (stypeSubstitute tass t1)
    let t2' = var2any env (stypeSubstitute tass t2)
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
distinguish' tvs t1@(ScalarT (TypeVarT {}) _) t2@(ScalarT (DatatypeT id args _) _) = do
    argNames <- mapM (\_ -> freshId "A") args
    let args' = map (\n -> ScalarT (TypeVarT Map.empty n) ()) argNames
    return (Just (t1, ScalarT (DatatypeT id args' []) ()))
distinguish' tvs t1@(ScalarT (DatatypeT {}) _) t2@(ScalarT (TypeVarT _ id) _) | id `elem` tvs = do
    diffs <- distinguish' tvs t2 t1
    case diffs of
      Nothing -> return Nothing
      Just d -> return (Just (swap d))
distinguish' tvs (ScalarT (DatatypeT {}) _) (ScalarT (TypeVarT _ id) _) = error "undecided actual type" -- return (Just (ScalarT (TypeVarT Map.empty id) ()))
distinguish' tvs t1@(ScalarT (TypeVarT _ id1) _) t2@(ScalarT (TypeVarT _ id2) _) | id1 `elem` tvs || id2 `elem` tvs = return (Just (t1, t2))
distinguish' tvs (ScalarT (TypeVarT {}) _) (ScalarT (TypeVarT {}) _) = return Nothing
distinguish' _ t1 t2 = error $ printf "unhandled case for distinguish %s and %s" (show t1) (show t2)

findSymbol :: MonadIO m => Environment -> Id -> PNSolver m RType
findSymbol env sym = do
    nameMap <- view nameMapping <$> get
    let name = fromMaybe sym (Map.lookup sym nameMap)
    case lookupSymbol name 0 env of
        Nothing -> do
            case lookupSymbol ("(" ++ name ++ ")") 0 env of
                Nothing -> do
                    modify $ set isChecked False
                    writeLog 2 $ text "cannot find symbol" <+> text name <+> text "in the current environment"
                    return AnyT
                Just sch -> freshType sch
        Just sch -> freshType sch

strengthenRoot :: MonadIO m => Environment -> AbstractSkeleton -> SType -> SType -> PNSolver m AbstractSkeleton
strengthenRoot env dfault expected actual = do
    diff <- distinguish env expected actual
    semantic <- gets (view abstractionTree)
    writeLog 3 $ text "strengthen root with expected" <+> pretty expected <+> text "and actual" <+> pretty actual <+> text "and get" <+> pretty diff
    case diff of
      Nothing -> error $ "same expected and actual type" 
      Just (ed, ad) -> do
          let bound = env ^. boundTypeVars
          -- find an abstraction that unifies both the current abstraction and the distinguished type
          absTyp <- freshAbstract bound (toAbstractType ad)
          eabsTyp <- freshAbstract bound (toAbstractType ed)
          let constraints = [(absTyp, dfault)]
          let unifier = getUnifier bound constraints
          case unifier of
              Nothing -> do
                  writeLog 3 $ text "distinguished type" <+> pretty absTyp <+> text "does not unify the current abstraction" <+> pretty dfault
                  when (and (map (not . equalAbstract bound absTyp) (Set.toList semantic)))
                       (modify $ over splitTypes (nubBy (equalAbstract bound) . (:) absTyp))
                  when (and (map (not . equalAbstract bound eabsTyp) (Set.toList semantic)))
                       (modify $ over splitTypes (nubBy (equalAbstract bound) . (:) eabsTyp))
                  return absTyp
              Just u -> do
                  let newAbsTyp = foldr (uncurry abstractSubstitute) absTyp (Map.toList u)
                  when (and (map (not . equalAbstract bound newAbsTyp) (Set.toList semantic)))
                       (modify $ over splitTypes (nubBy (equalAbstract bound) . (:) newAbsTyp))
                  when (and (map (not . equalAbstract bound eabsTyp) (Set.toList semantic)))
                       (modify $ over splitTypes (nubBy (equalAbstract bound) . (:) eabsTyp))
                  return newAbsTyp

propagate :: MonadIO m => Environment -> AProgram -> AbstractSkeleton -> PNSolver m (Maybe AbstractSkeleton)
propagate env (Program (PSymbol sym) (_, _, local)) upstream = do
    semantic <- gets (view abstractionTree)
    sigs <- gets (view currentSigs)
    -- try to unify the upstream type with the current return type
    t <- findSymbol env (removeLast '_' sym)
    let bound = env ^. boundTypeVars
    let resTyp = toAbstractType (shape (lastType t))
    let localTyp = lastAbstract local
    let upstreamTyp = upstream
    let consTyps = [localTyp, upstreamTyp]
    v <- freshId "v"
    constraints <- mapM (mkConstraint bound v) consTyps
    let unifier = getUnifier bound constraints
    case unifier of
        Nothing -> do
            writeLog 3 $ pretty localTyp <+> text "and" <+> pretty upstreamTyp <+> text "does not unify"
            let absTyp = upstreamTyp
            when (and (map (not . equalAbstract bound absTyp) (Set.toList semantic)))
                 (modify $ over splitTypes (nubBy (equalAbstract bound) . (:) absTyp))
            let tass' = fromJust (getUnifier bound [(resTyp, absTyp)])
            let t' = foldl' (\acc (id, tt) -> abstractSubstitute id tt acc) (toAbstractType (shape t)) (Map.toList tass')
            return (Just t')
        Just tass -> do
            let absTyp = foldl' (\acc (id, tt) -> abstractSubstitute id tt acc) (AScalar (ATypeVarT v)) (Map.toList tass)
            writeLog 3 $ text "backprop get" <+> pretty absTyp <+> text "from upstream" <+> pretty upstream <+> text "and local" <+> pretty localTyp
            when (and (map (not . equalAbstract bound absTyp) (Set.toList semantic)))
                 (modify $ over splitTypes (nubBy (equalAbstract bound) . (:) absTyp))
            let tass' = fromJust (getUnifier bound [(resTyp, absTyp)])
            let t' = foldl' (\acc (id, tt) -> abstractSubstitute id tt acc) (toAbstractType (shape t)) (Map.toList tass')
            return (Just t')
propagate env (Program (PApp pFun pArg) (_, _, local)) upstream = do
    maybeFun <- propagate env pFun upstream
    case maybeFun of
      Nothing -> return Nothing
      Just (AFunctionT tArg tRet) -> do
          maybeArg <- propagate env pArg tArg
          case maybeArg of
            Nothing -> return Nothing
            Just t -> return (Just tRet)
      t -> error $ "unexpected type pattern " ++ show t
propagate env (Program (PFun x body) (FunctionT _ tArg tRet, _, _)) (AFunctionT atArg atRet) = do
    maybeRet <- propagate (addVariable x (addTrue tArg) env) body atRet
    case maybeRet of
      Nothing -> return Nothing
      Just atRet' -> return (Just (AFunctionT atArg atRet'))
propagate env (Program (PFun x body) t) (AFunctionT atArg atRet) = do
    id <- freshId "A"
    let tArg = addTrue (ScalarT (TypeVarT Map.empty id) ())
    maybeRet <- propagate (addVariable x (addTrue tArg) env) body atRet
    case maybeRet of
      Nothing -> return Nothing
      Just atRet' -> return (Just (AFunctionT atArg atRet'))
propagate _ prog t = return Nothing

-- bottom up check a program on the concrete type system
-- at the same time, keep track of the abstract type for each node
bottomUpCheck :: MonadIO m => Environment -> RProgram -> PNSolver m AProgram
bottomUpCheck env p@(Program (PSymbol sym) typ) = do
    -- lookup the symbol type in current scope
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    t <- findSymbol env (removeLast '_' sym)
    sigs <- view currentSigs <$> get
    params <- view paramNames <$> get
    srcs <- view sourceTypes <$> get
    -- add arguments into this map
    let sigs' = foldr (uncurry Map.insert) sigs (zip params srcs)
    let n = fromMaybe (error $ "cannot find symbol " ++ sym ++ " in signatures")
                      (Map.lookup sym sigs')
    return (Program (PSymbol sym) (t, typ, n))
bottomUpCheck env (Program (PApp pFun pArg) typ) = do
    arg <- bottomUpCheck env pArg
    ifM (view isChecked <$> get)
        (do
            fun <- bottomUpCheck env pFun
            ifM (view isChecked <$> get)
                (do
                    let (tFun, tFun', tf) = typeOf fun
                    let FunctionT _ tArg tRet = tFun
                    let FunctionT _ tArg' tRet' = tFun'
                    let AFunctionT atArg atRet = tf
                    let Program pArg (cArg, cArg', _) = arg
                    writeLog 3 $ text "Solving constraint for" <+> pretty arg 
                               <+> text "::" <+> pretty (shape cArg) 
                               <+> text "==" <+> pretty (shape tArg)
                    solveTypeConstraint env (shape cArg) (shape tArg)
                    ifM (view isChecked <$> get)
                        (return (Program (PApp fun arg) (tRet, tRet', atRet)))
                        (return (Program pArg (cArg, tArg, atArg))))
                (return fun))
        (return arg)
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    body' <- bottomUpCheck (addVariable x (addTrue tArg) env) body
    let (tBody, _, at) = typeOf body'
    let t = FunctionT x tArg tBody
    ifM (view isChecked <$> get)
        (return (Program (PFun x body') (t, t, AFunctionT (toAbstractType (shape tArg)) at)))
        (return body')
bottomUpCheck env p@(Program (PFun x body) _) = do
    writeLog 3 $ text "Bottom up checking type for" <+> pretty p
    id <- freshId "A"
    let tArg = addTrue (ScalarT (TypeVarT Map.empty id) ())
    body' <- bottomUpCheck (addVariable x tArg env) body
    let (tBody, _, at) = typeOf body'
    let t = FunctionT x tArg tBody
    ifM (view isChecked <$> get)
        (return (Program (PFun x body') (t, t, AFunctionT (toAbstractType (shape tArg)) at)))
        (return body')
bottomUpCheck _ p = error ("unhandled case for checking " ++ show p ++ "::" ++ show (typeOf p))

solveTypeConstraint :: MonadIO m => Environment -> SType -> SType -> PNSolver m ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | id == id' = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | isBound env id && isBound env id' =
    modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | isBound env id = do
    st <- get
    if id' `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty tv
            solveTypeConstraint env tv typ
        else unify env id' tv
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty tv'
            solveTypeConstraint env typ tv'
        else if id' `Map.member` (st ^. typeAssignment)
            then do
                let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
                writeLog 3 $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty typ
                solveTypeConstraint env tv typ
            else do
                unify env id tv'
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
            solveTypeConstraint env typ t
        else do
            unify env id t
solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 $ text "Solving constraint" <+> pretty tArg <+> text "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    st <- get
    when (st ^. isChecked) (do
        writeLog 3 $ text "Solving constraint" <+> pretty tRet <+> text "==" <+> pretty tRet'
        solveTypeConstraint env tRet tRet')
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id /= id' = do
    modify $ set isChecked False
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id == id' = do
    solveTypeConstraint' env tArgs tArgs'
  where
    solveTypeConstraint' _ []  [] = return ()
    solveTypeConstraint' env (ty:tys) (ty':tys') = do
        writeLog 3 $ text "Solving constraint" <+> pretty ty <+> text "==" <+> pretty ty'
        solveTypeConstraint env ty ty'
        checked <- view isChecked <$> get
        -- if the checking between ty and ty' succeeds, proceed to others
        when (checked) (solveTypeConstraint' env tys tys')
solveTypeConstraint env t1 t2 = error $ "unknown types " ++ show t1 ++ " or " ++ show t2

-- | unify the type variable with some given type
-- add the type assignment to our state
unify :: (MonadIO m) => Environment -> Id -> SType -> PNSolver m ()
unify env v t = do
    modify $ over typeAssignment (Map.map (stypeSubstitute (Map.singleton v t)))
    tass <- view typeAssignment <$> get
    modify $ over typeAssignment (Map.insert v (stypeSubstitute tass t))
