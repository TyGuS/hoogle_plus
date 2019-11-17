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

findSymbol :: MonadIO m => Environment -> Id -> PNSolver m TypeSkeleton
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
updateCover tvs t cover = updateCover' tvs t cover rootNode

-- | find which term subsumes the given one, there is no closed under meet
updateCover' :: [Id] -> AbstractSkeleton -> AbstractCover -> AbstractSkeleton -> AbstractCover
updateCover' bound t cover paren | isSubtypeOf bound t paren = 
    if isSubtypeOf bound paren t 
        then cover  -- t = paren
        else cover' -- t <: paren 
    where
        children = Set.toList $ HashMap.lookupDefault Set.empty paren cover
        lower c = isSubtypeOf bound t c || isSubtypeOf bound c t
        inSubtree = any lower children
        cover' = if inSubtree 
            then foldl' (updateCover' bound t) cover children -- t <: child
            else HashMap.insertWith Set.union paren (Set.singleton t) cover -- t is added as child of paren
updateCover' bound t cover paren | isSubtypeOf bound paren t = cover'
    where
        grandparents = HashMap.keys $ HashMap.filter (Set.member paren) cover
        rmParen = HashMap.map (Set.delete paren) cover
        addCurr p = HashMap.insertWith Set.union p $ Set.singleton t
        addedCurr = foldr addCurr rmParen grandparents -- add current type to all the grandparents and delete the parent
        cover' = HashMap.insertWith Set.union t (Set.singleton paren) addedCurr
updateCover' _ _ cover _ = cover

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
    let closedArgs = map typeOf args
    let argConcs = map (compactAbstractType . toAbstractType) closedArgs
    let absFun = toAbstractType t
    abstractArgs <- observeT $ mostGeneral argConcs absFun
    mapM_ (uncurry $ propagate env) (zip args abstractArgs)
  where
    mostGeneral cArgs t = do
        cover <- gets (view abstractionCover)
        let bound = env ^. boundTypeVars
        -- if the argument has the concrete type ct
        -- its current abstraction is at
        -- we are finding a refined abstraction at' < at
        -- such that ct < at' < at
        currAbs <- lift $ map head <$> mapM (currentAbst bound cover) cArgs
        absArgs <- mapM (generalize bound) cArgs
        -- lift $ writeLog 3 "propagate" $ text "try" <+> pretty absArgs <+> text "from" <+> pretty cArgs <+> text "with currently" <+> pretty currAbs
        guard ((all (uncurry $ isSubtypeOf bound)) (zip absArgs currAbs))
        lift $ writeLog 3 "propagate" $ text "get generalized types" <+> pretty absArgs <+> text "from" <+> pretty cArgs
        res <- lift $ applySemantic bound t absArgs
        lift $ writeLog 3 "propagate" $ text "apply" <+> pretty absArgs <+> text "to" <+> pretty t <+> text "gets" <+> pretty res
        guard (any (\r -> isSubtypeOf bound r upstream) res)
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
      let argVars = allArgTypes t
      let checkedArgTys = map typeOf checkedArgs
      mapM_ (uncurry $ solveTypeConstraint env) (zip checkedArgTys argVars)
      -- we eagerly substitute the assignments into the return type of t
      tass <- gets (view typeAssignment)
      let ret = typeSubstitute tass (lastType t)
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
    body' <- bottomUpCheck (addVariable x tArg) env) body
    let tBody = typeOf body'
    let t = FunctionT x tArg tBody
    ifM (gets $ view isChecked)
        (return $ Program (PFun x body') t)
        (return body')
bottomUpCheck env p@(Program (PFun x body) _) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    id <- freshId "A"
    id' <- freshId "A"
    let tArg = TypeVarT id
    let tRet = TypeVarT id'
    bottomUpCheck env (Program (PFun x body)(FunctionT x tArg tRet))
bottomUpCheck _ p = error $ "unhandled case for checking "
                          ++ show p ++ "::" ++ show (typeOf p)

solveTypeConstraint :: MonadIO m => Environment -> TypeSkeleton -> TypeSkeleton -> PNSolver m ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint env tv@(TypeVarT id) tv'@(TypeVarT id')
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
    tass <- gets (view typeAssignment)
    if id `Map.member` tass
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty tv'
            solveTypeConstraint env typ tv'
        else if id' `Map.member` tass
            then do
                let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
                writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty typ
                solveTypeConstraint env tv typ
            else unify env id tv'
solveTypeConstraint env tv@(TypeVarT id) t | isBound env id = modify $ set isChecked False
solveTypeConstraint env tv@(TypeVarT id) t = do
    st <- get
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty t
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
            solveTypeConstraint env typ t
        else unify env id t
solveTypeConstraint env t tv@(TypeVarT id) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tArg <+> text "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tRet <+> text "==" <+> pretty tRet'
    solveTypeConstraint env tRet tRet'
solveTypeConstraint env (DatatypeT id) (DatatypeT id') 
    | id /= id' = modify $ set isChecked False
    | id == id' = return ()
solveTypeConstraint env (TyAppT tFun tArg) (TyAppT tFun' tArg') = do
    solveTypeConstraint env tFun tFun'
    ifM (gets $ view isChecked)
        (solveTypeConstraint env tArg tArg')
        (return ())
solveTypeConstraint env (TyFunT tArg tRes) (TyFunT tArg' tRes') = do
    solveTypeConstraint env tArg tArg'
    ifM (gets $ view isChecked)
        (solveTypeConstraint env tRes tRes')
        (return ())
solveTypeConstraint env t1 t2 = do
    writeLog 3 "solveTypeConstraint" $ text "unmatched types" <+> pretty t1 <+> text "and" <+> pretty t2
    modify $ set isChecked False

-- | unify the type variable with some given type
-- add the type assignment to our state
unify :: MonadIO m => Environment -> Id -> TypeSkeleton -> PNSolver m ()
unify env v t =
    if v `Set.member` typeVarsOf t
      then modify $ set isChecked False
      else do
        tass' <- gets $ view typeAssignment
        writeLog 3 "unify" $ text (show tass')
        modify $ over typeAssignment (Map.map (typeSubstitute (Map.singleton v t)))
        tass <- gets $ view typeAssignment
        modify $ over typeAssignment (Map.insert v (typeSubstitute tass t))

-- | generalize a closed concrete type into an abstract one
generalize :: MonadIO m => [Id] -> AbstractSkeleton -> LogicT (PNSolver m) AbstractSkeleton
generalize bound t@(ATypeVarT id)
  | id `notElem` bound = return t
  | otherwise = do
    v <- lift $ freshId "T"
    return (ATypeVarT v) `mplus` return t
generalize bound t@(ADatatypeT id k) = do
    v <- lift $ freshId "T"
    return (ATypeVarT v) `mplus` return t
generalize bound t@(ATyAppT tFun tArg) = do
    f <- generalize bound tFun
    a <- generalize bound tArg
    -- might abstracting some TyApp as a higher kinded type variable
    -- [TODO] does this slow us down?
    v <- lift $ freshId "T"
    return (ATypeVarT v) `mplus` return (ATyAppT f a)
generalize bound t@(ATyFunT tArg tRes) = do
    a <- generalize bound tArg
    r <- generalize bound tRes
    v <- lift $ freshId "T"
    return (ATypeVarT v) `mplus` return (ATyFunT a r)
generalize bound (AFunctionT tArg tRes) = do
    tArg' <- generalize bound tArg
    tRes' <- generalize bound tRes
    return (AFunctionT tArg' tRes')
