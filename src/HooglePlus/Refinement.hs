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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Text.Printf
import Debug.Trace

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
     in foldr int_fun ([], baseCover) (nub scts)
updateCover' bound cover intscts t paren | isSubtypeOf bound paren t =
    let parents = HashMap.keys $ HashMap.filter (Set.member paren) cover
        rmParen s = let s' = Set.delete paren s in if Set.null s' then Nothing else Just s'
        replaceWith p acc = 
            if isSubtypeOf bound t p && not (equalAbstract bound t p)
                then HashMap.insertWith Set.union p (Set.singleton t) (HashMap.update rmParen p acc)
                else acc
        cover' = HashMap.insertWith Set.union t (Set.singleton paren) (foldr replaceWith cover parents)
     in (intscts, cover')
updateCover' bound cover intscts t paren =
    let intsctMb = abstractIntersect bound t paren
     in if isJust intsctMb then (fromJust intsctMb : intscts, cover)
                           else (intscts, cover)

-- -- | find which term subsumes the given one, there is no closed under meet
-- updateCover' :: [Id] -> AbstractSkeleton -> AbstractCover -> AbstractSkeleton -> AbstractCover
-- updateCover' bound t cover paren | isSubtypeOf bound t paren = 
--     if isSubtypeOf bound paren t 
--         then cover  -- t = paren
--         else cover' -- t <: paren 
--     where
--         children = Set.toList $ HashMap.lookupDefault Set.empty paren cover
--         lower c = isSubtypeOf bound t c || isSubtypeOf bound c t
--         inSubtree = any lower children
--         cover' = if inSubtree 
--             then foldl' (updateCover' bound t) cover children -- t <: child
--             else HashMap.insertWith Set.union paren (Set.singleton t) cover -- t is added as child of paren
-- updateCover' bound t cover paren | isSubtypeOf bound paren t = cover'
--     where
--         grandparents = HashMap.keys $ HashMap.filter (Set.member paren) cover
--         rmParen = HashMap.map (Set.delete paren) cover
--         addCurr p = HashMap.insertWith Set.union p $ Set.singleton t
--         addedCurr = foldr addCurr rmParen grandparents -- add current type to all the grandparents and delete the parent
--         cover' = HashMap.insertWith Set.union t (Set.singleton paren) addedCurr
-- updateCover' _ _ cover _ = cover

propagate :: MonadIO m => Environment -> TProgram -> AbstractSkeleton -> PNSolver m ()
-- | base case, when we reach the leaf of the AST
propagate env p@(Program (PSymbol sym) t) upstream = do
    writeLog 3 "propagate" $ text "propagate" <+> pretty upstream <+> text "into" <+> pretty p
    cover <- gets (view abstractionCover)
    let bound = env ^. boundTypeVars
    unless (existAbstract bound cover upstream)
           (do
                writeLog 3 "propagate" $ text "adding" <+> pretty upstream <+> text "into" <+> pretty cover
                let newCover = updateCover bound upstream cover
                writeLog 3 "propagate" $ text "add" <+> pretty upstream <+> text "gets" <+> pretty newCover
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
    cover <- gets (view abstractionCover)
    writeLog 3 "propagate" $ text "current abstraction cover:" <+> pretty (cover)
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
        lift $ writeLog 3 "propagate" $ text "inside mostGeneral"
        currAbs <- lift $ mapM (currentAbst bound cover) cArgs
        lift $ writeLog 3 "propagate" $ text "find current abstraction" <+> pretty currAbs 
        absArgs <- mapM (generalize env) cArgs
        lift $ writeLog 3 "propagate" $ text "try" <+> pretty absArgs <+> text "from" <+> pretty cArgs <+> text "with currently" <+> pretty currAbs
        guard ((all (uncurry $ isSubtypeOf bound)) (zip absArgs currAbs))
        lift $ writeLog 3 "propagate" $ text "get generalized types" <+> pretty absArgs <+> text "from" <+> pretty cArgs
        res <- lift $ applySemantic bound t absArgs
        lift $ writeLog 3 "propagate" $ text "apply" <+> pretty absArgs <+> text "to" <+> pretty t <+> text "gets" <+> pretty res
        guard (isSubtypeOf bound res upstream)
        return $ map compactAbstractType absArgs
-- | case for lambda functions
propagate env (Program (PFun x body) (FunctionT _ tArg tRet))
              (AFunctionT atArg atRet) =
    propagate (addVariable x tArg env) body atRet
propagate env (Program (PFun x body) t) (AFunctionT atArg atRet) = do
    id <- freshId "A"
    let tArg = TypeVarT id KnStar
    propagate (addVariable x tArg env) body atRet
propagate _ prog t = return ()

-- bottom up check a program on the concrete type system
-- at the same time, keep track of the abstract type for each node
bottomUpCheck :: MonadIO m => Environment -> TProgram -> PNSolver m TProgram
bottomUpCheck env p@(Program (PSymbol sym) typ) = do
    -- lookup the symbol type in current scope
    nameMap <- use nameMapping
    let sym' = removeLast '_' sym
    let name = stripSuffix $ fromMaybe sym' (Map.lookup sym' nameMap)
    t <- findSymbol env name
    writeLog 2 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p <+> text "get" <+> pretty t
    return (Program (PSymbol sym) t)
bottomUpCheck env p@(Program (PApp f args) typ) = do
    argResult <- checkArgs args
    case argResult of
        Left err -> return err
        Right checkedArgs -> do
            nameMap <- gets (view nameMapping)
            let name = stripSuffix $ fromMaybe f (Map.lookup f nameMap)
            t <- findSymbol env name
            -- check function signature against each argument provided
            let argVars = allArgTypes t
            let checkedArgTys = map typeOf checkedArgs
            mapM_ (uncurry $ solveTypeConstraint env) (zip checkedArgTys argVars)
            -- we eagerly substitute the assignments into the return type of t
            tass <- use typeAssignment
            let ret = typeSubstitute tass (partialReturn checkedArgs t)
            -- if any of these checks returned false, this function application
            -- would produce a bottom type
            writeLog 2 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p <+> text "get" <+> pretty ret
            ifM (use isChecked)
                (return $ Program (PApp f checkedArgs) ret)
                (return $ Program (PApp f checkedArgs) BotT)
  where
    partialReturn (_:args) (FunctionT _ _ tRes) = partialReturn args tRes
    partialReturn [] t = t
    
    checkArgs [] = return $ Right []
    checkArgs (arg:args) = do
        checkedArg <- bottomUpCheck env arg
        ifM (use isChecked)
            (do
                checkedArgs <- checkArgs args
                case checkedArgs of
                    Left err -> return $ Left err
                    Right args' -> return $ Right (checkedArg:args'))
            (return $ Left checkedArg)
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    body' <- bottomUpCheck (addVariable x tArg env) body
    let tBody = typeOf body'
    let t = FunctionT x tArg tBody
    ifM (use isChecked)
        (return $ Program (PFun x body') t)
        (return body')
bottomUpCheck env p@(Program (PFun x body) _) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    id <- freshId "A"
    id' <- freshId "A"
    let tArg = TypeVarT id KnStar
    let tRet = TypeVarT id' KnStar
    bottomUpCheck env (Program (PFun x body)(FunctionT x tArg tRet))
bottomUpCheck _ p = error $ "unhandled case for checking "
                          ++ show p ++ "::" ++ show (typeOf p)

solveTypeConstraint :: MonadIO m => Environment -> TypeSkeleton -> TypeSkeleton -> PNSolver m ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint env tv@(TypeVarT id k) tv'@(TypeVarT id' k')
  | id == id' && compareKind k k' = return ()
  | isBound env id && isBound env id' = isChecked .= False
  | isBound env id = do
    tass <- use typeAssignment
    if id' `Map.member` tass
        then do
            let typ = fromJust $ Map.lookup id' tass
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty tv
            solveTypeConstraint env tv typ
        else unify env id' k' tv
  | otherwise = do
    tass <- use typeAssignment
    if id `Map.member` tass
        then do
            let typ = fromJust $ Map.lookup id tass
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty tv'
            solveTypeConstraint env typ tv'
        else if id' `Map.member` tass
            then do
                let typ = fromJust $ Map.lookup id' tass
                writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty typ
                solveTypeConstraint env tv typ
            else unify env id k tv'
solveTypeConstraint env tv@(TypeVarT id _) t | isBound env id = isChecked .= False
solveTypeConstraint env tv@(TypeVarT id k) t = do
    tass <- use typeAssignment
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty t
    if id `Map.member` tass
        then do
            let typ = fromJust $ Map.lookup id tass
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
            solveTypeConstraint env typ t
        else unify env id k t
solveTypeConstraint env t tv@(TypeVarT {}) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tArg <+> text "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tRet <+> text "==" <+> pretty tRet'
    solveTypeConstraint env tRet tRet'
solveTypeConstraint env (DatatypeT id k) (DatatypeT id' k') 
    | id /= id' || not (compareKind k k') = do
        writeLog 2 "solveTypeConstraint" $ text "Solving constraint" <+> pretty (DatatypeT id k) <+> text "==" <+> pretty (DatatypeT id' k')
        isChecked .= False
    | id == id' && compareKind k k' = return ()
solveTypeConstraint env (TyAppT tFun tArg k) (TyAppT tFun' tArg' k') 
    | compareKind k k' = do
        solveTypeConstraint env tFun tFun'
        ifM (use isChecked)
            (solveTypeConstraint env tArg tArg')
            (return ())
solveTypeConstraint env (TyFunT tArg tRes) (TyFunT tArg' tRes') = do
    solveTypeConstraint env tArg tArg'
    ifM (use isChecked)
        (solveTypeConstraint env tRes tRes')
        (return ())
solveTypeConstraint env t1 t2 = do
    writeLog 3 "solveTypeConstraint" $ text "unmatched types" <+> pretty t1 <+> text "and" <+> pretty t2
    isChecked .= False

-- | unify the type variable with some given type
-- add the type assignment to our state
unify :: MonadIO m => Environment -> Id -> Kind -> TypeSkeleton -> PNSolver m ()
unify env v k t =
    if v `Set.member` typeVarsOf t
      then isChecked .= False
      else if compareKind k (kindOf t) then do
        tass' <- gets $ view typeAssignment
        writeLog 3 "unify" $ text (show tass')
        modify $ over typeAssignment (Map.map (typeSubstitute (Map.singleton v t)))
        tass <- gets $ view typeAssignment
        modify $ over typeAssignment (Map.insert v (typeSubstitute tass t))
        else isChecked .= False

-- | generalize a closed concrete type into an abstract one
generalize :: MonadIO m => Environment -> AbstractSkeleton -> LogicT (PNSolver m) AbstractSkeleton
generalize env t@(ATypeVarT id k)
  | id `notElem` (env ^. boundTypeVars) = return t
  | otherwise = do
    v <- lift $ freshId "T"
    return (ATypeVarT v k) `mplus` return t
generalize env t@(ADatatypeT id k) = do
    v <- lift $ freshId "T"
    return (ATypeVarT v k) `mplus` return t
generalize env t@(ATyAppT tFun tArg k) = do
    f <- generalize env tFun
    a <- generalize env tArg
    -- might abstracting some TyApp as a higher kinded type variable
    -- [TODO] does this slow us down?
    -- v <- lift $ freshId "T"
    -- TODO: let's try to not generalize the kind away
    return (ATyAppT f a k)
generalize env t@(ATyFunT tArg tRes) = do
    a <- generalize env tArg
    r <- generalize env tRes
    v <- lift $ freshId "T"
    return (ATypeVarT v KnStar) `mplus` return (ATyFunT a r)
generalize env (AFunctionT tArg tRes) = do
    tArg' <- generalize env tArg
    tRes' <- generalize env tRes
    return (AFunctionT tArg' tRes')
