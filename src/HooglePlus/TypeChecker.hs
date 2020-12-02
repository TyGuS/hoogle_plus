module HooglePlus.TypeChecker
    ( bottomUpCheck
    , solveTypeConstraint
    ) where

import Database.Convert
import Database.Utils
import Types.TypeChecker
import Types.Environment
import Types.Type
import Types.Program
import Types.CheckMonad
import Types.Common
import Synquid.Type
import Synquid.Pretty
import Synquid.Program
import PetriNet.Utils

import Control.Monad.State
import Control.Lens
import Control.Monad.Extra
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Text.Printf
import Debug.Trace

-- bottom up check a program on the concrete type system
-- at the same time, keep track of the abstract type for each node
bottomUpCheck :: MonadIO m => Environment -> TProgram -> CheckerIO m TProgram
bottomUpCheck env p@(Program (PSymbol sym) typ) = do
    -- lookup the symbol type in current scope
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    nameMap <- gets (view nameMapping)
    let sym' = removeLast '_' sym
    -- find the real name of a function
    let name = stripSuffix $ fromMaybe sym' (Map.lookup sym' nameMap)
    t <- findSymbol env name
    return (Program (PSymbol sym) t)
bottomUpCheck env (Program (PApp f args) typ) = do
  argResult <- checkArgs args
  case argResult of
    Left err -> return err
    Right checkedArgs -> do
      -- find the real name of a function
      nameMap <- gets (view nameMapping)
      let name = stripSuffix $ fromMaybe f (Map.lookup f nameMap)
      t <- findSymbol env name 
      writeLog 3 "bottomUpCheck" $ text "Bottom up checking function" <+> pretty f
                                 <+> text "get type" <+> pretty t
      -- check function signature against each argument provided
      let argVars = allArgTypes t
      let checkedArgTys = map typeOf checkedArgs
      writeLog 3 "bottomUpCheck" $ text "Bottom up checking get arg types" <+> pretty checkedArgTys
      state $ runState $ mapM_ (uncurry (solveTypeConstraint env)) (zip checkedArgTys argVars)
      -- we eagerly substitute the assignments into the return type of t
      tass <- gets (view typeAssignment)
      writeLog 3 "bottomUpCheck" $ pretty tass
      writeLog 3 "bottomUpCheck" $ pretty (partialReturn checkedArgs t)
      let ret = typeSubstitute tass (partialReturn checkedArgs t)
      -- if any of these checks returned false, this function application
      -- would produce a bottom type
      ifM (gets $ view isChecked)
          (return $ Program (PApp f checkedArgs) ret)
          (return $ Program (PApp f checkedArgs) BottomT)
  where
    partialReturn (_:args) (FunctionT _ _ tRes) = partialReturn args tRes
    partialReturn [] t = t

    checkArgs [] = return $ Right []
    checkArgs (arg:args) = do
        checkedArg <- bottomUpCheck env arg
        ifM (gets $ view isChecked)
            (do checkedArgs <- checkArgs args
                case checkedArgs of
                    Left err -> return $ Left err
                    Right args' -> return $ Right (checkedArg:args'))
            (return $ Left checkedArg)
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    body' <- bottomUpCheck (addVariable x tArg env) body
    let tBody = typeOf body'
    let t = FunctionT x tArg tBody
    ifM (gets $ view isChecked)
        (return $ Program (PFun x body') t)
        (return body')
bottomUpCheck env p@(Program (PFun x body) _) = do
    writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    let bound = env ^. boundTypeVars
    id <- freshId bound "A"
    id' <- freshId bound "A"
    let tArg = TypeVarT id
    let tRet = TypeVarT id'
    bottomUpCheck env (Program (PFun x body)(FunctionT x tArg tRet))
bottomUpCheck _ p = error $ "unhandled case for checking "
                          ++ show p ++ "::" ++ show (typeOf p)

checkAssignment :: Environment -> Id -> TypeSkeleton -> Checker ()
checkAssignment env id tv = do
    tass <- gets (view typeAssignment)
    let typ = fromJust $ Map.lookup id tass
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty tv
    solveTypeConstraint env typ tv

solveTypeConstraint :: Environment -> TypeSkeleton -> TypeSkeleton -> Checker ()
solveTypeConstraint _ t1 t2 | t1 == t2 = return ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint env tv@(TypeVarT id) tv'@(TypeVarT id')
  | id == id' = return ()
  | isBound env id && isBound env id' = modify $ set isChecked False
  | isBound env id = do
    tass <- gets (view typeAssignment)
    if id' `Map.member` tass then checkAssignment env id' tv
                             else unify env id' tv
  | otherwise = do
    tass <- gets (view typeAssignment)
    if id `Map.member` tass 
       then checkAssignment env id tv'
       else if id' `Map.member` tass
            then checkAssignment env id' tv
            else unify env id tv'
solveTypeConstraint env tv@(TypeVarT id) t | isBound env id = modify $ set isChecked False
solveTypeConstraint env tv@(TypeVarT id) (DatatypeT dt)
  | tyclassPrefix `isPrefixOf` dt = modify $ set isChecked False -- type class cannot unify with a type variable
solveTypeConstraint env tv@(TypeVarT id) t@(TyAppT {})
  | tyclassPrefix `isPrefixOf` dt = modify $ set isChecked False -- type class cannot unify with a type variable
  where
      (dt, _) = collectArgs t
solveTypeConstraint env tv@(TypeVarT id) t = do
    tass <- gets (view typeAssignment)
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tv <+> text "==" <+> pretty t
    if id `Map.member` tass
        then do
            let typ = fromJust $ Map.lookup id tass
            writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
            solveTypeConstraint env typ t
        else unify env id t
solveTypeConstraint env t tv@(TypeVarT id) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tArg <+> text "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tRet <+> text "==" <+> pretty tRet'
    solveTypeConstraint env tRet tRet'
solveTypeConstraint env (DatatypeT id) (DatatypeT id') | id == id' = return ()
solveTypeConstraint env (TyAppT tFun tArg) (TyAppT tFun' tArg') = do
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tFun <+> text "==" <+> pretty tFun'
    solveTypeConstraint env tFun tFun'
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tArg <+> text "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
solveTypeConstraint env (TyFunT tArg tRes) (TyFunT tArg' tRes') = do
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tArg <+> text "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    writeLog 3 "solveTypeConstraint" $ text "Solving constraint" <+> pretty tRes <+> text "==" <+> pretty tRes'
    solveTypeConstraint env tRes tRes'
solveTypeConstraint env t1 t2 = do
    writeLog 3 "solveTypeConstraint" $ text "unmatched types" <+> pretty t1 <+> text "and" <+> pretty t2
    modify $ set isChecked False

-- | unify the type variable with some given type
-- add the type assignment to our state
-- TODO: why do we need eager substitution
unify :: Environment -> Id -> TypeSkeleton -> Checker ()
unify env v t =
    if v `Set.member` typeVarsOf t
      then modify $ set isChecked False
      else do
          modify $ over typeAssignment (Map.insert v t)
          tass <- gets $ view typeAssignment
          unless (isValidSubst tass) (modify $ set isChecked False)
        {- no eager substitution
        do
        tass' <- gets (view typeAssignment)
        writeLog 3 "unify" $ text (show tass')
        if isValidSubst tass'
           then do
                modify $ over typeAssignment (Map.map (typeSubstitute (Map.singleton v t)))
                tass <- gets (view typeAssignment)
                if isValidSubst tass then modify $ over typeAssignment (Map.insert v (typeSubstitute tass t))
                                     else modify $ set isChecked False
           else modify $ set isChecked False
        -}

--------------------------------------------------------------------------------
-- | Miscellaneous
--------------------------------------------------------------------------------

isValidSubst :: Map Id TypeSkeleton -> Bool
isValidSubst m = case Map.lookupMin varGraph of
                   Nothing -> True
                   Just v -> let nodes = Map.keysSet varGraph `Set.union` Set.unions (Map.elems varGraph)
                              in not (any (hasLoop Set.empty) (Set.toList nodes))
    where
        insertToMap v = Map.insertWith Set.union v . Set.singleton
        varGraph = foldr (\(v, t) acc -> Set.foldr (insertToMap v) acc (typeVarsOf t)) Map.empty (Map.toList m)

        hasLoop seen v | v `Set.member` seen = True
        hasLoop seen v = let nexts = Map.findWithDefault Set.empty v varGraph
                          in any (hasLoop (Set.insert v seen)) (Set.toList nexts)
