module HooglePlus.TypeChecker(
    bottomUpCheck,
    solveTypeConstraint
    ) where

import Types.Type
import Types.Program
import Types.Environment
import Types.Checker
import Types.Common
import Synquid.Type
import Synquid.Program
import Synquid.Util
import Synquid.Pretty
import PetriNet.Util
import Database.Util
import Database.Convert

import Control.Monad.State
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace

-- bottom up check a program on the concrete type system
-- at the same time, keep track of the abstract type for each node
bottomUpCheck :: MonadIO m => Environment -> RProgram -> TypeChecker m RProgram
bottomUpCheck env p@(Program (PSymbol sym) typ) = do
    -- liftIO $ putStrLn $ "Checking for " ++ show p
    -- lookup the symbol type in current scope
    nameMap <- gets checkerNameMapping
    let sym' = removeLast '_' sym
    let name = replaceId hoPostfix "" $ fromMaybe sym' (Map.lookup sym' nameMap)
    t <- freshType $ findSymbol nameMap env name
    return (Program (PSymbol sym) t)
bottomUpCheck env p@(Program (PApp f args) typ) = do
    -- liftIO $ putStrLn $ "Checking for " ++ show p
    argResult <- checkArgs args
    -- liftIO $ putStrLn $ "Get argument " ++ show argResult
    case argResult of
        Left err -> return err
        Right checkedArgs -> do
            nameMap <- gets checkerNameMapping
            t <- freshType $ findSymbol nameMap env (removeLast '_' f)
            -- check function signature against each argument provided
            let argVars = allArgTypes t
            let checkedArgTys = map typeOf checkedArgs
            -- liftIO $ print $ zip checkedArgTys argVars
            mapM_ (uncurry $ solveTypeConstraint env) (zip checkedArgTys argVars)
            -- we eagerly substitute the assignments into the return type of t
            tass <- gets typeAssignment
            -- liftIO $ print tass
            let ret = typeSubstitute tass (lastType t)
            -- if any of these checks returned false, this function application
            -- would produce a bottom type
            ifM (gets isChecked)
                (return $ Program (PApp f checkedArgs) ret)
                (return $ Program (PApp f checkedArgs) BotT)
  where
    checkArgs [] = return $ Right []
    checkArgs (arg:args) = do
        checkedArg <- bottomUpCheck env arg
        ifM (gets isChecked)
            (do
               checkedArgs <- checkArgs args
               case checkedArgs of
                 Left err -> return $ Left err
                 Right args' -> return $ Right (checkedArg:args')
            )
            (return $ Left checkedArg)
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    -- writeLog 3 "bottomUpCheck" $ text "Bottom up checking type for" <+> pretty p
    body' <- bottomUpCheck (addVariable x (addTrue tArg) env) body
    let tBody = typeOf body'
    let t = FunctionT x tArg tBody
    ifM (gets isChecked)
        (return $ Program (PFun x body') t)
        (return body')
bottomUpCheck _ p = error $ "unhandled case for checking "
                          ++ show p ++ "::" ++ show (typeOf p)

solveTypeConstraint :: MonadIO m => Environment -> RType -> RType -> TypeChecker m ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _)
    | id == id' = return ()
    | isBound env id && isBound env id' = modify $ \st -> st { isChecked = False }
    | isBound env id = do
        tass <- gets typeAssignment
        if id' `Map.member` tass
            then do
                let typ = fromJust $ Map.lookup id' tass
                -- liftIO $ putStrLn $ "Solving constraint " ++ show typ ++ " == " ++ show tv
                solveTypeConstraint env tv typ
            else unify env id' tv
  | otherwise = do
    tass <- gets typeAssignment
    if id `Map.member` tass
        then do
            let typ = fromJust $ Map.lookup id tass
            -- liftIO $ putStrLn $ "Solving constraint " ++ show typ ++ " == " ++ show tv'
            solveTypeConstraint env typ tv'
        else if id' `Map.member` tass
            then do
                let typ = fromJust $ Map.lookup id' tass
                -- liftIO $ putStrLn $ "Solving constraint " ++ show tv ++ " == " ++ show typ
                solveTypeConstraint env tv typ
            else unify env id tv'
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = modify $ \st -> st { isChecked = False }
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    tass <- gets typeAssignment
    -- liftIO $ putStrLn $ "Solving constraint " ++ show tv ++ " == " ++ show t
    if id `Map.member` tass
        then do
            let typ = fromJust $ Map.lookup id tass
            -- liftIO $ putStrLn $ "Solving constraint " ++ show typ ++ " == " ++ show t
            solveTypeConstraint env typ t
        else unify env id t
solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    -- liftIO $ putStrLn $ "Solving constraint " ++ show tArg ++ " == " ++ show tArg'
    solveTypeConstraint env tArg tArg'
    -- liftIO $ putStrLn $ "Solving constraint " ++ show tRet ++ " == " ++ show tRet'
    solveTypeConstraint env tRet tRet'
solveTypeConstraint env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id /= id' =
    modify $ \st -> st { isChecked = False }
solveTypeConstraint env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' =
    solveTypeConstraint' env tArgs tArgs'
  where
    solveTypeConstraint' _ []  [] = return ()
    solveTypeConstraint' env (ty:tys) (ty':tys') = do
        -- liftIO $ putStrLn $ "Solving constraint " ++ show ty ++ " == " ++ show ty'
        solveTypeConstraint env ty ty'
        checked <- gets isChecked
        -- if the checking between ty and ty' succeeds, proceed to others
        when checked $ solveTypeConstraint' env tys tys'
solveTypeConstraint env t1 t2 = do
    -- liftIO $ putStrLn $ "unmatched types " ++ show t1 ++ " and " ++ show t2
    modify $ \st -> st { isChecked = False }

-- | unify the type variable with some given type
-- add the type assignment to our state
unify :: MonadIO m => Environment -> Id -> RType -> TypeChecker m ()
unify env v t
    | v `Set.member` typeVarsOf t = modify $ \st -> st { isChecked = False }
    | otherwise = do
        tass <- gets typeAssignment
        -- writeLog 3 "unify" $ text (show tass')
        let t' = typeSubstitute tass t
        if v `Set.member` typeVarsOf t'
            then modify $ \st -> st { isChecked = False }
            else do
                let tass' = Map.map (typeSubstitute (Map.singleton v t')) tass
                let tass'' = Map.insert v t' tass
                modify $ \st -> st { typeAssignment = tass'' }
