{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Examples.ExampleChecker where

import Types.Program
import Types.Type
import Types.Environment
import Types.Experiments
import Types.IOFormat
import Types.TypeChecker
import Synquid.Type
import Synquid.Pretty
import Synquid.Logic
import HooglePlus.TypeChecker
import PetriNet.Util

import Control.Exception
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Chan
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import GHC
import GHCi hiding(Message)
import GHCi.RemoteTypes
import GHC.Paths
import Debugger
import Exception
import HsUtils
import HsTypes
import Outputable
import Text.Printf

parseExample :: [String] -> Example -> IO RSchema
parseExample mdls ex = do
    typ <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        prepareModules mdls >>= setContext
        exprType TM_Default mkFun
    let hsType = typeToLHsType typ
    return (resolveType hsType)
    where
        mkFun = printf "\\f -> f %s == %s" (unwords $ inputs ex) (output ex)

resolveType :: LHsType GhcPs -> RSchema
resolveType (L _ (HsForAllTy bs t)) = foldr ForallT (resolveType t) vs
    where
        vs = map vname bs
        vname (L _ (UserTyVar (L _ id))) = showSDocUnsafe (ppr id)
        vname (L _ (KindedTyVar (L _ id) _)) = showSDocUnsafe (ppr id)
resolveType (L _ (HsFunTy f _)) = Monotype (resolveType' f)
resolveType t = error (showSDocUnsafe $ ppr t)

resolveType' :: LHsType GhcPs -> RType
resolveType' (L _ (HsFunTy f r)) = FunctionT "" (resolveType' f) (resolveType' r)
resolveType' (L _ (HsQualTy _ t)) = resolveType' t
resolveType' (L _ (HsTyVar _ (L _ v))) = ScalarT (TypeVarT Map.empty (showSDocUnsafe $ ppr v)) ftrue
resolveType' t@(L _ HsAppTy{}) = ScalarT (DatatypeT dtName dtArgs []) ftrue
    where
        dtName = case datatypeOf t of
                   "[]" -> "List"
                   "(,)" -> "Pair"
                   n -> n
        dtArgs = datatypeArgs t

        datatypeOf (L _ (HsAppTy f _)) = datatypeOf f
        datatypeOf (L _ (HsTyVar _ (L _ v))) = showSDocUnsafe (ppr v)

        datatypeArgs (L _ (HsAppTy (L _ HsTyVar {}) a)) = [resolveType' a]
        datatypeArgs (L _ (HsAppTy f a)) = datatypeArgs f ++ datatypeArgs a
        datatypeArgs t = [resolveType' t]

resolveType' (L _ (HsListTy t)) = ScalarT (DatatypeT "List" [resolveType' t] []) ftrue
resolveType' (L _ (HsTupleTy _ ts)) = foldr mkPair basePair otherTyps
    where
        mkPair acc t = ScalarT (DatatypeT "Pair" [acc, t] []) ftrue
        resolveTyps = map resolveType' ts
        (baseTyps, otherTyps) = splitAt (length ts - 2) resolveTyps
        basePair = ScalarT (DatatypeT "Pair" baseTyps []) ftrue
resolveType' (L _ (HsParTy t)) = resolveType' t
resolveType' t = error $ showSDocUnsafe (ppr t)

checkExample :: Environment -> RType -> Example -> Chan Message -> IO ()
checkExample env typ ex checkerChan = do
    exTyp <- parseExample (Set.toList $ env ^. included_modules) ex
    let sTyp = shape typ
    let initChecker = emptyChecker { _messageChan = checkerChan }
    state <- execStateT (do
        exTyp' <- freshType exTyp
        let sExTyp = shape exTyp'
        solveTypeConstraint env sTyp sExTyp) initChecker
    let err = printf "%s does not have type %s" (show ex) (show typ)
    when (not $ state ^. isChecked) (error err)

checkExamples :: Environment -> RType -> [Example] -> Chan Message -> IO ()
checkExamples env typ exs checkerChan = mapM_ (\ex -> checkExample env typ ex checkerChan) exs

execExample :: [String] -> Environment -> RProgram -> Example -> IO (Either ErrorMessage String)
execExample mdls env prog ex =
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        prepareModules mdls >>= setContext
        let prependArg = unwords (Map.keys $ env ^. arguments)
        let progBody = printf "let f = \\%s -> %s in" prependArg (show prog)
        let progCall = printf "f %s" (unwords (inputs ex))
        result <- execStmt (unwords [progBody, progCall]) execOptions
        case result of
            ExecComplete r _ -> case r of
                                  Left e -> liftIO (print e) >> return (Left (show e)) 
                                  Right ns -> getExecValue ns
            ExecBreak {} -> return (Left "error, break")
    where
        getExecValue (n:ns) = do
            df <- getSessionDynFlags
            mty <- lookupName n
            case mty of
                Just (AnId aid) -> do
                    t <- gtry $ obtainTermFromId maxBound True aid
                    case t of
                        Right term -> showTerm term >>= return . Right . showSDocUnsafe
                        Left (exn :: SomeException) -> return (Left $ show exn)
                _ -> return (Left "Unknown error")
        getExecValue [] = return (Left "Empty result list")

checkExampleOutput :: [String] -> Environment -> RProgram -> [Example] -> IO (Maybe [Example])
checkExampleOutput mdls env prog exs = do
    currOutputs <- mapM (execExample mdls env prog) exs
    let cmpResults = map (uncurry compareResults) (zip currOutputs exs)
    let justResults = catMaybes cmpResults
    if length justResults == length exs then return $ Just justResults 
                                        else return Nothing
    where
        compareResults currOutput ex
          | output ex == "??" = Just (ex { output = either id id currOutput })
          | otherwise = case currOutput of
                          Left e -> Nothing
                          Right o | o == output ex -> Just ex
                                  | otherwise -> Nothing


prepareModules mdls = do
    let imports = map (printf "import %s") mdls
    decls <- mapM parseImportDecl imports
    return (map IIDecl decls)
