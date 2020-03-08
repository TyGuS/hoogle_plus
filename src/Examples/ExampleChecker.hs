{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TemplateHaskell #-} 

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

import Bag
import Control.Exception
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Chan
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.Either
import Data.Maybe
import Data.List
import GHC
import GHCi hiding(Message)
import GHCi.RemoteTypes
import GHC.Paths
import TcRnDriver
import Debugger
import Exception
import HsUtils
import HsTypes
import Outputable
import Text.Printf

askGhc :: [String] -> Ghc a -> IO a
askGhc mdls f = runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    prepareModules mdls >>= setContext
    result <- f
    return result
    where
        prepareModules mdls = do
            let imports = map (printf "import %s") mdls
            decls <- mapM parseImportDecl imports
            return (map IIDecl decls)

parseExample :: [String] -> Example -> IO (Either RSchema ErrorMessage)
parseExample mdls ex = catch (do
    typ <- askGhc mdls $ exprType TM_Default mkFun
    let hsType = typeToLHsType typ
    return (Left $ resolveType hsType))
    (\(e :: SomeException) -> return (Right $ show e))
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
resolveType' (L _ (HsTyVar _ (L _ v))) = 
    if isLower (head name)
       then ScalarT (TypeVarT Map.empty name) ftrue
       else ScalarT (DatatypeT name [] []) ftrue
    where
        name = showSDocUnsafe $ ppr v
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

checkExample :: Environment -> RSchema -> Example -> Chan Message -> IO (Either RSchema ErrorMessage)
checkExample env typ ex checkerChan = do
    eitherTyp <- parseExample (Set.toList $ env ^. included_modules) ex
    case eitherTyp of
      Left exTyp -> do
        let err = printf "%s does not have type %s" (show ex) (show typ) :: String
        res <- checkTypes env checkerChan exTyp typ 
        if res then return $ Left exTyp
               else return $ Right err
      Right e -> return $ Right e

checkExamples :: Environment -> RSchema -> [Example] -> Chan Message -> IO (Either [RSchema] [ErrorMessage])
checkExamples env typ exs checkerChan = do
    outExs <- mapM (\ex -> checkExample env typ ex checkerChan) exs
    let (validResults, errs) = partitionEithers outExs
    if null errs then return $ Left validResults
                 else return $ Right errs

getExampleTypes :: Environment -> [Example] -> IO (Either [SType] [ErrorMessage])
getExampleTypes env exs = do
    eitherTyps <- mapM (parseExample (Set.toList $ env ^. included_modules)) exs
    let (validTypes, errs) = partitionEithers eitherTyps
    case errs of
      [] -> do
          t <- if length validTypes > 0 then foldM antiUnification (head validTypes) (tail validTypes)
                                        else error "get example types error"
          generals <- getGeneralizations t
          return $ Left $ generals ++ concatMap reduceVars generals
      errs -> return $ Right errs

execExample :: [String] -> Environment -> String -> Example -> IO (Either ErrorMessage String)
execExample mdls env prog ex =
    let prependArg = unwords (Map.keys $ env ^. arguments)
    let progBody = if Map.null (env ^. arguments) -- if this is a request from front end
        then printf "let f = %s in" prog
        else printf "let f = \\%s -> %s in" prependArg prog
    let wrapParens x = printf "(%s)" x
    let progCall = printf "f %s" (unwords (map wrapParens $ inputs ex))
    askGhc $ do
        execStmt (unwords [progBody, progCall]) execOptions
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

-- to check two type are exactly the same
-- what about swapping arg orders?
augmentTestSet :: Environment -> RSchema -> IO [Example]
augmentTestSet env goal = do
    let candidates = env ^. queryCandidates
    msgChan <- newChan
    matchCands <- filterM (\s -> checkTypes env msgChan s goal) (Map.keys candidates)
    let usefulExs = concatMap (\s -> candidates Map.! s) matchCands
    return $ nubBy (\x y -> inputs x == inputs y) usefulExs

checkExampleOutput :: [String] -> Environment -> String -> [Example] -> IO (Maybe [Example])
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


checkTypes :: Environment -> Chan Message -> RSchema -> RSchema -> IO Bool
checkTypes env checkerChan s1 s2 = do
    let initChecker = emptyChecker { _checkerChan = checkerChan }
    state <- execStateT (do
        s1' <- freshType s1
        s2' <- freshType s2
        solveTypeConstraint env (shape s1') (shape s2')) initChecker
    return $ state ^. isChecked

getGeneralizations :: SType -> [SType]
getGeneralizations t =
    let subtypesEach = map subtypesOf (breakdown t)
        commonSubtypes =  map intersections $ subsequences subtypesEach
        validCommons = filter (not . Set.null) commonSubtypes
        namedCommons = map (flip zip ['a'..'z'] . Set.toList) validCommons
     in map (foldr (uncurry antiSubstitute) t) namedCommons
    where
        intersections [] = Set.empty
        intersections (x:xs) = foldr Set.intersection x xs

reduceVars :: SType -> [SType]
reduceVars t =
    let freeVars = vars t
        varSubsts = varMaps freeVars
     in map (foldr (uncurry antiSubstitute) t) varSubsts
    where
        varMaps [] = [[]]
        varMaps (v:vs) = [[l1] | l1 <- map (vart _v,) vs] ++ 
            [l1 : l2 | l1 <- map (vart_ v,) vs, l2 <- varMap vs]

antiSubstitute :: SType -> Id -> SType -> SType
antiSubstitute pat name t | t == pat = vart_ name
antiSubstitute pat name (ScalarT (DatatypeT dt args _) _) = 
    ScalarT (DatatypeT dt (map (antiSubstitute pat name) args) []) ()
antiSubstitute pat name (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = antiSubstitute pat name tArg
        tRes' = antiSubstitute pat name tRes'
antiSubstitute _ _ t = t

antiUnification :: SType -> SType -> IO SType
antiUnification t1 t2 = evalStateT (antiUnification' t1 t2) emptyAntiUnifState

antiUnification' :: SType -> SType -> AntiUnifier SType
antiUnification' AnyT t = return t
antiUnification' t AnyT = return t
antiUnification' t@(ScalarT (TypeVarT {}) _) (ScalarT (TypeVarT {}) _) = return t
antiUnification' (ScalarT (TypeVarT {}) _) t = return t
antiUnification' t (ScalarT (TypeVarT {}) _) = return t
antiUnification' t1@(ScalarT (DatatypeT dt1 args1 _) _) t2@(ScalarT (DatatypeT dt2 args2 _) _)
  | dt1 == dt2 = do
      args' <- mapM (uncurry antiUnification') (zip args1 args2)
      let dt = if dt1 == "Integer" then "Int" else dt1
      return $ ScalarT (DatatypeT dt args' []) ()
  | dt1 /= dt2 = do
      tass1 <- gets $ view typeAssignment1
      tass2 <- gets $ view typeAssignment2
      v <- if dt1 `Map.member` tass1 && dt2 `Map.member` tass2
              then return $ tass1 Map.! dt1
              else do 
                  v <- freshId "a"
                  modify $ over typeAssignment1 (Map.insert t1 v)
                  modify $ over typeAssignment2 (Map.insert t2 v)
                  return v
      return $ vart_ v
antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
    tArg <- antiUnification' tArg1 tArg2
    tRes <- antiUnification' tRes1 tRes2
    return $ FunctionT x1 tArg tRes
