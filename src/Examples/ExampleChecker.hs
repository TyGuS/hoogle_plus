{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE TupleSections #-}

module Examples.ExampleChecker where

import Types.Program
import Types.Type
import Types.Environment
import Types.Experiments
import Types.IOFormat
import Types.TypeChecker
import Types.Common
import Synquid.Type
import Synquid.Pretty
import Synquid.Logic
import HooglePlus.TypeChecker
import PetriNet.Util
import Synquid.Util (permuteBy)
import Database.Convert (addTrue)

import qualified EnumSet as ES
import Control.Exception
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Chan
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import Data.Either
import Data.Maybe
import Data.List
import GHC hiding(Id)
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
import Debug.Trace
import System.Timeout

timeoutLimit = 10^5 :: Int -- in microsecond
outputDepth = 4 :: Int

askGhc :: [String] -> Ghc a -> IO a
askGhc mdls f = do
    mbResult <- timeout timeoutLimit $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = dflags { 
            generalFlags = ES.delete Opt_OmitYields (generalFlags dflags)
            }
        setSessionDynFlags dflags'
        prepareModules ("System.Timeout":"Prelude":mdls) >>= setContext
        f
    case mbResult of
        Just r -> return r
        Nothing -> error "timeout"
    where
        prepareModules mdls = do
            let imports = map (printf "import %s") mdls
            decls <- mapM parseImportDecl imports
            return (map IIDecl decls)

parseExample :: [String] -> Example -> IO (Either RSchema ErrorMessage)
parseExample mdls ex = catch (do
    typ <- askGhc mdls $ exprType TM_Default mkFun
    let hsType = typeToLHsType typ
    return (Left $ toInt $ resolveType hsType))
    (\(e :: SomeException) -> return (Right $ show e))
    where
        toInt (ForallT x t) = ForallT x (toInt t)
        toInt (Monotype t) = Monotype (integerToInt t)

        mkFun = printf "\\f -> f %s == %s" (unwords $ map wrapParens $ inputs ex) (output ex)
        wrapParens = printf "(%s)"

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

getExampleTypes :: Environment -> [RSchema] -> IO [SType]
getExampleTypes env validSchemas = do
    let validTypes = map (shape . toMonotype) validSchemas
    t <- if not (null validTypes) then foldM antiUnification (head validTypes) (tail validTypes)
                                  else error "get example types error"
    let tvars = typeVarsOf t
    let generals = getGeneralizations t
    -- print generals
    -- print $ concatMap reduceVars generals
    let reducedTypes = concatMap (reduceVars tvars) generals
    msgChan <- newChan
    checkedReduce <- filterM (\s -> checkTypes env msgChan (forall s) (forall t)) reducedTypes
    return $ t : generals ++ checkedReduce
    where
        forall t = let vars = typeVarsOf t
                    in foldr ForallT (Monotype $ addTrue t) vars

execExample :: [String] -> Environment -> String -> Example -> IO (Either ErrorMessage String)
execExample mdls env prog ex = do
    let prependArg = unwords (Map.keys $ env ^. arguments)
    let progBody = if Map.null (env ^. arguments) -- if this is a request from front end
        then printf "let f = %s in" prog
        else printf "let f = \\%s -> %s in" prependArg prog
    let wrapParens = printf "(%s)"
    let parensedInputs = map wrapParens $ inputs ex
    let progCall = printf "f %s" (unwords parensedInputs)
    askGhc mdls $ do
        result <- execStmt (unwords [progBody, progCall]) execOptions
        case result of
            ExecComplete r _ -> case r of
                                Left e -> liftIO (print e) >> return (Left (show e)) 
                                Right ns -> getExecValue ns
            ExecBreak {} -> return (Left "error, break")
    where
        getExecValue (n:ns) = do
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
    let permutedCands = concatMap permuteMap (Map.toList candidates)
    let permutedMap = Map.fromList permutedCands
    msgChan <- newChan
    matchCands <- filterM (\s -> checkTypes env msgChan s goal) (Map.keys permutedMap)
    let usefulExs = concatMap (\s -> permutedMap Map.! s) matchCands
    return $ nubBy (\x y -> inputs x == inputs y) usefulExs
    where
        permuteExamples ords (Example ins out) = Example (permuteBy ords ins) out
        permuteMap (q, exs) = let argLen = length (argsWithName $ toMonotype q)
                                  orderPermutes = permutations [1..argLen]
                                  -- TODO: maybe we need to monomorphize the
                                  -- types here
                                  typesList = map (flip permuteArgs q) orderPermutes
                                  examplesList = map (\o -> map (permuteExamples o) exs) orderPermutes
                               in zip typesList examplesList

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
        atLeast2 = filter ((>= 2) . length) $ subsequences subtypesEach
        commonSubtypes =  map intersections atLeast2
        validCommons = filter (not . Set.null) commonSubtypes
        freeVars = Set.toList $ typeVarsOf t
        validNames = foldr delete seqChars freeVars
        combineCommons = nub $ map Set.unions $ tail $ subsequences validCommons
        permutedCommons = concatMap (tail . subsequences . Set.toList) combineCommons
        namedCommons = map (flip zip validNames) (nub permutedCommons)
     in map (foldr (uncurry antiSubstitute) t) namedCommons
    where
        intersections [] = Set.empty
        intersections (x:xs) = foldr Set.intersection x xs

reduceVars :: Set Id -> SType -> [SType]
reduceVars vs t =
    let freeVars = Set.toList (typeVarsOf t)
        varSubsts = varMaps freeVars
     in map (foldr (uncurry antiSubstitute) t) varSubsts
    where
        listv = Set.toList vs
        varMaps [] = [[]]
        varMaps (v:vs) = [[l1] | l1 <- map (vart_ v,) vs] ++ 
            [l1 : l2 | l1 <- map (vart_ v,) vs, l2 <- varMaps vs]

antiSubstitute :: SType -> Id -> SType -> SType
antiSubstitute pat name t | t == pat = vart_ name
antiSubstitute pat name (ScalarT (DatatypeT dt args _) _) = 
    ScalarT (DatatypeT dt (map (antiSubstitute pat name) args) []) ()
antiSubstitute pat name (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = antiSubstitute pat name tArg
        tRes' = antiSubstitute pat name tRes
antiSubstitute _ _ t = t

antiUnification :: SType -> SType -> IO SType
antiUnification t1 t2 = evalStateT (antiUnification' t1 t2) emptyAntiUnifState

antiUnification' :: SType -> SType -> AntiUnifier IO SType
antiUnification' AnyT t = return t
antiUnification' t AnyT = return t
antiUnification' t@(ScalarT (TypeVarT {}) _) (ScalarT (TypeVarT {}) _) = return t
antiUnification' (ScalarT (TypeVarT {}) _) t = return t
antiUnification' t (ScalarT (TypeVarT {}) _) = return t
antiUnification' t1@(ScalarT (DatatypeT dt1 args1 _) _) t2@(ScalarT (DatatypeT dt2 args2 _) _)
  | dt1 == dt2 = do
      args' <- mapM (uncurry antiUnification') (zip args1 args2)
      return $ ScalarT (DatatypeT dt1 args' []) ()
  | dt1 /= dt2 = do
      tass1 <- gets $ view typeAssignment1
      tass2 <- gets $ view typeAssignment2
      let overlap = (tass1 Map.! t1) `intersect` (tass2 Map.! t2)
      if t1 `Map.member` tass1 && t2 `Map.member` tass2 && not (null overlap)
         then if length overlap > 1 then error "antiUnficiation fails"
                                    else return $ vart_ (head overlap)
         else do 
             v <- freshId "a"
             modify $ over typeAssignment1 (Map.insertWith (++) t1 [v])
             modify $ over typeAssignment2 (Map.insertWith (++) t2 [v])
             return $ vart_ v
antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
    tArg <- antiUnification' tArg1 tArg2
    tRes <- antiUnification' tRes1 tRes2
    return $ FunctionT x1 tArg tRes

seqChars = map (:[]) ['a'..'z']

integerToInt :: TypeSkeleton r -> TypeSkeleton r
integerToInt (ScalarT (DatatypeT dt args _) r) 
  | dt == "Integer" = ScalarT (DatatypeT "Int" (map integerToInt args) []) r
  | otherwise = ScalarT (DatatypeT dt (map integerToInt args) []) r 
integerToInt (FunctionT x tArg tRes) =
    FunctionT x (integerToInt tArg) (integerToInt tRes)
integerToInt t = t
