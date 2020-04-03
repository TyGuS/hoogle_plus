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
import Synquid.Program
import Synquid.Logic
import HooglePlus.TypeChecker
import HooglePlus.Utils
import PetriNet.Util
import Synquid.Util (permuteBy)
import Database.Convert (addTrue)
import Database.Util

import qualified EnumSet as ES
import Control.Exception
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Chan
import Data.Map (Map)
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
import GHC.LanguageExtensions.Type

timeoutLimit = 10^6 :: Int -- in microsecond
outputDepth = 4 :: Int

askGhc :: [String] -> Ghc a -> IO a
askGhc mdls f = do
    mbResult <- timeout timeoutLimit $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = dflags { 
            generalFlags = ES.delete Opt_OmitYields (generalFlags dflags),
            extensionFlags = ES.insert FlexibleContexts (extensionFlags dflags)
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

parseExample :: [String] -> String -> IO (Either RSchema ErrorMessage)
parseExample mdls mkFun = catch (do
    typ <- askGhc mdls $ exprType TM_Default mkFun
    let hsType = typeToLHsType typ
    return (Left $ toInt $ resolveType hsType))
    (\(e :: SomeException) -> return (Right $ show e))
    where
        toInt (ForallT x t) = ForallT x (toInt t)
        toInt (Monotype t) = Monotype (integerToInt t)

resolveType :: LHsType GhcPs -> RSchema
resolveType (L _ (HsForAllTy bs t)) = foldr ForallT (resolveType t) vs
    where
        vs = map vname bs
        vname (L _ (UserTyVar (L _ id))) = showSDocUnsafe (ppr id)
        vname (L _ (KindedTyVar (L _ id) _)) = showSDocUnsafe (ppr id)
resolveType (L _ (HsFunTy f _)) = Monotype (resolveType' f)
resolveType (L _ (HsQualTy ctx body)) = Monotype bodyWithTcArgs
    where
        unlocatedCtx = let L _ c = ctx in c
        tyConstraints = map resolveType' unlocatedCtx

        prefixTc (ScalarT (DatatypeT name args rs) r) = ScalarT (DatatypeT (tyclassPrefix ++ name) args rs) r
        prefixTc t = error $ "Unhandled " ++ show t

        tcArgs = map prefixTc tyConstraints
        bodyWithTcArgs = foldr (FunctionT "") (resolveType' body) tcArgs
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
    let mdls = Set.toList $ env ^. included_modules
    eitherTyp <- parseExample mdls mkFun
    case eitherTyp of
      Left exTyp -> do
        let err = printf "%s does not have type %s" (show ex) (show typ) :: String
        let tcErr = printf "%s does not satisfy type class constraint in %s" (show ex) (show typ) :: String
        (res, substedTyp) <- checkTypes env checkerChan exTyp typ
        let (tyclasses, strippedTyp) = unprefixTc substedTyp
        let tyclassesPrenex = intercalate ", " $ map show tyclasses
        let mkTyclass = printf "(%s) :: (%s) => (%s) -> ()" mkFun tyclassesPrenex (show strippedTyp)
        eitherTyclass <- if null tyclasses then return (Left (Monotype AnyT)) else parseExample mdls mkTyclass
        if res then if isLeft eitherTyclass then return $ Left exTyp
                                            else return $ Right tcErr
               else return $ Right err
      Right e -> return $ Right e
    where
        mkFun = printf "\\f -> case f %s of %s -> ()" (unwords $ map wrapParens $ inputs ex) (output ex)

        unprefixTc (FunctionT x tArg tRes) =
            case tArg of
              ScalarT (DatatypeT name args rs) r | tyclassPrefix `isPrefixOf` name ->
                  let (tcs, t) = unprefixTc tRes
                      currTc = ScalarT (DatatypeT (drop (length tyclassPrefix) name) args rs) r
                   in (currTc:tcs, t)
              _ -> ([], FunctionT x tArg tRes)
        unprefixTc t = ([], t)

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
    let reducedTypes = concatMap (reduceVars tvars) generals
    msgChan <- newChan
    checkRes <- mapM (\s -> checkTypes env msgChan (forall s) (forall t)) reducedTypes
    let checkedReduce = map snd $ filter (fst . fst) (zip checkRes reducedTypes)
    return $ t : generals ++ checkedReduce
    where
        forall t = let vars = typeVarsOf t
                    in foldr ForallT (Monotype $ addTrue t) vars

execExample :: [String] -> Environment -> String -> Example -> IO (Either ErrorMessage String)
execExample mdls env prog ex = do
    let args = Map.keys $ env ^. arguments
    let nontcArgs = filter (not . (tyclassArgBase `isPrefixOf`)) args
    let prependArg = unwords nontcArgs
    let progBody = if Map.null (env ^. arguments) -- if this is a request from front end
        then printf "let f = %s in" prog
        else printf "let f = (\\%s -> %s) in" prependArg prog
    let parensedInputs = map wrapParens $ inputs ex
    let progCall = printf "f %s" (unwords parensedInputs)
    runStmt mdls (unwords [progBody, progCall])

runStmt :: [String] -> String -> IO (Either ErrorMessage String)
runStmt mdls prog = askGhc mdls $ do
    -- allow type defaulting during execution
    dflags <- getSessionDynFlags
    let dflags' = dflags { 
        extensionFlags = ES.insert ExtendedDefaultRules (extensionFlags dflags)
        }
    setSessionDynFlags dflags'
    result <- execStmt prog execOptions
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
    matchCands <- filterM (\s -> generalThan goal s) (Map.keys permutedMap)
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

        generalThan s1 s2 = do
            msgChan <- newChan
            let initChecker = emptyChecker { _checkerChan = msgChan }
            state <- execStateT (do
                s1' <- freshType s1
                s2' <- freshType s2
                let vars = typeVarsOf s2'
                let env' = foldr addTypeVar env vars
                solveTypeConstraint env' (shape s1') (shape s2')) initChecker
            return $ state ^. isChecked

checkExampleOutput :: [String] -> Environment -> String -> [Example] -> IO (Maybe [Example])
checkExampleOutput mdls env prog exs = do
    let progWithoutTc = removeTypeclasses prog
    currOutputs <- mapM (execExample mdls env progWithoutTc) exs
    cmpResults <- mapM (uncurry compareResults) (zip currOutputs exs)
    let justResults = catMaybes cmpResults
    if length justResults == length exs then return $ Just justResults 
                                        else return Nothing
    where
        compareResults currOutput ex
          | output ex == "??" = return $ Just (ex { output = either id id currOutput })
          | otherwise = case currOutput of
                          Left e -> return Nothing
                          Right o -> do
                              expectedOutput <- runStmt mdls (output ex)
                              case expectedOutput of
                                  Left err -> return Nothing
                                  Right out | o == out -> return (Just ex)
                                            | otherwise -> return Nothing

checkTypes :: Environment -> Chan Message -> RSchema -> RSchema -> IO (Bool, SType)
checkTypes env checkerChan s1 s2 = do
    let initChecker = emptyChecker { _checkerChan = checkerChan }
    (t, state) <- runStateT (do
        r1 <- freshType s1
        r2 <- freshType s2
        let t1 = stripTyclass r1
        let t2 = stripTyclass r2
        solveTypeConstraint env (shape t1) (shape t2)
        tass <- gets $ view typeAssignment
        return $ stypeSubstitute tass $ shape r2) initChecker
    return (state ^. isChecked, t)
    where
        stripTyclass (FunctionT x (ScalarT (DatatypeT name args _) _) tRes)
          | tyclassPrefix `isPrefixOf` name = stripTyclass tRes
        stripTyclass t = t
        


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

wrapParens = printf "(%s)"
