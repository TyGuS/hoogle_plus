{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Language.Java.Inline.Plugin:dump-java #-}

module PetriNet.PNSolver where

import Language.Java (reify, reflect)
import Language.Java.Inline
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Control.Lens
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Either
import Data.Int
import Data.Scientific
import qualified Data.Char as Char
import qualified Data.Vector as Vector
import Data.List.Extra
import Database.Generate
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Data.Serialize (Serialize)
import Data.Aeson (ToJSON, genericToEncoding, defaultOptions)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import Debug.Trace
import Language.Haskell.Exts.Parser (parseExp, ParseResult(..))
import qualified Data.HashMap.Strict as HashMap
import Data.Time.Clock

import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Program
import Synquid.Type
import Synquid.Logic
import Synquid.Util hiding(fromRight)
import Synquid.Error
import Synquid.Pretty
import PetriNet.PolyDispatcher
import PetriNet.AbstractType
import Database.Convert

-- data InstantiateState = InstantiateState {
--     _functionIdx :: Map Id Int
-- } deriving(Eq, Ord, Show)

-- makeLenses ''InstantiateState
data TypingState = TypingState {
    _nameCounter :: Map Id Int,  -- name map for generating fresh names (type variables, parameters)
    _typeAssignment :: Map Id SType,  -- current type assignment for each type variable
    _typingError :: (SType, SType), -- typing error message, represented by the expected type and actual type
    _abstractionLevel :: Map Id (Set Id), -- current abstraction level
    _isChecked :: Bool, -- is the current state check passed
    _currentSolutions :: [String], -- type checked solutions
    _currentLoc :: Int32, -- current solution depth
    _currentSigs :: Map Id AbstractSkeleton, -- current instantiated signatures
    _logLevel :: Int -- temporary for log level
}

emptyTypingState = TypingState {
    _nameCounter = Map.empty,
    _typeAssignment = Map.empty,
    _typingError = (AnyT, AnyT),
    _abstractionLevel = Map.empty,
    _isChecked = True,
    _currentSolutions = [],
    _currentLoc = 1,
    _currentSigs = Map.empty,
    _logLevel = 0
}

makeLenses ''TypingState

type PNSolver m = StateT TypingState m

-- for encoding abstractions into JSON string
type Param = String -- parameter type

data FunctionCode = FunctionCode {
  funName   :: String,  -- function name
  funParams :: [Param], -- function parameter types and their count
  funReturn :: String   -- function return type
} deriving(Eq, Ord, Show, Generic)

instance ToJSON FunctionCode where
    toEncoding = genericToEncoding defaultOptions
instance Serialize FunctionCode

abstractParamList :: AbstractSkeleton -> [AbstractSkeleton]
abstractParamList t@(ADatatypeT _ _) = [t]
abstractParamList t@(AExclusion _)   = [t]
abstractParamList (AFunctionT tArg tFun) = 
    case tFun of
        ADatatypeT _ _  -> [tArg]
        AExclusion _    -> [tArg]
        ATypeVarT  _    -> [tArg]
        _               -> (tArg) : (abstractParamList tFun)
abstractParamList t = error $ "Unexpected type " ++ show t

lastAbstractType :: AbstractSkeleton -> AbstractSkeleton
lastAbstractType (AFunctionT tArg tFun) = lastAbstractType tFun
lastAbstractType t                      = t

encodeFunction :: Id -> AbstractSkeleton -> Maybe FunctionCode
encodeFunction id t@(ADatatypeT _ _)       = Just $ FunctionCode id [] (show t)
encodeFunction id t@(AExclusion tys)       = Just $ FunctionCode id [] (show tys)
encodeFunction id t@(ATypeVarT  _)         = Just $ FunctionCode id [] (show t) -- error "Cannot encode a variable"
encodeFunction id t@(AFunctionT tArg tRet) = Just $ FunctionCode id (map show $ abstractParamList t) (show $ lastAbstractType t)


freshId :: (MonadIO m) => Id -> PNSolver m Id
freshId prefix = do
    indices <- flip (^.) nameCounter <$> get
    let idx = Map.findWithDefault 0 prefix indices
    modify (over nameCounter $ Map.insert prefix (idx+1))
    return $ prefix ++ "_" ++ show idx

-- | Replace all bound type variables with fresh free variables
freshType :: (MonadIO m) => RSchema -> PNSolver m RType
freshType sch = do
  t <- freshType' Map.empty sch
  return t
  where
    freshType' subst (ForallT a sch) = do
      a' <- freshId "A"
      freshType' (Map.insert a (vart a' ftrue) subst) sch    
    freshType' subst (Monotype t) = return $ typeSubstitute subst $ t

-- TODO: start with only the datatypes in the query
instantiate :: (MonadIO m) => Environment -> Map Id AbstractSkeleton -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = instantiate' sigs $ Map.filter (not . hasAbstractVar) sigs
  where 
    removeSuffix id ty = (removeLast '_' id, ty)
    constructType abstraction key | key `Map.notMember` abstraction = 
        let currDt     = if null (splitBy ',' key) then "" else last $ splitBy ',' key
        in if currDt == "" then [AExclusion Set.empty]
                           else if Char.isUpper $ head currDt then [ADatatypeT currDt [AExclusion Set.empty]]
                                                              else [ATypeVarT currDt, AExclusion (Set.singleton currDt)] -- this is bounded type variables
    constructType abstraction key | key `Map.member`    abstraction = 
        let nextKeys   = fromJust $ Map.lookup key abstraction
            currDt     = if null (splitBy ',' key) then "" else last $ splitBy ',' key
        in if currDt == ""
            then [AExclusion nextKeys] ++ concatMap (constructType abstraction . (++) (key ++ ",")) (Set.toList nextKeys)
            else if Char.isUpper $ head currDt
                then map (ADatatypeT currDt . (:[])) $ [AExclusion nextKeys] ++ concatMap (constructType abstraction . (++) (key ++ ",")) (Set.toList nextKeys)
                else [ATypeVarT currDt, AExclusion (Set.singleton currDt)] -- this is bounded type variables
    instantiate' sigs sigsAcc = do
        st <- get
        writeLog 0 $ text "current abstraction types:" <+> pretty (constructType (st ^. abstractionLevel) "")
        let typs = Set.fromList $ concatMap (allAbstractBase (env ^. boundTypeVars)) (Map.union sigsAcc sigs)
        sigs' <- foldM (\acc -> (<$>) (Map.union acc) . uncurry (instantiateWith env $ Set.toList typs)) Map.empty (Map.toList sigs)
        -- iteratively compute for new added types
        let typs' = Set.fromList $ concatMap (allAbstractBase (env ^. boundTypeVars)) sigs'
        writeLog 3 $ pretty (Set.toList typs')
        if typs' /= typs
            then instantiate' sigs $ Map.fromList $ nubOn (uncurry removeSuffix) $ Map.toList $ Map.union sigsAcc sigs'
            else return $ Map.fromList $ nubOn (uncurry removeSuffix) $ Map.toList $ Map.union sigsAcc sigs'

instantiateWith :: (MonadIO m) => Environment -> [AbstractSkeleton] -> Id -> AbstractSkeleton -> PNSolver m (Map Id AbstractSkeleton)
instantiateWith env typs id sk = do
    let vars = Set.toList $ allAbstractVar sk
    let multiSubsts    = map (zip vars) $ multiPermutation (length vars) typs
    let substedSymbols = map (foldr (\(id, t) acc -> if id `Set.notMember` allAbstractVar t then abstractSubstitute (env ^. boundTypeVars) id t acc else acc) sk) multiSubsts
    st <- get
    foldrM (\t accMap -> do
        newId <- newSymbolName id
        return $ Map.insert newId (cutoff (st ^. abstractionLevel) "" t) accMap) Map.empty substedSymbols
    where
        multiPermutation len elmts | len == 0 = []
        multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
        multiPermutation len elmts            = nub $ [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]
        newSymbolName prefix = do
            indices <- flip (^.) nameCounter <$> get
            let idx = Map.findWithDefault 0 prefix indices
            modify (over nameCounter $ Map.insert prefix (idx+1))
            return $ prefix ++ "_" ++ show idx

cutoff :: Map Id (Set Id) -> Id -> AbstractSkeleton -> AbstractSkeleton
cutoff level key (ADatatypeT id tArgs) | key `Map.member` level = 
    let currIds = fromJust $ Map.lookup key level
    in if id `Set.member` currIds then ADatatypeT id (nub $ map (cutoff level (key ++ id)) tArgs)
                                  else AExclusion currIds
cutoff level key (ADatatypeT id tArgs) | otherwise = AExclusion Set.empty
cutoff level key (AFunctionT tArg tRet) = AFunctionT (cutoff level key tArg) (cutoff level key tRet)
cutoff level key t@(AExclusion _)  = if key `Map.member` level then AExclusion (fromJust $ Map.lookup key level)
                                                               else AExclusion Set.empty
cutoff _ _ t = t

distinguish :: Environment -> Map Id (Set Id) -> Id -> SType -> SType -> Map Id (Set Id)
distinguish env level key (ScalarT (DatatypeT id _ _) _) (ScalarT (DatatypeT id' _ _) _) | id /= id' = 
    Map.insertWith Set.union key (Set.fromList [id, id']) level
distinguish env level key (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' = 
    distinguish' (key ++ id) tArgs tArgs'
  where
    -- split the current node into two when we cannot distinguish the error type from its abstract representation
    distinguish' key [] [] = level -- error "distinguish error: these two types are exactly the same"
    distinguish' key [] ((ScalarT (TypeVarT _ id) _):_) | isBound env id = Map.insertWith Set.union key (Set.singleton id) level
    distinguish' key [] ((ScalarT (TypeVarT _ _) _):_) = level
    distinguish' key [] (t:_) = Map.insertWith Set.union key (Set.singleton $ scalarName t) level
    distinguish' key args [] = distinguish' key [] args
    distinguish' key (arg:args) (arg':args') = 
        let level' = distinguish env level key arg arg'
        in if level' /= level then level' -- if we get several new representations, stop refining
                              else distinguish' key args args' -- otherwise append the current arg to the recursive result
-- TODO: need change to support higher order functions
distinguish env level key (ScalarT (TypeVarT _ id) _) (ScalarT (TypeVarT _ id') _) | id == id' = level
distinguish env level key (ScalarT (TypeVarT _ id) _) (ScalarT (TypeVarT _ id') _) | id /= id' = 
    let level' = if isBound env id  then Map.insertWith Set.union key (Set.singleton id ) level 
                                    else level 
    in           if isBound env id' then Map.insertWith Set.union key (Set.singleton id') level'
                                    else level'
distinguish env level key (ScalarT (TypeVarT _ id) _) t | isBound env id = Map.insertWith Set.union key (Set.singleton (scalarName t)) level
distinguish env level key t (ScalarT (TypeVarT _ id) _) | isBound env id = Map.insertWith Set.union key (Set.singleton (scalarName t)) level
distinguish env level key t tv@(ScalarT (TypeVarT _ _) _) = level
distinguish env level key (FunctionT _ tArg tRes) (FunctionT _ tArg' tRes') = 
    distinguish env level key tArg tArg' `Map.union` distinguish env level key tRes tRes'
distinguish env level _ AnyT _ = level
distinguish env level _ _ AnyT = level
distinguish env level key t1 t2 = if t1 == t2 then level 
                                              else Map.insertWith Set.union key (Set.fromList [scalarName t1, scalarName t2]) level

checkProgramType :: (MonadIO m) => Environment -> SType -> UProgram -> PNSolver m (Maybe RProgram)
checkProgramType env typ p@(Program (PSymbol sym) _) = do
    -- lookup the symbol type in current scope
    writeLog 3 $ text "Checking type for" <+> pretty p
    t <- case lookupSymbol sym 0 env of
            Nothing  -> error $ "checkProgramType: cannot find symbol " ++ sym ++ " in the current environment"
            Just sch -> freshType sch
    -- solve the type constraint t == typ
    writeLog 3 $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
    solveTypeConstraint env typ (shape t)
    st <- get
    if st ^. isChecked then return (Just $ Program (PSymbol sym) t)
                       else return Nothing
checkProgramType env typ p@(Program (PApp pFun pArg) _) = do
    -- first check the function type with @AnyT@ as parameters and @typ@ as returns
    writeLog 3 $ text "Checking type for" <+> pretty p
    fun <- checkProgramType env (FunctionT "x" AnyT typ) pFun
    case fun of
        Nothing -> return Nothing -- if the check fails, stop here and return error state
        Just f  -> checkArgumentType f
  where
    checkArgumentType f = do -- otherwise continue check for the argument type
        let FunctionT _ tArg tRet = typeOf f
        arg <- checkProgramType env (shape tArg) pArg
        case arg of
            Nothing -> actualArgumentType >> return Nothing
            Just a  -> return $ Just (Program (PApp f a) tRet)
    buildFunctionT t@(ScalarT _ _) tRet = FunctionT "x" t tRet
    buildFunctionT t@(FunctionT x tArg tRes) tRet = FunctionT x tArg (buildFunctionT tRes tRet)
    actualArgumentType = do
        st <- get
        -- distinguish the argument type
        let (expected, actual) = st ^. typingError
        -- writeLog 3 $ text "Distinguishing" <+> pretty expected <+> text "and" <+> pretty actual
        -- modify $ set abstractionLevel (distinguish (st' ^. abstractionLevel) "" expected actual)
        writeLog 3 $ text "actual argument type is" <+> pretty actual
        -- distinguish the application type
        modify $ set isChecked True -- temporarily open the checking channel
        fun <- checkProgramType env (buildFunctionT actual AnyT) pFun
        modify $ set isChecked False -- close after the local type checking
        st' <- get
        case fun of
            Nothing -> modify $ over typingError (over mapped lastType)
            Just f  -> do
                let FunctionT _ tArg tRet = typeOf f
                let typ' = stypeSubstitute (st' ^. typeAssignment) typ
                let tRet' = stypeSubstitute (st' ^. typeAssignment) (shape tRet)
                writeLog 3 $ text "Distinguishing" <+> pretty typ' <+> text "and" <+> pretty tRet'
                modify $ set abstractionLevel (distinguish env (st' ^. abstractionLevel) "" typ' tRet')
                modify $ set typingError (typ', tRet')

-- peel until we get a E-term
checkProgramType env typ (Program (PFun x body) _) = checkProgramType env typ body

solveTypeConstraint :: (MonadIO m) => Environment -> SType -> SType -> StateT TypingState m ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | id == id' = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | isBound env id = do
    st <- get
    if id' `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv
            solveTypeConstraint env tv typ
        else modify $ over typeAssignment (Map.insert id' tv)
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) = do
    st <- get 
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv'
            solveTypeConstraint env typ tv'
        else if id' `Map.member` (st ^. typeAssignment)
            then do
                let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
                writeLog 3 $ text "Solving constraint" <+> pretty tv <+> "==" <+> pretty typ
                solveTypeConstraint env tv typ
            else do
                modify $ over typeAssignment (Map.insert id tv')
                -- modify $ set isChecked False
                -- error "solveTypeConstraint: free type variables on both sides"

solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = do
    st <- get
    modify $ set isChecked False
    writeLog 3 $ text "Distinguishing" <+> pretty tv <+> text "and" <+> pretty t
    modify $ set abstractionLevel (distinguish env (st ^. abstractionLevel) "" tv t)
    modify $ set typingError (tv, t)
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty t
            solveTypeConstraint env typ t
        else do
            modify $ over typeAssignment (Map.insert id t)
            -- modify $ set isChecked False
            -- writeLog 3 $ text "Distinguishing" <+> pretty tv <+> text "and" <+> pretty t
            -- modify $ set abstractionLevel (distinguish (st ^. abstractionLevel) "" tv t)
            -- modify $ set typingError (tv, t)
solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 $ text "Solving constraint" <+> pretty tArg <+> "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    st <- get
    when (st ^. isChecked) (do
        writeLog 3 $ text "Solving constraint" <+> pretty tRet <+> "==" <+> pretty tRet'
        solveTypeConstraint env tRet tRet')
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id /= id' = do
    -- error $ "Check fail for " ++ show t1 ++ " and " ++ show t2
    st <- get
    modify $ set isChecked False
    writeLog 3 $ text "Distinguishing" <+> pretty t1 <+> text "and" <+> pretty t2
    modify $ set abstractionLevel (distinguish env (st ^. abstractionLevel) "" t1 t2)
    modify $ set typingError (t1, t2)
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id == id' = do
    (expected, actual) <- solveTypeConstraint' env tArgs tArgs'
    st <- get
    when (not $ st ^. isChecked) $ do
        let t1' = ScalarT (DatatypeT id expected []) ()
        let t2' = ScalarT (DatatypeT id' actual []) ()
        writeLog 3 $ text "Distinguishing" <+> pretty t1' <+> text "and" <+> pretty t2'
        modify $ set abstractionLevel (distinguish env (st ^. abstractionLevel) "" t1' t2')
        modify $ set typingError (t1', t2')
  where
    solveTypeConstraint' _ []  [] = return ([],[])
    solveTypeConstraint' env (ty:tys) (ty':tys') = do
        writeLog 3 $ text "Solving constraint" <+> pretty ty <+> "==" <+> pretty ty'
        solveTypeConstraint env ty ty'
        st <- get
        -- if the checking between ty and ty' succeeds, proceed to others
        if st ^. isChecked
            then do
                (expected, actual) <- solveTypeConstraint' env tys tys'
                st' <- get
                -- if the typing check fails, prepend current argument to the error message
                if not $ st' ^. isChecked
                    then return (ty:expected , ty':actual)
                    else return ([], [])
            else do
                writeLog 3 $ pretty ty <+> text "does not match with" <+> pretty ty'
                let (expected, actual) = st ^. typingError
                return ([expected], [actual])

checkType :: (MonadIO m) => Environment -> RType -> UProgram -> PNSolver m (Either RProgram SType)
checkType env typ p = do
    writeLog 1 $ text "Find program" <+> pretty p
    modify $ set isChecked True
    p' <- checkProgramType env (shape typ) p
    st <- get
    if st ^. isChecked then return $ Left (fromJust p') 
                       else return $ Right (snd (st ^. typingError))

findPath :: (MonadIO m) => Environment -> RType -> PNSolver m String
findPath env dst = do
    writeLog 3 $ text "Start looking for a new path"
    st <- get
    writeLog 1 $ text "Current abstraction level" <+> pretty (Map.toList $ Map.map Set.toList $ st ^. abstractionLevel)
    -- abstract all the symbols in the current environment
    start <- liftIO $ getCurrentTime
    freshSymbols <- mapM (\(id, sch) -> do t <- freshType sch; return (id, t)) $ Map.toList $ allSymbols env
    let absSymbols = foldr (\(id, t) -> Map.insert id $ abstract (env ^. boundTypeVars) (st ^. abstractionLevel) "" 
                                                      $ shape t) Map.empty freshSymbols
    let argIds = Map.keys (env ^. arguments)
    let args = map toMonotype $ Map.elems (env ^. arguments)
    sigs <- if Map.null (st ^. currentSigs) then instantiate env absSymbols else return (st ^. currentSigs)
    modify $ set currentSigs sigs
    writeLog 3 $ text "Current signature abstractions" <+> text (show (Map.toList sigs))

    -- encode all the abstracted signatures into JSON string and pass it to SyPet
    symbols <- liftIO $ reflect (Text.pack . LB8.unpack . Aeson.encode $ map (uncurry encodeFunction) 
                                $ Map.toList $ foldr Map.delete sigs argIds)
    tgt <- liftIO $ reflect (Text.pack $ show $ abstract (env ^. boundTypeVars) (st ^. abstractionLevel) "" $ shape dst)
    srcTypes <- liftIO $ reflect (map (Text.pack . show . abstract (env ^. boundTypeVars) (st ^. abstractionLevel) "" . shape) args)
    argNames <- liftIO $ reflect (map Text.pack argIds)
    excludeLists <- liftIO $ reflect (map Text.pack (st ^. currentSolutions))
    loc <- liftIO $ reflect (st ^. currentLoc)
    end <- liftIO $ getCurrentTime
    writeLog 3 $ text "Time for preparing data" <+> text (show $ diffUTCTime end start)
    code <- liftIO $ [java| {
        java.util.List<java.lang.String> srcTypes = java.util.Arrays.asList($srcTypes);
        java.util.List<java.lang.String> argNames = java.util.Arrays.asList($argNames);
        java.util.List<java.lang.String> solutions = java.util.Arrays.asList($excludeLists);
        String tgtType = $tgt;
        System.out.println("Arguments:" + srcTypes.toString());
        System.out.println("Target:" + tgtType);
        cmu.edu.utils.SynquidUtil.init(srcTypes, argNames, tgtType, $symbols, solutions, $loc);
        cmu.edu.utils.SynquidUtil.buildNextEncoding();
        //java.util.List<java.lang.String> res = cmu.edu.utils.SynquidUtil.synthesize();
        java.lang.String res = cmu.edu.utils.SynquidUtil.synthesize();
        return res;
    } |]
    codeText <- liftIO $ reify code
    -- parse the result into AST in Synquid
    -- let codeCheck = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" (Text.unpack codeText)
    return $ Text.unpack codeText

findProgram :: (MonadIO m) => Environment -> RType -> PNSolver m RProgram
findProgram env dst = do
    jsonResult <- findPath env dst
    -- liftIO $  print jsonResult
    (code, loc) <- liftIO $ parseJson $ LB8.pack jsonResult  
    let prog = case parseExp code of
                ParseOk exp -> toSynquidProgram exp
                ParseFailed loc err -> error err
    -- checkRes <- case path of
    --                 Left err   -> error $ "Parse error in\n" ++ show path
    --                 Right decl -> checkType env dst $ head decl
    checkRes <- checkType env dst prog
    modify $ set currentLoc loc
    if isLeft checkRes
        then do -- FIXME when the program type checks and prepare to find the next one, save time for reconstructing the components
            let Left p = checkRes
            -- add the satisfied solution to current state for future exclusion
            modify $ over currentSolutions ((:) code)
            st <- get
            liftIO $ putStrLn "*******************SOLUTION*********************"
            liftIO $ print p
            liftIO $ putStrLn "************************************************"
            return p -- return the first correct program we find
        else do
            modify $ set currentSigs Map.empty
            findProgram env dst
  where
    parseJson jsonResult = do
        let decodeResult = Aeson.eitherDecode jsonResult
        let result = case decodeResult of
                        Left err -> error err
                        Right v  -> case v of 
                            Aeson.Object contents -> ((fromJust $ HashMap.lookup "code" contents)
                                                     -- ,(fromJust $ HashMap.lookup "satList" contents)
                                                     ,(fromJust $ HashMap.lookup "loc" contents))
                            _  -> error "findProgram: unknown error when decoding the JSON string"
        -- let (Aeson.String code, Aeson.Array arr, Aeson.Number num) = result
        let (Aeson.String code, Aeson.Number num) = result
        return ( Text.unpack code
               -- , map (\(Aeson.Number num) -> (fromJust (toBoundedInteger num))::Int32) (Vector.toList arr)
               , (fromJust (toBoundedInteger num))::Int32)

findFirstN :: (MonadIO m) => Environment -> RType -> Int -> Int -> PNSolver m RProgram
findFirstN env dst n cnt | cnt == n  = findProgram env dst
findFirstN env dst n cnt | otherwise = findProgram env dst >> findFirstN env dst n (cnt+1)

writeLog level msg = do
    st <- get
    if level <= st ^. logLevel then traceShow (plain msg) $ return () else return ()