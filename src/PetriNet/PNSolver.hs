{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module PetriNet.PNSolver where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics
import Control.Lens
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Data (Data)
import qualified Data.Char as Char
import Data.Either hiding (fromLeft, fromRight)
import Data.List.Extra
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
import qualified Z3.Monad as Z3

import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Program
import Synquid.Type
import Synquid.Logic
import Synquid.Util
import Synquid.Error
import Synquid.Pretty
import PetriNet.AbstractType
import PetriNet.PNBuilder
import PetriNet.PNEncoder
import PetriNet.Encoder
import PetriNet.PNEncoderSMT
import HooglePlus.CodeFormer
import PetriNet.GHCChecker
import Database.Convert
import Database.Generate

-- makeLenses ''InstantiateState
data SolverState = SolverState {
    _nameCounter :: Map Id Int,  -- name map for generating fresh names (type variables, parameters)
    _typeAssignment :: Map Id SType,  -- current type assignment for each type variable
    _typingError :: (SType, SType), -- typing error message, represented by the expected type and actual type
    _abstractionSemantic :: AbstractionSemantic, -- current abstraction level
    _isChecked :: Bool, -- is the current state check passed
    _currentSolutions :: [RProgram], -- type checked solutions
    _currentLoc :: Int, -- current solution depth
    _functionMap :: HashMap Id FunctionCode,
    _targetType :: Id,
    _sourceTypes :: [Id],
    _paramNames :: [Id],
    _logLevel :: Int -- temporary for log level
}

emptySolverState = SolverState {
    _nameCounter = Map.empty,
    _typeAssignment = Map.empty,
    _typingError = (AnyT, AnyT),
    _abstractionSemantic = Map.empty,
    _isChecked = True,
    _currentSolutions = [],
    _currentLoc = 1,
    _functionMap = HashMap.empty,
    _targetType = "",
    _sourceTypes = [],
    _paramNames = [],
    _logLevel = 0
}

makeLenses ''SolverState

type PNSolver m = StateT SolverState m

data PathSolver =
    SATSolver
  | SMTSolver
  deriving(Data, Show, Eq)

abstractParamList :: AbstractSkeleton -> [AbstractSkeleton]
abstractParamList t@(ADatatypeT _ _) = [t]
abstractParamList t@(AExclusion _)   = [t]
abstractParamList (AFunctionT tArg tFun) =
    case tFun of
        ADatatypeT _ _  -> [tArg]
        AExclusion _    -> [tArg]
        ATypeVarT  _    -> [tArg]
        _               -> (tArg) : (abstractParamList tFun)
abstractParamList t@(AOneOf _) = [t]
abstractParamList t = error $ "Unexpected type " ++ show t

lastAbstractType :: AbstractSkeleton -> AbstractSkeleton
lastAbstractType (AFunctionT tArg tFun) = lastAbstractType tFun
lastAbstractType t                      = t

encodeFunction :: Id -> AbstractSkeleton -> FunctionCode
encodeFunction id t@(AFunctionT tArg tRet) = FunctionCode id hoParams params (show $ lastAbstractType t)
  where
    base = (0, [])
    hoFun x (i, a) = (i+1, (encodeFunction ("f" ++ show i) x) : a)
    paramFun x (i, a) = if isAFunctionT x then (i+1, ("f" ++ show i):a) else (i, (show x):a)
    (_, hoParams) = foldr hoFun base $ filter isAFunctionT (abstractParamList t)
    (_, params) = foldr paramFun base (abstractParamList t)
encodeFunction id t                        = FunctionCode id [] [] (show t)

type ClassConstraint = [(Id, [Id])]

freshId :: (MonadIO m) => Id -> PNSolver m Id
freshId prefix = do
    indices <- flip (^.) nameCounter <$> get
    let idx = Map.findWithDefault 0 prefix indices
    modify (over nameCounter $ Map.insert prefix (idx+1))
    return $ prefix ++ "_" ++ show idx

-- | Replace all bound type variables with fresh free variables
freshType :: (MonadIO m) => RSchema -> PNSolver m (RType, ClassConstraint)
freshType sch = freshType' Map.empty [] sch
  where
    freshType' subst constraints (ForallT (a,constraint) sch) = do
        a' <- freshId "A"
        freshType' (Map.insert a (vart a' ftrue) subst) ((a',constraint):constraints) sch
    freshType' subst constraints (Monotype t) = return (typeSubstitute subst $ t, constraints)

constructType :: Environment -> AbstractionSemantic -> Id -> [AbstractSkeleton]
constructType env abstraction key
    | key `Map.notMember` abstraction && currDt == "" = [AExclusion Set.empty]
    | key `Map.notMember` abstraction && isDt = [ADatatypeT currDt $ replicate dtParams (AExclusion Set.empty)]
    | key `Map.notMember` abstraction = [ATypeVarT currDt, AExclusion (Set.singleton currDt)] -- this is bounded type variables
    | currDt == "" = nubOrd $ AExclusion allIds : anyOneIds ++ concatMap (constructType env abstraction . (++) (key ++ ",")) (Set.toList allIds)
    | isDt = nubOrd $ map (ADatatypeT currDt) $ listOfN dtParams $ (AExclusion allIds) : anyOneIds ++ concatMap (constructType env abstraction . (++) (key ++ ",")) (Set.toList allIds)
    | otherwise = nubOrd $ [ATypeVarT currDt, AExclusion (Set.singleton currDt)] -- this is bounded type variables
  where
    currDt = if null (splitBy ',' key)
                then ""
                else if Map.member (last $ splitBy ',' key) (env ^. datatypes) || Char.isLower (head $ last $ splitBy ',' key)
                    then last $ splitBy ',' key
                    else ""
    nextIdSet = Map.findWithDefault Set.empty key abstraction
    anyOneIds = Set.toList $ Set.map (AOneOf . fromRight) $ Set.filter isRight nextIdSet
    allIds = Set.foldr (\i -> Set.union (if isLeft i then Set.singleton (fromLeft i) else fromRight i)) Set.empty nextIdSet
    isDt = Char.isUpper $ head currDt
    dtParams = case Map.lookup currDt (env ^. datatypes) of
        Just def -> length (def ^. typeParams)
        Nothing  -> error $ "constructType: cannot find datatype " ++ currDt ++ " in the environment\n" ++ "here are the datatypes: " ++ (show $ Map.keys $ env ^. datatypes)

    listOfN 0 _  = []
    listOfN 1 xs = map (: []) xs
    listOfN n xs = [x:y | x <- xs, y <- listOfN (n-1) xs]

instantiate :: (MonadIO m) => Environment -> [(Id, AbstractSkeleton)] -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs =
    Map.fromList <$> instantiate' sigs (filter (not . hasAbstractVar . snd) sigs)
  where
    removeSuffix id ty = (removeLast '_' id, ty)

    instantiate' sigs sigsAcc = do
        st <- get
        let typs = constructType env (st ^. abstractionSemantic) ""
        writeLog 3 $ text "Current abstraction semantics:" <+> text (show (st ^. abstractionSemantic))
        writeLog 3 $ text "Current abstract types:" <+> pretty typs
        sigs' <- foldM (\acc -> (<$>) ((++) acc) . uncurry (instantiateWith env typs)) [] sigs
        return $ nubOrdOn (uncurry removeSuffix) (sigsAcc ++ sigs')

instantiateWith :: (MonadIO m) => Environment -> [AbstractSkeleton] -> Id -> AbstractSkeleton -> PNSolver m [(Id, AbstractSkeleton)]
instantiateWith env typs id sk = do
    let vars = Set.toList $ allAbstractVar sk
    let multiSubsts = map (zip vars) $ multiPermutation (length vars) typs
    let substedSymbols = map (foldr (\(id, t) acc -> abstractSubstitute (env ^. boundTypeVars) id t acc) sk) multiSubsts
    st <- get
    foldrM (\t accMap -> do
        newId <- newSymbolName id
        return $ (newId, cutoff (st ^. abstractionSemantic) "" t):accMap) [] substedSymbols
    where
        multiPermutation len elmts | len == 0 = []
        multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
        multiPermutation len elmts            = nubOrd $ [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]

        newSymbolName prefix = do
            indices <- flip (^.) nameCounter <$> get
            let idx = Map.findWithDefault 0 prefix indices
            modify (over nameCounter $ Map.insert prefix (idx+1))
            return $ prefix ++ "_" ++ show idx

cutoff :: AbstractionSemantic -> Id -> AbstractSkeleton -> AbstractSkeleton
cutoff semantic key (ADatatypeT id tArgs) =
    if id `Set.member` possibleIds then ADatatypeT id (map (cutoff semantic (key ++ "," ++ id)) tArgs)
                               else AExclusion possibleIds
  where
    currIds        = Map.findWithDefault Set.empty key semantic
    unionEither id = Set.union (if isLeft id then Set.singleton (fromLeft id) else fromRight id)
    possibleIds    = Set.foldr unionEither Set.empty currIds
cutoff semantic key (AFunctionT tArg tRet) = AFunctionT (cutoff semantic key tArg) (cutoff semantic key tRet)
cutoff semantic key t@(AExclusion _) = AExclusion possibleIds
  where
    currIds        = Map.findWithDefault Set.empty key semantic
    unionEither id = Set.union (if isLeft id then Set.singleton (fromLeft id) else fromRight id)
    possibleIds    = Set.foldr unionEither Set.empty currIds
cutoff _ _ t = t

distinguish :: Environment -> AbstractionSemantic -> Map Id SType -> Id -> SType -> SType -> AbstractionSemantic
distinguish env semantic _ _ AnyT _ = semantic
distinguish env semantic _ _ _ AnyT = semantic
distinguish env semantic _ _ ErrorT _ = semantic
distinguish env semantic _ _ _ ErrorT = semantic
distinguish env semantic _ key (ScalarT (DatatypeT id _ _) _) (ScalarT (DatatypeT id' _ _) _) | id /= id' =
    Map.insertWith Set.union key (Set.fromList [Left id, Left id']) semantic
distinguish env semantic tass key (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' =
    distinguish' (key ++ "," ++ id) tass tArgs tArgs'
  where
    -- split the current node into two when we cannot distinguish the error type from its abstract representation
    distinguish' key _ [] [] = semantic -- error "distinguish error: these two types are exactly the same"
    -- distinguish' key _ [] ((ScalarT (TypeVarT _ id) _):_) | isBound env id = Map.insertWith Set.union key (Set.singleton $ Left id) semantic
    distinguish' key tass [] ((ScalarT (TypeVarT _ id) _):_) | id `Map.member` tass = distinguish' key tass [] [fromJust $ Map.lookup id tass]
    distinguish' key _ [] ((ScalarT (TypeVarT _ id) _):_) = semantic
    distinguish' key tass [] (t:_) = Map.insertWith Set.union key (Set.singleton $ Left $ scalarName (stypeSubstitute tass t)) semantic
    distinguish' key tass args [] = distinguish' key tass [] args
    distinguish' key tass (arg:args) (arg':args') =
        let semantic' = distinguish env semantic tass key arg arg'
        in if semantic' /= semantic then semantic' -- if we get several new representations, stop refining
                              else distinguish' key tass args args' -- otherwise append the current arg to the recursive result
-- TODO: need change to support higher order functions
distinguish env semantic tass key (ScalarT (TypeVarT _ id) _) (ScalarT (TypeVarT _ id') _) | id == id' = semantic
-- has unsubtituted type variables, substitute them and recheck
distinguish env semantic tass key (ScalarT (TypeVarT _ id) _) t | id `Map.member` tass =
    distinguish env semantic tass key (fromJust $ Map.lookup id tass) t
distinguish env semantic tass key t (ScalarT (TypeVarT _ id) _) | id `Map.member` tass =
    distinguish env semantic tass key t (fromJust $ Map.lookup id tass)
-- has unbounded type variable, view it as AnyT
distinguish env semantic _ key (ScalarT (TypeVarT _ id) _) t | not (isBound env id) = semantic
distinguish env semantic _ key t (ScalarT (TypeVarT _ id) _) | not (isBound env id) = semantic
-- has bounded type variable
distinguish env semantic tass key (ScalarT (TypeVarT {}) _) (ScalarT (TypeVarT {}) _) = semantic
    -- Map.insertWith Set.union key (Set.singleton $ Left $ scalarName (stypeSubstitute tass t)) semantic
distinguish env semantic tass key (ScalarT (TypeVarT _ _) _) t = Map.insertWith Set.union key (Set.singleton (Left $ scalarName t)) semantic
distinguish env semantic tass key t tv@(ScalarT (TypeVarT _ _) _) = distinguish env semantic tass key tv t
    -- Map.insertWith Set.union key (Set.singleton $ Left $ scalarName (stypeSubstitute tass t)) semantic
distinguish env semantic tass key (FunctionT _ tArg tRes) (FunctionT _ tArg' tRes') =
    Map.unionWith Set.union (distinguish env semantic tass key tArg tArg')
                            (distinguish env semantic tass key tRes tRes')
distinguish env semantic tass key t1 t2 =
    let t1' = stypeSubstitute tass t1
        t2' = stypeSubstitute tass t2
    in if t1' == t2' then semantic
                     else Map.insertWith Set.union key (Set.fromList [Left $ scalarName t1', Left $ scalarName t2']) semantic

topDownCheck :: (MonadIO m) => Environment -> SType -> UProgram -> PNSolver m RProgram
topDownCheck env typ p@(Program (PSymbol sym) _) = do
    -- lookup the symbol type in current scope
    writeLog 3 $ text "Checking type for" <+> pretty p
    -- liftIO $ print (allSymbols env)
    (t,c) <- case lookupSymbol sym 0 env of
                Nothing  ->
                    case lookupSymbol ("(" ++ sym ++ ")") 0 env of -- symbol name with parenthesis
                        Nothing  -> do
                            writeLog 2 $ text "topDownCheck: cannot find symbol" <+> text sym <+> text "in the current environment"
                            modify $ set isChecked False
                            return (AnyT, [])
                        Just sch -> freshType sch
                Just sch -> freshType sch
    -- solve the type constraint t == typ
    writeLog 3 $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
    solveTypeConstraint env typ (shape t) (Map.fromList c)
    ifM (view isChecked <$> get)
        (return $ Program (PSymbol sym) t)
        (return $ Program (PSymbol sym) (addTrue typ))
topDownCheck env typ p@(Program (PApp pFun pArg) _) = do
    -- first check the function type with @AnyT@ as parameters and @typ@ as returns
    writeLog 3 $ text "Checking type for" <+> pretty p
    fun <- topDownCheck env (FunctionT "x" AnyT typ) pFun
    ifM (view isChecked <$> get)
        (checkArgumentType fun)
        (return $ Program (PApp fun uHole) (addTrue typ))
  where
    checkArgumentType f = do -- otherwise continue check for the argument type
        let FunctionT _ tArg tRet = typeOf f
        arg <- topDownCheck env (shape tArg) pArg
        -- return $ Program (PApp f arg) (addTrue typ)
        ifM (view isChecked <$> get)
            (return $ Program (PApp f arg) tRet)
            (return $ Program (PApp f arg) (addTrue typ)) -- if check fails
topDownCheck env typ@(FunctionT _ tArg tRet) p@(Program (PFun x body) _) = do
    writeLog 3 $ text "Checking type for" <+> pretty p
    body' <- topDownCheck (addVariable x (addTrue tArg) env) tRet body
    ifM (view isChecked <$> get)
        (return $ Program (PFun x body') (FunctionT x (addTrue tArg) (typeOf body')))
        (return $ Program (PFun x body') (addTrue typ))
-- topDownCheck env typ p@(Program (PFun x body) AnyT) = do
--     writeLog 3 $ text "Checking type for" <+> pretty p
--     topDownCheck (addVariable x AnyT env) typ body

bottomUpCheck :: (MonadIO m) => Environment -> RProgram -> PNSolver m (Maybe (RProgram, ClassConstraint))
bottomUpCheck env p@(Program PHole _) = return $ Just (p, [])
bottomUpCheck env p@(Program (PSymbol sym) typ) = do
    -- lookup the symbol type in current scope
    writeLog 3 $ text "Checking type for" <+> pretty p
    (t,c) <- case lookupSymbol sym 0 env of
                Nothing  ->
                    case lookupSymbol ("(" ++ sym ++ ")") 0 env of -- symbol name with parenthesis
                        Nothing  -> do
                            modify $ set isChecked False
                            return (AnyT, []) -- error $ "bottomUpCheck: cannot find symbol " ++ sym ++ " in the current environment"
                        Just sch -> freshType sch
                Just sch -> freshType sch
    -- solveTypeConstraint env (shape typ) (shape t)
    writeLog 3 $ text "Distinguishing" <+> pretty (shape typ) <+> text "and" <+> pretty (shape t)
    tass <- view typeAssignment <$> get
    curr <- view abstractionSemantic <$> get
    modify $ set abstractionSemantic $ distinguish env curr tass "" (shape typ) (shape t)
    ifM (view isChecked <$> get)
        (return $ Just $ (Program (PSymbol sym) t, c))
        (return Nothing)
bottomUpCheck env (Program (PApp pFun pArg) typ) = do
    fun <- bottomUpCheck env pFun
    case fun of
        Nothing -> return Nothing
        Just (f,fConstraints) -> do
            let FunctionT _ tArg tRet = typeOf f
            arg <- bottomUpCheck env pArg
            case arg of
                Nothing -> return Nothing
                Just (a, aConstraints) -> do
                    writeLog 3 $ text "Solving constraint for" <+> pretty a <+> pretty (shape $ typeOf a) <+> text "==" <+> pretty (shape tArg)
                    solveTypeConstraint env (shape $ typeOf a) (shape tArg) (Map.fromList (fConstraints ++ aConstraints))
                    tass <- view typeAssignment <$> get
                    curr <- view abstractionSemantic <$> get
                    writeLog 3 $ text "Distinguishing" <+> pretty (shape $ typeOf a) <+> text "and" <+> pretty (shape tArg)
                    let argLevel = distinguish env curr tass "" (shape $ typeOf a) (shape tArg)
                    writeLog 3 $ text "Distinguishing" <+> pretty (shape typ) <+> text "and" <+> pretty (shape tRet)
                    modify $ set abstractionSemantic $ distinguish env argLevel tass "" (shape typ) (shape tRet)
                    ifM (view isChecked <$> get)
                        (return $ Just $ (Program (PApp f a) tRet, fConstraints ++ aConstraints)) -- Either type to store the conflict types
                        (return Nothing)
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 $ text "Checking type for" <+> pretty p
    body' <- bottomUpCheck (addVariable x (addTrue tArg) env) body
    case body' of
        Nothing -> return Nothing
        Just (b,c) -> return $ Just (Program (PFun x b) (FunctionT x tArg (typeOf b)), c)

solveTypeConstraint :: (MonadIO m) => Environment -> SType -> SType -> Map Id [Id] -> PNSolver m ()
solveTypeConstraint _ AnyT _ _ = return ()
solveTypeConstraint _ _ AnyT _ = return ()
solveTypeConstraint _ ErrorT _ _ = return ()
solveTypeConstraint _ _ ErrorT _ = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) _ | id == id' = return ()
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) constraints | isBound env id && isBound env id' =
    modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) constraints | isBound env id = do
    st <- get
    if id' `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv
            solveTypeConstraint env tv typ constraints
        else unify env id' tv constraints
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) constraints = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty tv'
            solveTypeConstraint env typ tv' constraints
        else if id' `Map.member` (st ^. typeAssignment)
            then do
                let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
                writeLog 3 $ text "Solving constraint" <+> pretty tv <+> "==" <+> pretty typ
                solveTypeConstraint env tv typ constraints
            else do
                unify env id tv' constraints
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t _ | isBound env id = modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t constraints = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty t
            solveTypeConstraint env typ t constraints
        else do
            unify env id t constraints
solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) constraints = solveTypeConstraint env tv t constraints
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') constraints = do
    writeLog 3 $ text "Solving constraint" <+> pretty tArg <+> "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg' constraints
    st <- get
    when (st ^. isChecked) (do
        writeLog 3 $ text "Solving constraint" <+> pretty tRet <+> "==" <+> pretty tRet'
        solveTypeConstraint env tRet tRet' constraints)
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) _ | id /= id' = do
    modify $ set isChecked False
solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) constraints | id == id' = do
    solveTypeConstraint' env tArgs tArgs'
  where
    solveTypeConstraint' _ []  [] = return ()
    solveTypeConstraint' env (ty:tys) (ty':tys') = do
        writeLog 3 $ text "Solving constraint" <+> pretty ty <+> "==" <+> pretty ty'
        solveTypeConstraint env ty ty' constraints
        checked <- view isChecked <$> get
        -- if the checking between ty and ty' succeeds, proceed to others
        when (checked) (solveTypeConstraint' env tys tys')
solveTypeConstraint env t1 t2 _ = error $ "unknown types " ++ show t1 ++ " or " ++ show t2

unify :: (MonadIO m) => Environment -> Id -> SType -> Map Id [Id] -> PNSolver m ()
unify env v t _ = modify $ over typeAssignment (Map.insert v t)

checkType :: (MonadIO m) => Environment -> RType -> UProgram -> PNSolver m (Maybe RProgram)
checkType env typ p = do
    writeLog 1 $ text "Find program" <+> pretty p
    modify $ set isChecked True
    top <- topDownCheck env (shape typ) p
    ifM (view isChecked <$> get)
        (return $ Just top)
        (do
            liftIO $ putStrLn "INSTRUMENTED: DIY Type checker failed this program"
            -- writeLog 3 $ text "Top down type checking get" <+> pretty top
            -- modify $ set isChecked True
            -- bottomUpCheck env top
            return Nothing)

initNet :: MonadIO m => Environment -> PNSolver m PetriNet
initNet env = do
    startTime <- liftIO $ getCurrentTime
    let binds = env ^. boundTypeVars
    abstraction <- view abstractionSemantic <$> get
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    -- first abstraction all the symbols with fresh type variables and then instantiate them
    absSymbols <- mapM (uncurry (abstractSymbol binds abstraction))
                      $ Map.toList $ allSymbols env
    sigs <- instantiate env absSymbols
    symbols <- mapM (addEncodedFunction)
                    -- first order arguments are tokens but not transitions in petri net
                    $ Map.toList $ foldr Map.delete sigs $ Map.keys foArgs
    let srcTypes = map ( show
                       . abstract binds abstraction
                       . shape
                       . toMonotype) $ Map.elems foArgs
    modify $ set sourceTypes srcTypes
    let net = buildPetriNet symbols srcTypes
    endTime <- liftIO $ getCurrentTime
    let diff = diffUTCTime endTime startTime
    liftIO $ putStrLn $ "INSTRUMENTED: time spent creating graph: " ++ (show diff)
    return net
  where
    abstractSymbol binds abstraction id sch = do
        (t, _) <- freshType sch
        let absTy = abstract binds abstraction (shape t)
        return (id, absTy)

    addEncodedFunction (id, f) = do
        let ef = encodeFunction id f
        modify $ over functionMap (HashMap.insert id ef)
        return ef

resetEncoder :: (MonadIO m) => Environment -> RType -> PathSolver -> PetriNet -> PNSolver m EncoderType
resetEncoder env dst solver net = do
    let binds = env ^. boundTypeVars
    abstraction <- view abstractionSemantic <$> get
    let tgt = show $ abstract binds abstraction $ shape dst
    modify $ set targetType tgt
    let foArgs = Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    modify $ set paramNames $ Map.keys foArgs
    srcTypes <- view sourceTypes <$> get
    writeLog 2 $ text "parameter types are" <+> pretty srcTypes
    writeLog 2 $ text "return type is" <+> pretty tgt
    loc <- view currentLoc <$> get
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    liftIO $ case solver of
                SATSolver -> encoderInit net loc hoArgs srcTypes tgt
                SMTSolver -> encoderInitSMT net loc hoArgs srcTypes tgt

findPath :: (MonadIO m) => Environment -> RType -> PetriNet -> EncoderType -> PNSolver m (CodePieces, EncoderType)
findPath env dst net st = do
    (res, st') <- liftIO $ case st of
                            Left s -> encoderSolve s
                            Right s -> encoderSolveSMT s
    case res of
        [] -> do
            currSt <- get
            modify $ set currentLoc ((currSt ^. currentLoc) + 1)
            st'' <- case st' of
                        Left  _ -> resetEncoder env dst SATSolver net
                        Right _ -> resetEncoder env dst SMTSolver net
            findPath env dst net st''
        _  -> do
            fm <- view functionMap <$> get
            src <- view sourceTypes <$> get
            args <- view paramNames <$> get
            let sortedRes = sortOn snd res
            let sigNames = map removeSuffix
                         $ filter skipEntry
                         $ map (transitionId . fst) sortedRes
            let sigs = map (findFunction fm) sigNames
            let initialFormer = FormerState 0 HashMap.empty
            code <- liftIO $ evalStateT (generateProgram sigs src args) initialFormer
            return (code, st')
  where
    findFunction fm name =
        let name' = name
        in case HashMap.lookup name' fm of
            Just fc -> fc
            Nothing -> error $ "cannot find function " ++ name'

    skipEntry = not . isInfixOf "|entry"
    removeSuffix = removeLast '|'

findProgram :: (MonadIO m) => Environment -> RType -> PetriNet -> EncoderType -> PNSolver m (RProgram, EncoderType)
findProgram env dst net st = do
    (codeResult, st') <- findPath env dst net st
    liftIO $ print codeResult
    checkResult <- mapM parseAndCheck $ Set.toList codeResult
    let codes = catMaybes checkResult
    solutions <- view currentSolutions <$> get
    if null codes
        then do
            -- net' <- initNet env
            -- st'' <- resetEncoder env dst net'
            -- findProgram env dst net' st''
            findProgram env dst net st'
        else do
            let solutionProgram = head codes
            if head codes `elem` solutions
            then do
                findProgram env dst net st'
            else do
                doesHaskellTypeCheck <- liftIO $ haskellTypeChecks env dst solutionProgram
                if not doesHaskellTypeCheck
                    then do
                        liftIO $ putStrLn "INSTRUMENTED: GHC Type checker failed"
                        modify $ over currentSolutions ((:) solutionProgram)
                        findProgram env dst net st'
                    else do
                        liftIO $ putStrLn "*******************SOLUTION*********************"
                        liftIO $ print $ solutionProgram
                        liftIO $ putStrLn "************************************************"
                        modify $ over currentSolutions ((:) solutionProgram)
                        return $ (solutionProgram, st')
  where
    parseAndCheck code = do
        let prog = case parseExp code of
                    ParseOk exp -> toSynquidProgram exp
                    ParseFailed loc err -> error err
        checkType env dst prog

findFirstN :: (MonadIO m) => Environment -> RType -> PetriNet -> EncoderType -> Int -> PNSolver m RProgram
findFirstN env dst net st cnt | cnt == 1  = do
    (res, _) <- findProgram env dst net st
    return res
findFirstN env dst net st cnt | otherwise = do
    (_, st') <- findProgram env dst net st
    findFirstN env dst net st' (cnt-1)

writeLog level msg = do
    st <- get
    if level <= st ^. logLevel then traceShow (plain msg) $ return () else return ()
