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
import Data.List.Extra (nubOrd)
import Data.Maybe
import Data.Either hiding (fromLeft, fromRight)
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
import Synquid.Util
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
freshType :: (MonadIO m) => RSchema -> PNSolver m (RType, [(Id,[Id])])
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

-- TODO: start with only the datatypes in the query
instantiate :: (MonadIO m) => Environment -> Map Id (AbstractSkeleton,[(Id,[Id])]) -> PNSolver m (Map Id AbstractSkeleton)
instantiate env sigs = instantiate' sigs $ Map.map fst $ Map.filter (not . hasAbstractVar . fst) sigs
  where
    removeSuffix id ty = (removeLast '_' id, ty)
    constructType abstraction key | key `Map.notMember` abstraction =
        let currDt     = if null (splitBy ',' key) then "" else last $ splitBy ',' key
        in if currDt == "" then [AExclusion Set.empty]
                           else nubOrd $ if Char.isUpper $ head currDt
                                        then case Map.lookup currDt (env ^. datatypes) of
                                                Just def -> if null $ def ^. typeParams then [ADatatypeT currDt []] else [ADatatypeT currDt [AExclusion Set.empty]]
                                                Nothing  -> error $ "instantiate: cannot find datatype " ++ currDt ++ " in the environment"
                                        else [ATypeVarT currDt, AExclusion (Set.singleton currDt)] -- this is bounded type variables
    constructType abstraction key | key `Map.member`    abstraction =
        let nextKeys   = fromJust $ Map.lookup key abstraction
            currDt     = if null (splitBy ',' key) then "" else last $ splitBy ',' key
        in if currDt == ""
            then nubOrd $ [AExclusion nextKeys] ++ concatMap (constructType abstraction . (++) (key ++ ",")) (Set.toList nextKeys)
            else if Char.isUpper $ head currDt
                then case Map.lookup currDt (env ^. datatypes) of
                        Just def -> if null $ def ^. typeParams then [ADatatypeT currDt []] else nubOrd $ map (ADatatypeT currDt . (:[])) $ [AExclusion nextKeys] ++ concatMap (constructType abstraction . (++) (key ++ ",")) (Set.toList nextKeys)
                        Nothing  -> error $ "instantiate: cannot find datatype " ++ currDt ++ " in the environment"
                else nubOrd $ [ATypeVarT currDt, AExclusion (Set.singleton currDt)] -- this is bounded type variables
    instantiate' sigs sigsAcc = do
        st <- get
        -- writeLog 0 $ text "current abstraction types:" <+> pretty (constructType (st ^. abstractionLevel) "")
        let typs = constructType (st ^. abstractionLevel) ""
        -- let typs = Set.fromList $ concatMap (allAbstractBase (env ^. boundTypeVars)) (Map.union sigsAcc sigs)
        sigs' <- foldM (\acc -> (<$>) (Map.union acc) . uncurry (instantiateWith env typs)) Map.empty (Map.toList sigs)
        return $ Map.fromList $ nubOrdOn (uncurry removeSuffix) $ Map.toList $ Map.union sigsAcc sigs'

instantiateWith :: (MonadIO m) => Environment -> [AbstractSkeleton] -> Id -> (AbstractSkeleton, [(Id,[Id])]) -> PNSolver m (Map Id AbstractSkeleton)
instantiateWith env typs id (sk, constraints) = do
    -- liftIO $ print id
    let vars = Set.toList $ allAbstractVar sk
    let constraintMap = Map.fromList constraints
    -- liftIO $ print constraintMap
    -- liftIO $ print $ env ^. typeClasses
    -- liftIO $ print $ map (flip (Map.findWithDefault Set.empty) (env ^. typeClasses)) $ (Map.findWithDefault [] "A_14" constraintMap)
    -- liftIO $ print $ map (flip (Map.findWithDefault Set.empty) (env ^. typeClasses)) $ (Map.findWithDefault [] "A_15" constraintMap)
    let multiSubsts = map (zip vars) $ multiPermutation (length vars) typs
    -- liftIO $ print multiSubsts
    let validSubsts = filter (foldr (\(id, t) acc -> (unifierValid constraintMap id t) && acc) True) multiSubsts
    let substedSymbols = map (foldr (\(id, t) acc -> abstractSubstitute (env ^. boundTypeVars) id t acc) sk) validSubsts
    -- writeLog 3 $ text id <+> text "::" <+> pretty sk
    st <- get
    foldrM (\t accMap -> do
        newId <- newSymbolName id
        return $ Map.insert newId (cutoff (st ^. abstractionLevel) "" t) accMap) Map.empty substedSymbols
    where
        outerName (ADatatypeT id _) = Left id
        outerName (ATypeVarT id) = Right Set.empty
        outerName (AExclusion names) = Right names

        multiPermutation len elmts | len == 0 = []
        multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
        multiPermutation len elmts            = nubOrd $ [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]
        
        unifierValid constraintMap id t = True
            -- let constraints = map (flip (Map.findWithDefault Set.empty) (env ^. typeClasses)) $ (Map.findWithDefault [] id constraintMap)
            -- in case outerName t of
            --     Left names -> foldr ((&&) . not . Set.null . Set.intersection names) True constraints -- containts id
            --     Right names -> foldr ((&&) . not . Set.null . flip Set.difference names) True constraints -- not contains names

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

distinguish :: Environment -> Map Id (Set Id) -> Map Id SType -> Id -> SType -> SType -> Map Id (Set Id)
distinguish env level _ _ AnyT _ = level
distinguish env level _ _ _ AnyT = level
distinguish env level _ _ ErrorT _ = level
distinguish env level _ _ _ ErrorT = level
-- distinguish env level _ key (ScalarT (DatatypeT id _ _) _) (ScalarT (DatatypeT id' _ _) _) | id /= id' =
    -- Map.insertWith Set.union key (Set.fromList [id, id']) level
-- distinguish env level tass key (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' =
    -- distinguish' (key ++ "," ++ id) tass tArgs tArgs'
  -- where
    -- split the current node into two when we cannot distinguish the error type from its abstract representation
    -- distinguish' key _ [] [] = level -- error "distinguish error: these two types are exactly the same"
    -- distinguish' key _ [] ((ScalarT (TypeVarT _ id) _):_) | isBound env id = Map.insertWith Set.union key (Set.singleton id) level
    -- distinguish' key tass [] ((ScalarT (TypeVarT _ id) _):_) | id `Map.member` tass = distinguish' key tass [] [fromJust $ Map.lookup id tass]
    -- distinguish' key _ [] ((ScalarT (TypeVarT _ id) _):_) = level
    -- distinguish' key tass [] (t:_) = Map.insertWith Set.union key (Set.singleton $ scalarName (stypeSubstitute tass t)) level
    -- distinguish' key tass args [] = distinguish' key tass [] args
    -- distinguish' key tass (arg:args) (arg':args') =
        -- let level' = distinguish env level tass key arg arg'
        -- in if level' /= level then level' -- if we get several new representations, stop refining
                              -- else distinguish' key tass args args' -- otherwise append the current arg to the recursive result
-- TODO: need change to support higher order functions
distinguish env level tass key (ScalarT (TypeVarT _ id) _) (ScalarT (TypeVarT _ id') _) | id == id' = level
-- has unsubtituted type variables, substitute them and recheck
distinguish env level tass key (ScalarT (TypeVarT _ id) _) t | id `Map.member` tass =
    distinguish env level tass key (fromJust $ Map.lookup id tass) t
distinguish env level tass key t (ScalarT (TypeVarT _ id) _) | id `Map.member` tass =
    distinguish env level tass key t (fromJust $ Map.lookup id tass)
-- has unbounded type variable, view it as AnyT
distinguish env level _ key (ScalarT (TypeVarT _ id) _) t | not (isBound env id) = level
distinguish env level _ key t (ScalarT (TypeVarT _ id) _) | not (isBound env id) = level
-- has bounded type variable
distinguish env level tass key t tv@(ScalarT (TypeVarT _ _) _) =
    Map.insertWith Set.union key (Set.singleton $ scalarName (stypeSubstitute tass t)) level
distinguish env level tass key tv@(ScalarT (TypeVarT _ _) _) t =
    Map.insertWith Set.union key (Set.singleton $ scalarName (stypeSubstitute tass t)) level
distinguish env level tass key (FunctionT _ tArg tRes) (FunctionT _ tArg' tRes') =
    Map.unionWith Set.union (distinguish env level tass key tArg tArg')
                            (distinguish env level tass key tRes tRes')
distinguish env level tass key t1 t2 =
    let t1' = stypeSubstitute tass t1
        t2' = stypeSubstitute tass t2
    in if t1' == t2' then level
                     else Map.insertWith Set.union key (Set.fromList [scalarName t1', scalarName t2']) level

{-# SCC distinguish #-}

topDownCheck :: (MonadIO m) => Environment -> SType -> UProgram -> PNSolver m RProgram
topDownCheck env typ p@(Program (PSymbol sym) _) = do
    -- lookup the symbol type in current scope
    writeLog 3 $ text "Checking type for" <+> pretty p
    -- liftIO $ print (allSymbols env)
    t <- case lookupSymbol sym 0 env of
            Nothing  ->
                case lookupSymbol ("(" ++ sym ++ ")") 0 env of -- symbol name with parenthesis
                    Nothing  -> error $ "topDownCheck: cannot find symbol " ++ sym ++ " in the current environment"
                    Just sch -> fst <$> freshType sch
            Just sch -> fst <$> freshType sch
    -- solve the type constraint t == typ
    writeLog 3 $ text "Solving constraint" <+> pretty typ <+> text "==" <+> pretty t
    solveTypeConstraint env typ (shape t)
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

bottomUpCheck :: (MonadIO m) => Environment -> RProgram -> StateT TypingState m (Maybe (RProgram, ClassConstraint))
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
        Just  f -> do
            let FunctionT _ tArg tRet = typeOf f
            arg <- bottomUpCheck env pArg
            case arg of
                Nothing -> return Nothing
                Just (a, aConstraints) -> do
                    writeLog 3 $ text "Solving constraint for" <+> pretty a <+> pretty (shape $ typeOf a) <+> text "==" <+> pretty (shape tArg)
                    solveTypeConstraint env (shape $ typeOf a) (shape tArg) (Map.fromList (fConstraints ++ aConstraints))
                    tass <- view typeAssignment <$> get
                    curr <- view abstractionLevel <$> get
                    writeLog 3 $ text "Distinguishing" <+> pretty (shape $ typeOf a) <+> text "and" <+> pretty (shape tArg)
                    let argLevel = distinguish env curr tass "" (shape $ typeOf a) (shape tArg)
                    writeLog 3 $ text "Distinguishing" <+> pretty (shape typ) <+> text "and" <+> pretty (shape tRet)
                    -- writeLog 1 $ text "would distinguish" <+> (text (show $ distinguish env argLevel tass "" (shape typ) (shape tRet)))
                    -- TODO: the following line enables abstraction refinement
                    -- modify $ set abstractionLevel $ distinguish env argLevel tass "" (shape typ) (shape tRet)
                    ifM (view isChecked <$> get)
                        (return $ Just $ Program (PApp f a) tRet) -- Either type to store the conflict types
                        (return Nothing)
bottomUpCheck env p@(Program (PFun x body) (FunctionT _ tArg tRet)) = do
    writeLog 3 $ text "Checking type for" <+> pretty p
    body' <- bottomUpCheck (addVariable x (addTrue tArg) env) body
    case body' of
        Nothing -> return Nothing
        Just (b,c) -> return $ Just (Program (PFun x b) (FunctionT x tArg (typeOf b)), c)

solveTypeConstraint :: (MonadIO m) => Environment -> SType -> SType -> StateT TypingState m ()
solveTypeConstraint _ AnyT _ = return ()
solveTypeConstraint _ _ AnyT = return ()
solveTypeConstraint _ ErrorT _ = return ()
solveTypeConstraint _ _ ErrorT = return ()
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
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = modify $ set isChecked False
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            writeLog 3 $ text "Solving constraint" <+> pretty typ <+> "==" <+> pretty t
            solveTypeConstraint env typ t
        else do
            modify $ over typeAssignment (Map.insert id t)
solveTypeConstraint env t tv@(ScalarT (TypeVarT _ id) _) = solveTypeConstraint env tv t
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    writeLog 3 $ text "Solving constraint" <+> pretty tArg <+> "==" <+> pretty tArg'
    solveTypeConstraint env tArg tArg'
    st <- get
    when (st ^. isChecked) (do
        writeLog 3 $ text "Solving constraint" <+> pretty tRet <+> "==" <+> pretty tRet'
        solveTypeConstraint env tRet tRet')
-- solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id /= id' = do
    -- modify $ set isChecked False
-- solveTypeConstraint env t1@(ScalarT (DatatypeT id tArgs _) _) t2@(ScalarT (DatatypeT id' tArgs' _) _) | id == id' = do
    -- solveTypeConstraint' env tArgs tArgs'
  -- where
    -- solveTypeConstraint' _ []  [] = return ()
    -- solveTypeConstraint' env (ty:tys) (ty':tys') = do
        -- writeLog 3 $ text "Solving constraint" <+> pretty ty <+> "==" <+> pretty ty'
        -- solveTypeConstraint env ty ty'
        -- checked <- view isChecked <$> get
        -- if the checking between ty and ty' succeeds, proceed to others
        when (checked) (solveTypeConstraint' env tys tys')
solveTypeConstraint env t1 t2 _ = error $ "unknown types " ++ show t1 ++ " or " ++ show t2

unify :: (MonadIO m) => Environment -> Id -> SType -> Map Id [Id] -> PNSolver m ()
-- unify env v t@(ScalarT (DatatypeT name _ _) _) constraints | v `Map.member` constraints = 
--     if foldr ((&&) . Set.member name) True instances -- containts id and it has instance definition for each type classes constraints
--         then modify $ over typeAssignment (Map.insert v t)
--         else do
--             writeLog 3 $ text "Unify fails:" <+> pretty t <+> text "is not in" <+> pretty (Set.toList firstUnsat)
--             -- get first unsatisfied type class, and distinguish this type out of all the other defined instances
--             modify $ set isChecked False
--             modify $ over abstractionSemantic (Map.insertWith Set.union "" (Set.singleton $ Right firstUnsat))
--   where
--     classes = Map.findWithDefault [] v constraints
--     getInstances c = Map.findWithDefault Set.empty c (env ^. typeClasses)
--     instances = map getInstances classes
--     firstUnsat = minimum $ filter (not . Set.member name) instances
-- unify env v t@(ScalarT (TypeVarT _ name) _) constraints | isBound env name = 
--   if foldr ((&&) . Set.member name) True instances -- containts id and it has instance definition for each type classes constraints
--         then modify $ over typeAssignment (Map.insert v t)
--         else do-- get first unsatisfied type class, and distinguish this type out of all the other defined instances
--             writeLog 3 $ text "Unify fails:" <+> pretty t <+> text "is not in" <+> pretty (Set.toList firstUnsat)
--             modify $ set isChecked False
--             modify $ over abstractionSemantic (Map.insertWith Set.union "" (Set.singleton $ Right firstUnsat))
--  where
--     classes = Map.findWithDefault [] v constraints
--     getInstances c = Map.findWithDefault Set.empty c (env ^. typeClasses)
--     instances = map getInstances classes
--     firstUnsat = minimum $ filter (not . Set.member name) instances
unify env v t _ = modify $ over typeAssignment (Map.insert v t)

checkType :: (MonadIO m) => Environment -> RType -> UProgram -> PNSolver m (Maybe RProgram)
checkType env typ p = do
    writeLog 1 $ text "Find program" <+> pretty p
    modify $ set isChecked True
    top <- topDownCheck env (shape typ) p
    ifM (view isChecked <$> get)
        (return $ Just top)
        (do
            writeLog 3 $ text "Top down type checking get" <+> pretty top
            modify $ set isChecked True
            bottomUpCheck env top
            return Nothing)

parseTypeClass :: Environment -> IO Environment
parseTypeClass env = do
    content <- readFile "typeClass.json"
    case Aeson.eitherDecode (LB8.pack content) of
        Left err -> error err
        Right v  -> case v of
            Aeson.Object contents -> do
                let classMap = HashMap.foldrWithKey (\k v -> Map.insert (Text.unpack k) (map getString $ getArray v)) Map.empty contents
                let transClasses = Map.foldrWithKey insertSet Map.empty classMap
                return $ set typeClasses transClasses env
            _  -> error "parseTypeClass: unknown error when decoding the JSON string"
  where
    getArray (Aeson.Array arr) = Vector.toList arr
    getString (Aeson.String str) = Text.unpack str
    insertSet t cs m = foldr (flip (Map.insertWith Set.union) (Set.singleton t)) m cs

findPath :: (MonadIO m) => Environment -> RType -> PNSolver m ()
findPath env dst = do
    writeLog 3 $ text "Start looking for a new path"
    -- writeLog 3 $ text "Current symbols:" <+> pretty (Map.toList $ allSymbols env)
    st <- get
    -- liftIO $ print $ env' ^. typeClasses
    -- writeLog 1 $ text "Current abstraction level" <+> pretty (Map.toList $ Map.map Set.toList $ st ^. abstractionLevel)
    -- abstract all the symbols in the current environment
    start <- liftIO $ getCurrentTime
    freshSymbols <- mapM (\(id, sch) -> do (t,c) <- freshType sch; return (id, t, c)) $ Map.toList $ allSymbols env
    let absSymbols = foldr (\(id, t, c) -> Map.insert id (abstract (env ^. boundTypeVars) (st ^. abstractionLevel) ""
                                                          $ shape t, c)) Map.empty freshSymbols
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
    writeLog 1 $ text "Time for preparing data" <+> text (show $ diffUTCTime end start)
    -- undefined
    liftIO $ [java| {
        java.util.List<java.lang.String> srcTypes = java.util.Arrays.asList($srcTypes);
        java.util.List<java.lang.String> argNames = java.util.Arrays.asList($argNames);
        java.util.List<java.lang.String> solutions = java.util.Arrays.asList($excludeLists);
        String tgtType = $tgt;
        //System.out.println("Arguments:" + srcTypes.toString());
        //System.out.println("Target:" + tgtType);
        cmu.edu.utils.SynquidUtil.init(srcTypes, argNames, tgtType, $symbols, solutions, $loc);
    } |]

findProgram :: (MonadIO m) => Environment -> RType -> PNSolver m RProgram
findProgram env dst = do
    -- findPath env dst
    code <- liftIO $ [java| {
        cmu.edu.utils.SynquidUtil.buildNextEncoding();
        //java.util.List<java.lang.String> res = cmu.edu.utils.SynquidUtil.synthesize();
        java.lang.String res = cmu.edu.utils.SynquidUtil.synthesize();
        return res;
    } |]
    codeResult <- liftIO $ map Text.unpack <$> reify code
    checkResult <- mapM parseAndCheck codeResult
    let codes = catMaybes checkResult
    solutions <- view currentSolutions <$> get
    if (null codes) || (head codes `elem` solutions)
        then do
            -- modify $ set currentSigs Map.empty
            findProgram env dst 
        else do
            liftIO $ putStrLn "*******************SOLUTION*********************"
            liftIO $ print p
            liftIO $ putStrLn "************************************************"
            return p -- return the first correct program we find
        else do
            modify $ over currentSolutions ((:) code)
            modify $ set currentSigs Map.empty
            modify $ set nameCounter Map.empty
            modify $ set typeAssignment Map.empty
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
