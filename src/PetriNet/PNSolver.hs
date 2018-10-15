{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}

module PetriNet.PNSolver where

import Data.String (fromString)
import Foreign.JNI (withJVM)
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
import Data.List.Extra
import Database.Generate
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8

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

data InstantiateState = InstantiateState {
    _functionIdx :: Map Id Int
} deriving(Eq, Ord, Show)

makeLenses ''InstantiateState

-- initSigs :: Environment -> Map Id AbstractSkeleton
-- initSigs env = Map.map (abstract . shape . toMonotype) $ allSymbols env -- get all symbols in the environment


-- TODO: start with only the datatypes in the query
instantiate :: (MonadIO m) => Map Id AbstractSkeleton -> StateT InstantiateState m (Map Id AbstractSkeleton, AbstractSkeleton)
instantiate sigs = instantiate' sigs $ Map.filter (not . hasAbstractVar) sigs
    where 
        removeSuffix id ty = (removeLast '_' id, ty)
        instantiate' sigs sigsAcc = do
            let typs = Set.fromList $ concatMap allAbstractBase sigs
            -- liftIO $ print ("sigs", sigs)
            sigs' <- foldM (\acc -> (<$>) (Map.union acc) . uncurry (instantiateWith $ Set.toList typs)) Map.empty (Map.toList sigs)
            -- iteratively compute for new added types
            let typs' = Set.fromList $ concatMap allAbstractBase sigs'
            -- liftIO $ print ("typs'", typs')
            if typs' /= typs
                then instantiate' sigs $ Map.fromList $ nubOn (uncurry removeSuffix) $ Map.toList $ Map.union sigsAcc sigs'
                else do
                    let currSigs = Map.fromList $ nubOn (uncurry removeSuffix) $ Map.toList $ Map.union sigsAcc sigs'
                    let allDts = foldr (\(ADatatypeT id _) acc -> id `Set.insert` acc) Set.empty $ Set.fromList $ concatMap allAbstractBase $ Map.elems currSigs
                    exSigs <- foldM (\acc -> (<$>) (Map.union acc) . uncurry (instantiateWith [AExclusion allDts])) Map.empty (Map.toList sigs)
                    return $ (Map.fromList $ nubOn (uncurry removeSuffix) $ Map.toList $ Map.union currSigs exSigs, AExclusion allDts)

instantiateWith :: (MonadIO m) => [AbstractSkeleton] -> Id -> AbstractSkeleton -> StateT InstantiateState m (Map Id AbstractSkeleton)
instantiateWith typs id sk = do
    let vars = allAbstractVar sk
    let multiSubsts    = map (zip vars) $ multiPermutation (length vars) typs
    let substedSymbols = map (foldr (\(id, t) acc -> abstractSubstitute id t acc) sk) multiSubsts
    -- liftIO $ print substedSymbols
    foldrM (\t accMap -> do
        newId <- newSymbolName id
        return $ Map.insert newId t accMap) Map.empty substedSymbols
    where
        multiPermutation len elmts | len == 0 = []
        multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
        multiPermutation len elmts            = nub $ [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]
        newSymbolName prefix = do
            indices <- flip (^.) functionIdx <$> get
            let idx = Map.findWithDefault 0 prefix indices
            modify (over functionIdx $ Map.insert prefix (idx+1))
            return $ prefix ++ "_" ++ show idx

data TypingState = TypingState {
    _nameCounter :: Map Id Int,  -- name map for generating fresh names (type variables, parameters)
    _typeAssignment :: Map Id SType,  -- current type assignment for each type variable
    _typingError :: (SType, SType), -- typing error message, represented by the expected type and actual type
    _isChecked :: Bool -- is the current state check passed
}

emptyTypingState = TypingState {
    _nameCounter = Map.empty,
    _typeAssignment = Map.empty,
    _typingError = (AnyT, AnyT),
    _isChecked = True
}

makeLenses ''TypingState

freshId :: (MonadIO m) => Id -> StateT TypingState m Id
freshId prefix = do
    indices <- flip (^.) nameCounter <$> get
    let idx = Map.findWithDefault 0 prefix indices
    modify (over nameCounter $ Map.insert prefix (idx+1))
    return $ prefix ++ "_" ++ show idx

-- | Replace all bound type variables with fresh free variables
freshType :: (MonadIO m) => RSchema -> StateT TypingState m RType
freshType sch = do
  t <- freshType' Map.empty sch
  return t
  where
    freshType' subst (ForallT a sch) = do
      a' <- freshId "A"
      freshType' (Map.insert a (vart a' ftrue) subst) sch    
    freshType' subst (Monotype t) = return $ typeSubstitute subst $ t

checkProgramType :: (MonadIO m) => Environment -> SType -> UProgram -> StateT TypingState m (Maybe TProgram)
checkProgramType env typ (Program (PSymbol sym) _) = do
    -- lookup the symbol type in current scope
    t <- case lookupSymbol (map (\c -> if c == '_' then '.' else c) sym) 0 env of
            Nothing  -> error $ "checkProgramType: cannot find symbol " ++ sym ++ " in the current environment"
            Just sch -> shape <$> freshType sch
    -- solve the type constraint t == typ
    liftIO $ putStrLn $ "Solving constraint " ++ show typ ++ " == " ++ show t
    solveTypeConstraint env typ t
    st <- get
    return $ if st ^. isChecked then Just (Program (PSymbol sym) t) else Nothing
checkProgramType env typ (Program (PApp pFun pArg) _) = do
    -- first check the function type with @AnyT@ as parameters and @typ@ as returns
    liftIO $ putStrLn $ "Checking type for " ++ show pFun
    fun <- checkProgramType env (FunctionT "x" AnyT typ) pFun
    case fun of
        Nothing -> return Nothing -- if the check fails, stop here and return error state
        Just f  -> do -- otherwise continue check for the argument type
            let FunctionT _ tArg tRet = typeOf f
            arg <- checkProgramType env tArg pArg
            case arg of
                Nothing -> return Nothing
                Just a  -> return $ Just (Program (PApp f a) tRet)
-- peel until we get a E-term
checkProgramType env typ (Program (PFun x body) _) = checkProgramType env typ body

solveTypeConstraint :: (MonadIO m) => Environment -> SType -> SType -> StateT TypingState m ()
solveTypeConstraint _ AnyT _ = modify $ set isChecked True
solveTypeConstraint _ _ AnyT = modify $ set isChecked True
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) tv'@(ScalarT (TypeVarT _ id') _) | isBound env id = do
    st <- get
    if id' `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id' $ st ^. typeAssignment
            liftIO $ putStrLn $ "Solving constraint " ++ show typ ++ " == " ++ show tv
            solveTypeConstraint env tv typ
        else do
            modify $ set isChecked True
            modify $ over typeAssignment (Map.insert id' tv)
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t | isBound env id = do
    modify $ set isChecked False
    modify $ set typingError (tv, t)
solveTypeConstraint env tv@(ScalarT (TypeVarT _ id) _) t = do
    st <- get
    if id `Map.member` (st ^. typeAssignment)
        then do
            let typ = fromJust $ Map.lookup id $ st ^. typeAssignment
            liftIO $ putStrLn $ "Solving constraint " ++ show tv ++ " == " ++ show typ
            solveTypeConstraint env typ t
        else do
            modify $ set isChecked False
            modify $ set typingError (tv, t)
solveTypeConstraint env (FunctionT _ tArg tRet) (FunctionT _ tArg' tRet') = do
    liftIO $ putStrLn $ "Solving constraint " ++ show tArg ++ " == " ++ show tArg'
    solveTypeConstraint env tArg tArg'
    st <- get
    when (st ^. isChecked) (do
        liftIO $ putStrLn $ "Solving constraint " ++ show tRet ++ " == " ++ show tRet'
        solveTypeConstraint env tRet tRet')
solveTypeConstraint _ (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id /= id' = do
    modify $ set isChecked False
    modify $ set typingError (ScalarT (DatatypeT id [] []) (), ScalarT (DatatypeT id' [] []) ())
solveTypeConstraint env (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' = do
    (expected, actual) <- solveTypeConstraint' env tArgs tArgs'
    modify $ set typingError (ScalarT (DatatypeT id expected []) (), ScalarT (DatatypeT id' actual []) ())
  where
    solveTypeConstraint' _ []  [] = do
        modify $ set isChecked True
        return ([],[])
    solveTypeConstraint' env (ty:tys) (ty':tys') = do
        liftIO $ putStrLn $ "Solving constraint " ++ show ty ++ " == " ++ show ty'
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
                let (expected, actual) = st ^. typingError
                return ([expected], [actual])

checkType :: Environment -> RType -> Declaration -> IO ()
checkType env typ (Pos _ (SynthesisGoal id p)) = do
    -- liftIO $ print $ env ^. boundTypeVars
    -- print id
    putStrLn $ "Find program"
    print p
    (p', st) <- runStateT (checkProgramType env (shape typ) p) emptyTypingState
    -- print p'
    print $ st ^. typingError
    -- print $ st ^. typeAssignment

-- compare whether the existing datatype is distinguishable from the error type we found
-- if not, split that type and reinstantiate all the polymorphic functions
-- keep the abstracted argument types and return types!!
refineAbstraction :: Environment -> SType -> IO ()
refineAbstraction env errTy = undefined

findPath :: Environment -> [RType] -> [Id] -> RType -> AbstractSkeleton -> IO ()
findPath env src args dst ex = do
    withJVM [ fromString ("-Djava.class.path=src/sypet/sypet.jar:"                 ++ 
                                            "src/sypet/lib/sat4j-pb.jar:"          ++
                                            "src/sypet/lib/commons-lang3-3.4.jar:" ++
                                            "src/sypet/lib/gson-2.8.5.jar:"        ++
                                            "src/sypet/lib/apt.jar")
            ] $ do
        symbols <- reflect (Text.pack . LB8.unpack . Aeson.encode $ map (uncurry encodeFunction) $ Map.toList (env ^. abstractSymbols))
        tgt <- reflect (Text.pack $ show $ abstract (Just ex) $ shape dst)
        srcTypes <- reflect (map (Text.pack . show . abstract (Just ex) . shape) src)
        argNames <- reflect (map (Text.pack) args)
        [java| {
            java.util.List<java.lang.String> srcTypes = java.util.Arrays.asList($srcTypes);
            java.util.List<java.lang.String> argNames = java.util.Arrays.asList($argNames);
            String tgtType = $tgt;
            // System.out.println(srcTypes.toString());
            // System.out.println(tgtType);
            cmu.edu.utils.SynquidUtil.init(srcTypes, argNames, tgtType, $symbols);
            cmu.edu.utils.SynquidUtil.buildNextEncoding();
        } |] :: IO ()
        code <- [java| {
            java.util.List<java.lang.String> res = cmu.edu.utils.SynquidUtil.synthesize();
            return res.toArray(new String[res.size()]);
        } |]
        codeTexts <- reify code
        -- liftIO $ Text.putStrLn codeText
        let codeCheck = map (flip evalState (initialPos "goal") . runIndentParserT parseProgram () "" . Text.unpack) codeTexts
        mapM_ (\cc -> case cc of
                        Left err   -> error "parse error"
                        Right decl -> liftIO $ checkType env dst $ head decl) codeCheck