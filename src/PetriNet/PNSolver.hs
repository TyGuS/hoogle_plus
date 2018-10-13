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
import Data.List.Extra
import Database.Generate
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos

import Synquid.Parser (parseFromFile, parseProgram, toErrorMessage)
import Synquid.Program
import Synquid.Type
import Synquid.Util
import Synquid.Error
import Synquid.Pretty
import PetriNet.PolyDispatcher

data AbstractSkeleton = 
      ADatatypeT Id [AbastractSkeleton] -- explicit datatypes
    | AExclusion [Id] -- not included datatypes
    | ATypeVarT Id -- type variable is only temporarily before building the PetriNet
    | AFunctionT AbstractSkeleton AbstractSkeleton
    deriving (Eq, Ord, Show, Generic)

data InstantiateState = InstantiateState {
    _functionIdx :: Map Id Int
} deriving(Eq, Ord, Show)

makeLenses ''InstantiateState

abstract :: SType -> AbstractSkeleton
abstract (ScalarT (DatatypeT id _ _) _) = ADatatypeT id []
abstract (ScalarT BoolT _)              = ADatatypeT "Bool" []
abstract (ScalarT IntT _)               = ADatatypeT "Int" []
abstract (ScalarT (TypeVarT _ id) _)    = ATypeVarT id
abstract (FunctionT x tArg tRet)        = AFunctionT (abstract tArg) (abstract tRet)

allAbstractBase :: AbstractSkeleton -> [AbstractSkeleton]
allAbstractBase t@(ADatatypeT _ _)     = [t]
allAbstractBase (AFunctionT tArg tRet) = allAbstractBase tArg ++ allAbstractBase tRet
allAbstractBase (ATypeVarT _)          = []
allAbstractBase t@(AExclusion _)       = [t]

allAbstractVar :: AbstractSkeleton -> [Id]
allAbstractVar (ATypeVarT id)         = [id]
allAbstractVar (ADatatypeT _ _)       = []
allAbstractVar (AFunctionT tArg tRet) = allAbstractVar tArg ++ allAbstractVar tRet
allAbstractVar (AExclusion _)         = []

hasAbstractVar :: AbstractSkeleton -> Bool
hasAbstractVar (ATypeVarT id)         = True
hasAbstractVar (ADatatypeT _ _)       = False
hasAbstractVar (AExclusion _)         = False
hasAbstractVar (AFunctionT tArg tRet) = hasAbstractVar tArg || hasAbstractVar tRet

abstractSubstitute :: Id -> AbstractBase -> AbstractSkeleton -> AbstractSkeleton
abstractSubstitute id bt t@(ADatatypeT _ _)     = t
abstractSubstitute id bt t@(AExclusion _)       = t
abstractSubstitute id bt t@(ATypeVarT var)      = if id == var then AScalarT bt else t
abstractSubstitute id bt (AFunctionT tArg tRet) = AFunctionT (abstractSubstitute id bt tArg) (abstractSubstitute id bt tRet)

abstractParamList :: AbstractSkeleton -> [Param]
abstractParamList t@(ADatatypeT _)                            = [show t]
abstractParamList t@(AExclusion _)                            = ["~" ++ show t]
abstractParamList (AFunctionT tArg tFun) | AScalarT _ <- tFun = [show tArg]
abstractParamList (AFunctionT tArg tFun)                      = (show tArg) : (abstractParamList tFun)
abstractParamList t                                           = error $ "Unexpected type " ++ show t

lastAbstractType :: AbstractSkeleton -> AbstractSkeleton
lastAbstractType (AFunctionT tArg tFun) = lastAbstractType tFun
lastAbstractType t                      = t

encodeFunction :: Id -> AbstractSkeleton -> Maybe FunctionCode
encodeFunction id t@(ADatatypeT _ _)       = Just $ FunctionCode id [] (show t)
encodeFunction id t@(AExclusion tys)       = Just $ FunctionCode id [] ("~" ++ show tys)
encodeFunction id   (ATypeVarT  _)         = error "Cannot encode a variable"
encodeFunction id t@(AFunctionT tArg tRet) = Just $ FunctionCode id (abstractParamList t) (show $ lastAbstractType t)

initSigs :: Environment -> Map Id AbstractSkeleton
initSigs env = Map.map (abstract . shape . toMonotype) $ allSymbols env -- get all symbols in the environment

instantiate :: (MonadIO m) => Map Id AbstractSkeleton -> StateT InstantiateState m (Map Id AbstractSkeleton)
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
                else return $ Map.fromList $ nubOn (uncurry removeSuffix) $ Map.toList $ Map.union sigsAcc sigs'

instantiateWith :: (MonadIO m) => [AbstractBase] -> Id -> AbstractSkeleton -> StateT InstantiateState m (Map Id AbstractSkeleton)
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

findPath :: Environment -> [String] -> String -> IO ()
findPath env src dst = do
    withJVM [ fromString ("-Djava.class.path=src/sypet/sypet.jar:"                 ++ 
                                            "src/sypet/lib/sat4j-pb.jar:"          ++
                                            "src/sypet/lib/commons-lang3-3.4.jar:" ++
                                            "src/sypet/lib/gson-2.8.5.jar:"        ++
                                            "src/sypet/lib/apt.jar")
            ] $ do
        tgt <- reflect (Text.pack dst)
        srcTypes <- reflect (map Text.pack src)  
        [java| {
            java.util.List<java.lang.String> srcTypes = java.util.Arrays.asList($srcTypes);;
            String tgtType = $tgt;
            cmu.edu.utils.SynquidUtil.init(srcTypes, tgtType);
            cmu.edu.utils.SynquidUtil.buildNextEncoding();
        } |] :: IO ()
        code <- [java| {
            return cmu.edu.utils.SynquidUtil.synthesize();
        } |]
        codeText <- reify code
        let codeCheck = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" $ Text.unpack codeText
        case codeCheck of
            Left err   -> error "parse error"
            Right decl -> liftIO $ checkType env $ head decl

checkType :: Environment -> Declaration -> IO ()
checkType env (Pos _ (SynthesisGoal id p)) = do
    print id
    print p

data TypingState = TypingState {
    _nameCounter :: Map Id Int,  -- name map for generating fresh names (type variables, parameters)
    _typeAssignment :: Map Id SType  -- current type assignment for each type variable
    _typingError :: (AbstractSkeleton, AbstractSkeleton), -- typing error message, represented by the expected type and actual type
    _isChecked :: Bool -- is the current state check passed
}

makeLenses ''TypingState

checkProgramType :: (MonadIO m) => Environment -> SType -> TProgram -> StateT m TypingState (Either () ())
checkProgramType env typ (Program (PSymbol sym) _) = do
    -- lookup the symbol type in current scope
    let t = case lookupSymbol sym 0 env of
              Nothing  -> error $ "checkProgramType: cannot find symbol " ++ sym ++ " in the current environment"
              Just sch -> shape $ toMonotype sch
    -- solve the type constraint t <: typ
    solveTypeConstraint typ t
checkProgramType env typ (Program (PApp pFun pArg) _) = do
    -- first check the function type with @AnyT@ as parameters and @typ@ as returns
    checkProgramType env (FunctionT "x" AnyT typ) pFun
    -- if the check fails, stop here and return error state
    -- otherwise continue check for the argument types

solveTypeConstraint :: (MonadIO m) => SType -> SType -> StateT m TypingState ()
solveTypeConstraint (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id /= id' = do
    modify $ set isChecked False
    modify $ set typingError (ADatatypeT id [], ADatatypeT id' [])
solveTypeConstraint (ScalarT (DatatypeT id tArgs _) _) (ScalarT (DatatypeT id' tArgs' _) _) | id == id' = do
    solveTypeConstraint' tArgs tArgs'
    state <- get
    let (expected, actual) = state' ^. typingError
    modify $ set typingError (ADatatypeT id expected, ADatatypeT id' actual)
  where
    solveTypeConstraint' []  [] = do
        modify $ set isChecked True
        return ()
    solveTypeConstraint' (ty:tys) (ty':tys') = do
        solveTypeConstraint ty ty'
        state <- get
        -- if the checking between ty and ty' succeeds, proceed to others
        when (state ^. isChecked) $ do
            solveTypeConstraint' tys tys'
            state' <- get
            -- if the typing check fails, prepend current argument to the error message
            when (not $ state' ^. isChecked) $ do
                let (expected, actual) = state' ^. typingError
                case expected of
                    _:_ -> modify $ set typingError ((abstract ty): expected , (abstract ty'): actual )
                    _   -> modify $ set typingError ((abstract ty):[expected], (abstract ty'):[actual])