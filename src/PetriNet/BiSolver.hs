{-# LANGUAGE FlexibleContexts #-}

module PetriNet.BiSolver where

import Types.Type
import Types.Solver
import Types.Common
import Types.Program
import Types.Environment
import Types.Encoder
import Types.Experiments
import Types.Checker
import Synquid.Type
import Synquid.Util
import Synquid.Logic (ftrue)
import PetriNet.AbstractType
import HooglePlus.TypeChecker
import HooglePlus.CodeFormer
import Database.Convert
import Database.Util
import Types.Abstract
import PetriNet.PNEncoder
import PetriNet.Util hiding (writeLog)
import HooglePlus.Stats

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import Data.Ord
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Lens
import Language.Haskell.Exts.Parser (ParseResult(..), parseExp)
import Debug.Trace
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

data Direction = Forward | Backward deriving(Show)

selectComps :: MonadIO m => Environment -> BiSolver m ()
selectComps env = do
    components <- gets $ view allSymbols
    fset <- gets $ view forwardSet
    bset <- gets $ view backwardSet
    mapM_ (pickOne fset bset) (Map.toList components)
    modify $ over maxLength (+ 2)
    where
        -- we may try different pick strategies in the near future
        pickOne fset bset (cname, csch) = do
            -- if containing guarded arguments and they are all inhabited in fset
            ctype <- freshType csch
            let args = allArgTypes ctype
            let tvs = env ^. boundTypeVars
            let guardedArgs = filter (isGuarded tvs) $ map toFunDts args
            let constraints = mkConstraints guardedArgs (Set.toList fset)
            mapM_ (addInhabitant Forward cname ctype) constraints
            -- if returning a guarded type and it is inhabited in bset
            let res = lastType ctype
            let rets = if isGuarded tvs res then [res] else []
            let constraints = mkConstraints rets (Set.toList bset)
            mapM_ (addInhabitant Backward cname ctype) constraints

        mkConstraints [] _ = [[]]
        mkConstraints (arg:args) types = [ (arg, t):c | t <- types
                                         , c <- mkConstraints args types]


        mkNewTransition dir cname ctype state | isChecked state = do
            modify $ setNameIndices (getNameIndices state)
            let tass = typeAssignment state
            let ftype = typeSubstitute tass ctype
            writeLog "mkNewTransition" $ text (show dir)
            let boundTvs = env ^. boundTypeVars
            let freeVars = Set.toList (typeVarsOf ftype) \\ boundTvs
            symbols <- gets $ view selectedSymbols
            names <- gets $ view nameMap
            incompletes <- gets $ view incompleteSymbols
            if null freeVars && not (hasInstance boundTvs names symbols ftype cname)
                then do
                    fname <- freshId "f"
                    let types = map toFunDts (lastType ftype : allArgTypes ftype)
                    let places = decompose (toAbstractType ftype)
                    writeLog "mkNewTransition" $ pretty places
                    writeLog "mkNewTransition" $ text cname <+> text "==>" <+> text fname <+> text "::" <+> pretty ftype
                    modify $ over nameMap (Map.insert fname cname)
                    modify $ over selectedSymbols (Map.insert fname ftype)
                    modify $ over selectedTypes (Set.union $ Set.fromList types)
                    mapM_ (\p -> modify $ over petrinet (HashMap.insertWith Set.union p (Set.singleton fname))) places
                    updateSets dir ftype
                else
                    when (not (null freeVars) && not (hasInstance boundTvs names incompletes ftype cname)) $ do
                        fname <- freshId "f"
                        modify $ over nameMap (Map.insert fname cname)
                        modify $ over incompleteSymbols (Map.insert fname ftype)
        mkNewTransition _ _ _ _ = return ()

        updateSets Forward t = do
            let res = lastType t
            when (arity t > 0) (modify $ over forwardSet (Set.insert res))
        updateSets Backward t = do
            let args = Set.fromList $ map toFunDts $ allArgTypes t
            modify $ over backwardSet (Set.union args)
            when (arity t == 0) (modify $ over nullaryInhabitant (Set.insert t))

        addInhabitant dir cname ctype constraints = do
            names <- gets $ view nameMap
            indices <- gets getNameIndices
            let initialState = emptyCheckerState {
                checkerNameMapping = names,
                checkerNameCounter = indices }
            unless (null constraints) $ do
                finalState <- execStateT (mapM_ (uncurry $ solveTypeConstraint env) constraints) initialState
                mkNewTransition dir cname ctype finalState

searchPath :: MonadIO m
           => Environment
           -> EncodeState
           -> BiSolver m (Maybe RProgram, EncodeState)
searchPath env st = do
    len <- gets $ view maxLength
    (res, st') <- withTime SolverTime (liftIO (encoderSolve st))
    case res of
        [] | len <= loc st' -> return (Nothing, st')
           | otherwise -> withTime EncodingTime (incEncoder st') >>= searchPath env
        path  -> do
            let usefulTrans = filter skipClone path
            writeLog "searchPath" $ pretty path
            writeLog "searchPath" $ pretty usefulTrans
            candidates <- fillSketch usefulTrans
            res <- withTime TypeCheckTime $ firstCheckedOrError $
                sortOn (Data.Ord.Down . length) $ Set.toList candidates
            return (res, st')
    where
        skipClone = not . isInfixOf "|clone"

        incEncoder st = do
            funcs <- gets $ view selectedSymbols
            src <- gets $ view srcTypes
            rets <- gets $ view dstTypes
            types <- gets $ view selectedTypes
            clones <- mapM (addCloneFunction . toAbstractType) (Set.toList types)
            let src' = map toAbstractType src
            let dst' = map toAbstractType rets
            let funcs' = clones ++ map encode (Map.toList funcs)
            liftIO $ execStateT (encoderInc funcs' src' dst') st

        fillSketch firedTrans = do
            src <- gets $ view srcTypes
            rets <- gets $ view dstTypes
            names <- gets $ view nameMap
            funcs <- gets $ view selectedSymbols
            let src' = map toAbstractType src
            let dst' = map toAbstractType rets
            let args = Map.keys $ Map.filter (not . isFunctionType) (env ^. arguments)
            let sigs = map (\n -> encode (n, fromJust (Map.lookup n funcs))) firedTrans
            -- writeLog 2 "fillSketch" $ text "found path" <+> pretty firedTrans
            let initialFormer = FormerState HashMap.empty []
            withTime FormerTime $ liftIO $
                evalStateT (generateProgram sigs src' args dst' False) initialFormer

        firstCheckedOrError [] = return Nothing
        firstCheckedOrError (x:xs) =
            case parseAndCheck x of
                Left prog -> return $ Just prog
                Right err -> firstCheckedOrError xs

        parseAndCheck code = case parseExp code of
            ParseOk exp -> Left (toSynquidProgram exp)
            ParseFailed loc err -> Right err

searchProgram :: MonadIO m => Environment -> EncodeState -> BiSolver m ()
searchProgram env encoderSt = do
    mbProg <- searchPath env encoderSt
    names <- gets $ view nameMap
    case mbProg of
        (Just p, encoderSt) -> liftIO $ print (recoverNames names p)
        (Nothing, encoderSt) -> do
            types <- gets $ view selectedTypes
            symbols <- gets $ view selectedSymbols
            selectComps env
            types' <- gets $ view selectedTypes
            symbols' <- gets $ view selectedSymbols
            let newTypes = Set.toList (types' `Set.difference` types)
            let newFuncs = map encode $ Map.toList (symbols' `Map.difference` symbols)
            encoderSt' <- addEncoderSymbols env newTypes newFuncs $ encoderSt { loc = 1 }
            searchProgram env encoderSt'

addEncoderSymbols :: MonadIO m
                  => Environment
                  -> [RType]
                  -> [FunctionCode]
                  -> EncodeState
                  -> BiSolver m EncodeState
addEncoderSymbols env newTypes newFuncs st = do
    src <- gets $ view srcTypes
    dst <- gets $ view dstTypes
    names <- gets $ view nameMap
    incompletes <- gets $ view incompleteSymbols
    allTypes <- gets $ view selectedTypes
    let encoderSrc = map toAbstractType src
    let encoderDst = map toAbstractType dst
    let hoArgNames = Map.keys $ Map.filter isFunctionType (env ^. arguments)
    let hoFunNames = Map.filter (`elem` hoArgNames) names
    let reverseMap (k, v) = HashMap.insertWith (++) v [k]
    let musters = foldr reverseMap HashMap.empty (Map.toList hoFunNames)
    -- only add those inhabited types into the net
    -- if one type is not a source type, not a return type
    -- not a new type that appears both in the forward set and backward set
    -- remove it from the net
    inhabited <- filterM isInhabited newTypes
    let types' = map toAbstractType inhabited
    let uninhabited = map toAbstractType (newTypes \\ inhabited)
    cloneFuncs <- mapM addCloneFunction types'
    let selectedInhabited = (Set.toList allTypes \\ newTypes) ++ inhabited
    filledFvs <- mapM (uncurry $ fillFreeVar selectedInhabited) (Map.toList incompletes)
    t2tr <- gets $ view petrinet
    let allInhabited = not . null . intersect uninhabited . funParams
    let newFuncs' = filter allInhabited (newFuncs ++ concat filledFvs)
    let allFuncs = newFuncs' ++ cloneFuncs
    let info = SplitInfo types' [] (map funName allFuncs)
    let t2tr' = HashMap.filterWithKey (\k _ -> k `notElem` uninhabited) t2tr
    liftIO $ execStateT (encoderRefine info musters encoderSrc encoderDst allFuncs t2tr') st
    where
        isInhabited t = do
            fset <- gets $ view forwardSet
            bset <- gets $ view backwardSet
            src <- gets $ view srcTypes
            dst <- gets $ view dstTypes
            nullaries <- gets $ view nullaryInhabitant
            return $ t `elem` src ||
                     t `elem` dst ||
                    (t `Set.member` nullaries) ||
                    (t `Set.member` fset && t `Set.member` bset)

        fillFreeVar types sname sig = do
            names <- gets $ view nameMap
            selected <- gets $ view selectedSymbols
            let fvs = (Set.toList $ typeVarsOf sig) \\ env ^. boundTypeVars
            let cname = fromJust (Map.lookup sname names)
            let substs = map (Map.fromList . zip fvs) $ combsWithRep (length fvs) types
            let newSigs = map (`typeSubstitute` sig) substs
            let tvs = env ^. boundTypeVars
            let filteredSigs = filter (\s -> hasInstance tvs names selected s cname) newSigs
            mapM (\s -> do
                let places = decompose $ toAbstractType s
                fname <- freshId "f"
                modify $ over nameMap (Map.insert fname cname)
                modify $ over selectedSymbols (Map.insert fname s)
                mapM_ (\p -> modify $ over petrinet (HashMap.insertWith Set.union p $ Set.singleton fname)) places
                return $ encode (fname, s)
                ) filteredSigs

addCloneFunction :: MonadIO m => AbstractSkeleton -> BiSolver m FunctionCode
addCloneFunction ty = do
    let fname = show ty ++ "|clone"
    let fc = FunctionCode fname [] [ty] [ty, ty]
    modify $ over petrinet (HashMap.insertWith Set.union ty $ Set.singleton fname)
    return fc

runBiSolver :: Environment -> RType -> IO ()
runBiSolver env goal = do
    let fset = Set.fromList $ map toFunDts (allArgTypes goal)
    let src = filter (not . isFunctionType) (allArgTypes goal)
    let bset = Set.singleton $ lastType goal
    let dst = [lastType goal]
    let (hoArgs, foArgs) = Map.partition isFunctionType (env ^. arguments)
    let hoFuncs = map (\(id, t) -> (id ++ hoPostfix, Monotype $ toFunDts t)) (Map.toList hoArgs)
    let initialState = emptyBiSolver {
        _forwardSet = fset,
        _backwardSet = bset,
        _srcTypes = src,
        _dstTypes = dst,
        _allSymbols = (env ^. symbols) `Map.difference` foArgs `Map.union` (Map.fromList hoFuncs)
    }
    let src' = map toAbstractType src
    let dst' = map toAbstractType dst
    let petrinet' = HashMap.fromList $ zip (src' ++ dst') (repeat Set.empty)
    initialEncodeState <- encoderInit 1 HashMap.empty src' dst' [] petrinet' False False False
    evalStateT (searchProgram env initialEncodeState) initialState

--------------------------------------------------------------------------------
{- helper functions -}
--------------------------------------------------------------------------------
isGuarded tvs (ScalarT DatatypeT {} _) = True
isGuarded tvs (ScalarT (TypeVarT _ id) _) = id `elem` tvs
isGuarded tvs _ = False

toFunDts (FunctionT _ tArg tRes) = ScalarT (DatatypeT "Fun" [toFunDts tArg, toFunDts tRes] []) ftrue
toFunDts t = t

encode (id, t) = encodeFunction id (toAbstractType $ shape t)

writeLog tag msg = trace (printf "[%s]: %s\n" tag (show $ plain msg)) $ return ()

eqExceptFvs :: [Id] -> TypeSkeleton r -> TypeSkeleton r -> Bool
eqExceptFvs tvs (ScalarT (TypeVarT _ id1) _) (ScalarT (TypeVarT _ id2) _) =
    id1 == id2 || ((id1 `notElem` tvs) && (id2 `notElem` tvs))
eqExceptFvs tvs (ScalarT (DatatypeT id1 args1 _) _) (ScalarT (DatatypeT id2 args2 _) _) =
    id1 == id2 && all (uncurry $ eqExceptFvs tvs) (zip args1 args2)
eqExceptFvs tvs (FunctionT _ arg1 res1) (FunctionT _ arg2 res2) =
    eqExceptFvs tvs arg1 arg2 && eqExceptFvs tvs res1 res2
eqExceptFvs _ BotT BotT = True
eqExceptFvs _ AnyT AnyT = True
eqExceptFvs _ _ _ = False

-- Return the combinations, with replacement, of k items from the
-- list.  We ignore the case where k is greater than the length of
-- the list.
combsWithRep :: Int -> [a] -> [[a]]
combsWithRep 0 _ = [[]]
combsWithRep _ [] = []
combsWithRep k xxs@(x:xs) =
  (x :) <$> combsWithRep (k - 1) xxs ++ combsWithRep k xs

hasInstance :: [Id] -> Map Id Id -> Map Id RType -> RType -> Id -> Bool
hasInstance tvs nameMap typeMap t n = let
    sameSigs = Map.filter (eqExceptFvs tvs t) typeMap
    sigNames = Map.keys sameSigs
    funNames = map (fromJust . (`Map.lookup` nameMap)) sigNames
    in n `elem` funNames