module PetriNet.BiSolver where

import Types.Type
import Types.Solver
import Types.Common
import Types.Program
import Types.Environment
import Types.Encoder
import Types.Experiment
import Synquid.Type
import Synquid.Util
import Synquid.Logic (ftrue)
import PetriNet.AbstractType

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Control.Lens

selectComps :: MonadIO m => PNSolver m ()
selectComps = do
    components <- gets $ view allSymbols
    fset <- gets $ view forwardSet
    bset <- gets $ view backwardSet
    where
        -- we may try different pick strategies in the near future
        pickOne c fset bset =
            -- if containing guarded arguments and they are all inhabited in fset
            let args = allArgTypes c
            let guardedArgs = map toFunDts $ filter isGuarded args

            -- if returning a guarded type and it is inhabited in bset
            let res = lastType c

        isInhabited set constraints = do


searchPath :: MonadIO m
           => EncoderState
           -> PNSolver m (Maybe RProgram, EncoderState)
searchPath st = do
    len <- gets $ view maxLength
    (res, st') <- withTime SolverTime (liftIO (encoderSolve st))
    case res of
        [] | len <= loc st' -> return (Nothing, st')
           | otherwise -> withTime EncodingTime (incEncoder env st') >>= searchPath
        path  -> do
            let usefulTrans = filter skipClone path
            candidates <- fillSketch usefulTrans
            checkResult <- withTime TypeCheckTime $ firstCheckedOrError $
                sortOn (Data.Ord.Down . length) $ Set.toList codeResult
    where
        skipClone = not . isInfixOf "|clone"

        encode (id, t) = encodeFunction id (toAbstractType $ shape t)

        incEncoder st = do
            funcs <- gets $ view selectedSymbols
            src <- gets $ view srcTypes
            rets <- gets $ view dstTypes
            let funcs' = map encode (Map.toList funcs)
            liftIO $ execStateT (encoderInc funcs' src rets) st

        fillSketch firedTrans = do
            src <- gets $ view srcTypes
            rets <- gets $ view dstTypes
            nameMap <- gets $ view nameMapping
            funcs <- gets $ view selectedSymbols
            let args = Map.keys $ foArgsOf env
            let sigs = map encode (Map.toList funcs)
            writeLog 2 "fillSketch" $ text "found path" <+> pretty firedTrans
            let initialFormer = FormerState HashMap.empty []
            withTime FormerTime $ liftIO $
                evalStateT (generateProgram sigs src args rets False) initialFormer

        firstCheckedOrError [] = return Nothing
        firstCheckedOrError [x] = return $ Just (parseAndCheck x)
        firstCheckedOrError (x:xs) = do
            let res = parseAndCheck x
            case res of
                Left prog -> return $ Just res
                Right err -> firstCheckedOrError xs

        parseAndCheck code = case parseExp code of
            ParseOk exp -> toSynquidProgram exp
            ParseFailed loc err -> error err

searchProgram :: MonadIO m => Environment -> EncoderState -> PNSolver m ()
searchProgram env encoderSt = do
    mbProg <- searchPath encoderSt
    case mbProg of
        (Just p, encoderSt) -> print p
        (Nothing, encoderSt) -> do
            oldTypes <- gets $ view selectedTypes
            oldSymbols <- gets $ view selectedSymbols
            selectComps
            newTypes <- gets $ view selectedTypes
            newSymbols <- gets $ view selectedSymbols
            let newTypes = Set.toList (newTypes `Set.difference` oldTypes)
            let newFuncs = Map.keys (newSymbols `Map.difference` oldSymbols)
            encoderSt' <- addEncoderSymbols env newTypes newFuncs encoderSt
            searchProgram env encoderSt'

addEncoderSymbols :: MonadIO m
                  => Environment
                  -> [RType]
                  -> [Id]
                  -> EncoderState
                  -> PNSolver m EncoderState
addEncoderSymbols env newTypes newFuncs st = do
    src <- gets $ view srcTypes
    dst <- gets $ view dstTypes
    names <- gets $ view nameMap
    t2tr <- gets $ view petrinet
    let encoderSrc = map (toAbstractType . shape) src
    let encoderDst = map (toAbstractType . shape) dst
    let types' = map toAbstractType newTypes
    let info = SplitInfo types' newFuncs []
    let hoArgNames = Map.keys $ Map.filter isFunctionType (env ^. arguments)
    let hoFunNames = HashMap.filter (`elem` hoArgNames) names
    let reverseMap (k, v) = HashMap.insertWith (++) v [k]
    let musters = foldr reverseMap (HashMap.toList hoFunNames)
    liftIO $ execStateT (encoderRefine info musters encoderSrc encoderDst newFuncs t2tr) st


--------------------------------------------------------------------------------
{- helper functions -}
--------------------------------------------------------------------------------
isGuarded tvs (ScalarT (DatatypeT {}) _) = True
isGuarded tvs (ScalarT (TypeVarT id _) _) = id `elem` tvs
isGuarded tvs _ = False

toFunDts (FunctionT tArg tRes) = ScalarT (DatatypeT "Fun" [toFunDts tArg, toFunDts tRes] []) ftrue
toFunDts t = t