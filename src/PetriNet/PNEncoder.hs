{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PetriNet.PNEncoder(
     encoderInit
    , encoderSolve
    , encoderRefine
    , encoderInc
    ) where

import Data.Maybe
import Data.List
import Data.List.Extra
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Z3.Monad hiding(Z3Env, newEnv)
import qualified Z3.Base as Z3
import Control.Monad.State
import System.CPUTime
import Text.Printf
import Data.Text (pack, unpack, replace)

import Types.Common
import Types.Encoder
import Types.Abstract
import PetriNet.AbstractType
import Synquid.Util
import Synquid.Pretty

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . z3env)
    getContext = gets (envContext . z3env)
    getOptimize = gets (envOptimize . z3env)

defaultColor = 0

-- | create a new encoder in z3
createEncoder :: [Id] -> Id -> [FunctionCode] -> Encoder ()
createEncoder inputs ret sigs = do
    places <- gets ((:) "void" . Map.keys . ty2tr)
    transIds <- gets (nubOrd . concat . Map.elems . ty2tr)
    -- create all the type variables for encoding
    createVariables places transIds
    -- transitions <- mapM foldFunc sigs
    -- add all the constraints for the solver
    createConstraints places sigs
    -- set initial and final state for solver
    setInitialState inputs places
    setFinalState ret places

-- | set the initial state for the solver, where we have tokens only in void or inputs
-- the tokens in the other places should be zero
setInitialState :: [Id] -> [Id] -> Encoder ()
setInitialState inputs places = do
    let nonInputs = filter (\k -> notElem k inputs) places
    let inputCounts = map (\t -> (head t, length t)) (group (sort inputs))
    let typeCounts = inputCounts ++ (map (\t -> if t == "void" then ("void", 1) else (t, 0)) nonInputs)
    -- assign tokens to each types
    mapM_ (uncurry assignToken) typeCounts
  where
    assignToken p v = do
        placeMap <- place2variable <$> get
        tVar <- mkZ3IntVar $ findVariable (p, 0) placeMap
        mkIntNum v >>= mkEq tVar >>= assert

-- | set the final solver state, we allow only one token in the return type
-- and maybe several tokens in the "void" place
setFinalState :: Id -> [Id] -> Encoder ()
setFinalState ret places = do
    -- the return value should have only one token
    includeRet
    -- other places excluding void and ret should have nothing
    let nonOutputs = filter ((/=) ret) places
    mapM_ excludeOther nonOutputs
    mapM_ excludeColor places
  where
    includeRet = do
        placeMap <- place2variable <$> get
        l <- loc <$> get
        retVar <- mkZ3IntVar $ findVariable (ret, l) placeMap
        mkIntNum 1 >>= mkEq retVar >>= assert
    excludeOther p = do
        l <- loc <$> get
        placeMap <- place2variable <$> get
        when (p /= "void") $ do
            tVar <- mkZ3IntVar $ findVariable (p, l) placeMap
            mkIntNum 0 >>= mkEq tVar >>= assert
    excludeColor p = do
        l <- gets loc
        placeMap <- gets place2variable
        tVar <- mkZ3IntVar $ findVariable (p ++ "_colored", l) placeMap
        mkIntNum 0 >>= mkEq tVar >>= assert

solveAndGetModel :: Encoder [(Id, Int)]
solveAndGetModel = do
    prev <- prevChecked <$> get
    when (prev) (do
        toBlock <- block <$> get
        assert toBlock)
    -- res <- optimizeCheck
    res <- check
    l <- loc <$> get
        {- when (l == 4) (do
        solverStr <- solverToString
        liftIO $ putStrLn solverStr
        transMap <- transition2id <$> get
        liftIO $ print transMap
        placeMap <- place2variable <$> get
        liftIO $ print placeMap
        transMap' <- id2transition <$> get
        liftIO $ print transMap'
                  ) -}
    case res of
        Sat -> do
            model <- solverGetModel
            modelStr <- modelToString model
            selected <- mapM (checkLit model) [0..(l-1)]
            blockTrs <- mapM (uncurry blockTr) (zip [0..(l-1)] selected)
            blockAss <- mkAnd blockTrs >>= mkNot
            st <- get
            put $ st { block = blockAss }
            selectedNames <- getTrNames selected

            return (zip selectedNames [0,1..])
        Unsat -> do
            liftIO $ print "unsat"
            return []
        Undef -> do
            liftIO $ print "undef"
            return []
  where
    getTrNames selected = do
        transMap <- gets id2transition
        let transNames = map (\id -> findVariable (fromIntegral id) transMap) selected
        return transNames

    checkLit model t = do
        tsMap <- gets time2variable
        tsVar <- mkZ3IntVar (findVariable t tsMap)
        bMay <- evalInt model tsVar
        case bMay of
            Just b -> return b
            Nothing -> error $ "cannot eval the variable" ++ show t

    blockTr t tr = do
        tsMap <- gets time2variable
        tsVar <- mkZ3IntVar (findVariable t tsMap)
        mkIntNum tr >>= mkEq tsVar

    checkIntLit model (k, v) = do
        pVar <- mkZ3IntVar v
        iMay <- eval model pVar
        case iMay of
            Just i -> mkEq pVar i
            Nothing -> error $ "cannot eval the variable" ++ show k

encoderInit :: Int -> [Id] -> [Id] -> Id -> [FunctionCode] -> Map Id [Id] -> IO EncodeState
encoderInit loc hoArgs inputs ret sigs t2tr = do
    z3Env <- initialZ3Env
    false <- Z3.mkFalse (envContext z3Env)
    let initialState = EncodeState z3Env false loc 0 1 1 HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty hoArgs [] t2tr False
    execStateT (createEncoder inputs ret sigs) initialState

encoderSolve :: EncodeState -> IO ([(Id, Int)], EncodeState)
encoderSolve st = runStateT solveAndGetModel st

encoderInc :: [FunctionCode] -> [Id] -> Id -> Encoder ()
encoderInc sigs inputs ret = do
    pop 1

    st <- get
    put $ st { loc = loc st + 1 }
    places <- gets ((:) "void" . Map.keys . ty2tr)
    transitions <- gets (nubOrd . concat . Map.elems . ty2tr)
    l <- gets loc

    -- add new place, transition and timestamp variables
    -- TODO change addPlaceVar to include time stamp as parameters
    mapM_ addPlaceVar places
    addColorVar places
    addTimestampVar (l - 1)

    let allTrans = [(l - 1, tr) | tr <- sigs ]

    -- all places have non-negative number of tokens
    withTime "not negative" (nonnegativeTokens places)

    -- refine the postcondition constraints
    withTime "fire conditions" $ mapM_ (uncurry fireTransitions) allTrans

    -- save the current state and add changeable constraints
    push

    withTime "transition index range" transitionRng

    withTime "assign colors" $ (assignColors sigs [])

    withTime "noTransitionTokens" $ mapM_ (uncurry noTransitionTokens) [(t, p) | p <- places, t <- [0..(l-1)]]

    -- refine the must firers
    withTime "mustFireTransitions" mustFireTransitions

    -- set new initial and final state
    setInitialState inputs places
    setFinalState ret places

-- | wrap some action with time measuring and print out the execution time
withTime :: String -> Encoder a -> Encoder a
withTime desc f = do
    {-    start <- liftIO getCPUTime
    res <- f
    end <- liftIO getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    let time = printf "%s time: %0.3f sec\n" desc (diff :: Double)
    liftIO $ putStrLn time
    return res
    -}
    f

encoderRefine :: SplitInfo -> [FunctionCode] -> Map Id [Id] -> [Id] -> Id -> [Id] -> Encoder ()
encoderRefine info sigs t2tr inputs ret musters = do
    pop 1

    {- update the abstraction level -}
    st <- get
    put $ st { ty2tr = t2tr 
             , mustFirers = musters
             }

    {- operation on places -}
    let newPlaces = map show (concat (snd (unzip (splitedPlaces info))))
    let newHos = (Map.keys t2tr) \\ (Map.keys (ty2tr st))
    let newTransIds = concat (snd (unzip (newTrans info)))

    l <- loc <$> get

    -- add new place, transition and timestamp variables
    mapM_ addPlaceVar (newPlaces ++ newHos)
    addColorVar (nub (newPlaces ++ newHos))
    addTransitionVar newTransIds

    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- sigs ]

    -- all places have non-negative number of tokens
    withTime "not negative" (nonnegativeTokens (newPlaces ++ newHos))

    -- refine the postcondition constraints
    withTime "fire conditions" $ mapM_ (uncurry fireTransitions) allTrans

    -- disable splitted transitions
    withTime "disable transitions" $ disableTransitions (removedTrans info)

    -- save the current state and add changeable constraints
    push

    -- prepare hierarchical higher order places
    let hoPlaces = []
    let currPlaces = Map.keys t2tr

    withTime "transition index range" transitionRng

    withTime "assign colors" $ (assignColors sigs hoPlaces)

    withTime "noTransitionTokens" $ mapM_ (uncurry noTransitionTokens) [(t, p) | p <- currPlaces, t <- [0..(l-1)]]

    -- refine the must firers
    withTime "mustFireTransitions" mustFireTransitions

    -- set new initial and final state
    setInitialState inputs currPlaces
    setFinalState ret currPlaces

disableTransitions :: [Id] -> Encoder ()
disableTransitions trs = do
    l <- gets loc
    mapM_ (uncurry disableTrAt) [(t, tr) | t <- [0..(l-1)], tr <- trs]
  where
    disableTrAt t tr = do
        transMap <- gets transition2id
        tsMap <- gets time2variable
        trVar <- mkIntNum (findVariable tr transMap)
        tsVar <- mkZ3IntVar (findVariable t tsMap)
        mkEq tsVar trVar >>= mkNot >>= assert

-- | add variables for each place:
-- 1) an integer variable for number of default tokens
-- 2) an integer variable for number of colored tokens
-- 3) an integer variable for color index
-- in this design, we allow only one color existing in the petri net
-- so there is no nested higher order functions
addPlaceVar ::  Id -> Encoder ()
addPlaceVar p = do
    l <- gets loc
    mapM_ addPlaceVarAt [0..l]
  where
    addPlaceVarAt t = do
        st <- get
        let placeVar = Variable (variableNb st) (p ++ "_" ++ show t) t 0 VarPlace
        let coloredVar = Variable (variableNb st + 1) (p ++ "_c_" ++ show t) t 0 VarPlace
        let p2v = HashMap.insert (p, t) placeVar
                $ HashMap.insert (p ++ "_colored", t) coloredVar
                $ place2variable st
        when (not (HashMap.member (p, t) (place2variable st)))
             (put $ st { place2variable = p2v
                       , variableNb = variableNb st + 2
                       })

-- | add transition mapping from (tr, lv) to integer id
-- 1) an integer variable for each transition
-- 2) an fresh integer variable for each higher-order argument
addTransitionVar :: [Id] -> Encoder ()
addTransitionVar trs = mapM_ addTransitionVarFor trs
  where
    addTransitionVarFor tr = do
        st <- get
        let tid = transitionNb st
        when (not (HashMap.member tr (transition2id st)))
             (put $ st { transitionNb = 1 + transitionNb st
                       , transition2id = HashMap.insert tr tid (transition2id st)
                       , id2transition = HashMap.insert tid tr (id2transition st)
                       })

addTimestampVar :: Int -> Encoder ()
addTimestampVar t = do
    st <- get
    let tsVar = Variable (variableNb st) ("ts_" ++ show t) t 0 VarTimestamp
    when (not (HashMap.member t (time2variable st)))
         (put $ st { time2variable = HashMap.insert t tsVar (time2variable st)
                   , variableNb = variableNb st + 1
                   })

addColorVar :: [Id] -> Encoder ()
addColorVar places = do
    l <- gets loc
    mapM_ addColorVarAt [(p, t) | p <- places, t <- [0..l]]
  where
    addColorVarAt (p, t) = do
        st <- get
        let cvar = Variable (variableNb st) p t 0 VarPlace
        when (not (HashMap.member (p, t) (color2variable st)))
             (put $ st { color2variable = HashMap.insert (p, t) cvar (color2variable st)
                       , variableNb = variableNb st + 1
                       })

-- | map each place and transition to a variable in z3
createVariables :: [Id] -> [Id] -> Encoder ()
createVariables places transitions = do
    l <- gets loc
    -- add place variables
    mapM_ addPlaceVar places
    -- add transition mapping
    addTransitionVar transitions
    -- add timestamp variables
    mapM_ addTimestampVar [0..(l-1)]
    -- add color variables
    addColorVar places

createConstraints :: [Id] -> [FunctionCode] -> Encoder ()
createConstraints places transitions = do
    -- prepare constraint parameters
    liftIO $ print places
    l <- gets loc
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- transitions]
    let allPlaces = [(t, p) | t <- [0..(l-1)], p <- places]

    withTime "non negative:" (nonnegativeTokens places)

    withTime "fire conditions:" (mapM_ (uncurry fireTransitions) allTrans)

    push

    withTime "restrict transitions:" transitionRng

    withTime "no fire:" (mapM_ (uncurry noTransitionTokens) allPlaces)

    withTime "must fire:" mustFireTransitions

    withTime "assign colors:" (assignColors transitions [])

mkZ3BoolVar ::  Variable -> Encoder AST
mkZ3BoolVar var = do
    varSymbol <- mkIntSymbol (varId var)
    boolS <- mkBoolSort
    const <- mkConst varSymbol boolS
    return const

mkZ3IntVar :: Variable -> Encoder AST
mkZ3IntVar var = do
    varSymbol <- mkIntSymbol (varId var)
    intS <- mkIntSort
    const <- mkConst varSymbol intS
    return const

findVariable :: (Eq k, Hashable k, Show k) => k -> HashMap k v -> v
findVariable k m = case HashMap.lookup k m of
                        Just v -> v
                        Nothing -> error $ "cannot find variable for " ++ show k


nonnegativeTokens :: [Id] -> Encoder ()
nonnegativeTokens places = do
    l <- gets loc
    let colors = map (\n -> n ++ "_colored") places
    mapM_ (uncurry nonnegAt) [(p, t) | p <- (places ++ colors), t <- [0..l]]
  where
    nonnegAt p t = do
        placeMap <- gets place2variable
        let pVar = findVariable (p, t) placeMap
        pZ3Var <- mkZ3IntVar pVar
        zero <- mkIntNum 0
        mkGe pZ3Var zero >>= assert

-- | at each timestamp, only one transition can be fired, we restrict the
-- fired transition id range here
transitionRng :: Encoder ()
transitionRng = do
    l <- gets loc
    mapM_ fireAt [0..(l-1)]
  where
    fireAt t = do
        tsMap <- gets time2variable
        transMax <- gets transitionNb
        let tsVar = findVariable t tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        start <- mkIntNum 0
        mkGe tsZ3Var start >>= assert
        end <- mkIntNum transMax
        mkLt tsZ3Var end >>= assert

-- | if this place has no connected transition fired,
-- it has the same # of tokens
noTransitionTokens :: Int -> Id -> Encoder ()
noTransitionTokens t p = do
    trans <- gets transition2id
    t2tr <- gets ty2tr
    let transitions = map (\i -> findVariable i trans) (Map.findWithDefault [] p t2tr)
    noFireLvs <- noFireAt transitions p t
    noFire <- mkOr noFireLvs >>= mkNot
    placeMap <- gets place2variable
    curr <- mkZ3IntVar $ findVariable (p, t) placeMap
    next <- mkZ3IntVar $ findVariable (p, t + 1) placeMap
    tokenSame <- mkEq curr next
    currColor <- mkZ3IntVar $ findVariable (p ++ "_colored", t) placeMap
    nextColor <- mkZ3IntVar $ findVariable (p ++ "_colored", t + 1) placeMap
    coloredSame <- mkEq currColor nextColor
    colorMap <- gets color2variable
    currCidx <- mkZ3IntVar $ findVariable (p, t) colorMap
    nextCidx <- mkZ3IntVar $ findVariable (p, t + 1) colorMap
    cidxSame <- mkEq currCidx nextCidx
    mkAnd [tokenSame, coloredSame, cidxSame] >>= mkImplies noFire >>= assert
  where
    noFireAt transitions p t = do
        idVars <- mapM mkIntNum transitions
        tsMap <- gets time2variable
        let tsVar = findVariable t tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        mapM (mkEq tsZ3Var) idVars

assignColors :: [FunctionCode] -> [(AbstractSkeleton, [AbstractSkeleton])] -> Encoder ()
assignColors trs hops = do
    let hopMap = foldl' (\acc (t, ts) -> foldl' (\acc' tt -> HashMap.insert (show tt) (show t) acc') acc ts) HashMap.empty hops
    -- build a mapping from arg types to their higher order args
    paramMap <- foldM collectTypes HashMap.empty trs
    mapM_ (uncurry (assignInitialColor hopMap)) (HashMap.toList paramMap)
  where
    collectTypes m (FunctionCode _ [] _ _) = return m
    collectTypes m (FunctionCode _ hoParams _ _) = foldM assignParam m hoParams

    assignParam m (FunctionCode name _ params _) = do
        let m' = foldr (\p -> HashMap.insertWith union p [name]) m params
        return m'

    assignInitialColor hopMap param colors = do
        -- each higher-order type is able to produce colored tokens
        -- this is encoded in the initial state
        placeMap <- gets place2variable
        colorMap <- gets color2variable
        hoMap <- gets ho2id
        let paramToken = param ++ "_colored"
        zero <- mkIntNum 0
        colorIdxs <- mapM (\c -> mkIntNum (findVariable c hoMap)) colors
        paramInit <- mkZ3IntVar $ findVariable (paramToken, 0) placeMap
        colorInit <- mkZ3IntVar $ findVariable (param, 0) colorMap
        hasTokens <- mkGt paramInit zero
        correctColors <- mapM (mkEq colorInit) colorIdxs
        correctColor <- mkOr correctColors
        mkImplies hasTokens correctColor >>= assert

fireTransitions :: Int -> FunctionCode -> Encoder ()
fireTransitions t (FunctionCode name [] params rets) = do
    transMap <- gets transition2id
    placeMap <- gets place2variable
    colorMap <- gets color2variable
    tsMap <- gets time2variable
    -- accumulate counting for parameters and return types
    let tid = findVariable name transMap
    let pcnt = if null params then [("void", 1)] else map (\l -> (head l, length l)) (group (sort params))
    let pmap = Map.fromList pcnt
    let rmap = foldl' (\acc t -> Map.insertWith (+) t (-1) acc) pmap rets
    let rcnt = Map.toList rmap
    let tsVar = findVariable t tsMap
    tsZ3Var <- mkZ3IntVar tsVar
    trVar <- mkIntNum tid
    changes <- mapM (mkChange False t) rcnt
    -- all parameter tokens must have the same color
    -- if one place has the id of p, then p + 1 indicates its colorful token number
    -- p + 2 indicates its color index
    let places = fst (unzip pcnt)
    let foPlaces = filter (not . ("AFunctionT" `isInfixOf`)) places
    let colors = map (\p -> findVariable (p, t) colorMap) foPlaces
    let currResColors = map (\p -> findVariable (p, t) colorMap) rets
    let resColors = map (\p -> findVariable (p, t + 1) colorMap) rets
    cid:cids <- mapM mkZ3IntVar (nub (colors ++ resColors ++ currResColors))
    dcolor <- mkIntNum defaultColor
    sameColors <- mapM (mkEq cid) cids
    fire <- mkEq tsZ3Var trVar
    hasDefault <- mkEq cid dcolor
    enoughTokens <- mapM (getSatisfiedPlace False) pcnt
    defaultPost <- mkAnd (hasDefault : enoughTokens ++ changes ++ sameColors)
    if places == foPlaces
       then do
           colorChanges <- mapM (mkChange True t) rcnt
           notDefault <- mkGt cid dcolor
           enoughColors <- mapM (getSatisfiedPlace True) pcnt
           colorPost <- mkAnd (notDefault : enoughColors ++ colorChanges ++ sameColors)
           postCond <- mkOr [defaultPost, colorPost]
           mkImplies fire postCond >>= assert
       else do
           mkImplies fire defaultPost >>= assert
  where
    mkChange color t (p, diff) = do
        let d = if p == "void" then 0 else -diff
        let pc = if color then p ++ "_colored" else p
        let pu = if color then p else p ++ "_colored"
        placeMap <- gets place2variable
        before <- mkZ3IntVar $ findVariable (pc, t) placeMap
        after <- mkZ3IntVar $ findVariable (pc, t + 1) placeMap
        diffw <- mkIntNum d
        changed <- mkAdd [before, diffw] >>= mkEq after
        tBef <- mkZ3IntVar $ findVariable (pu, t) placeMap
        tAft <- mkZ3IntVar $ findVariable (pu, t + 1) placeMap
        unchanged <- mkEq tBef tAft
        mkAnd [changed, unchanged]

    getSatisfiedPlace color (p, cnt) = do
        w <- mkIntNum cnt
        placeMap <- gets place2variable
        let pc = if color then p ++ "_colored" else p
        pVar <- mkZ3IntVar (findVariable (pc, t) placeMap)
        mkGe pVar w
-- for higher order functions, on top of adding a new transition,
-- we assign a new id for each of the higher order argument
fireTransitions t (FunctionCode name hoParams params rets) = do
    mapM_ newColor hoParams
    fireTransitions t (FunctionCode name [] params rets)
  where
    initialHo p = do
        placeMap <- gets place2variable
        let pv = findVariable (p, 0) placeMap
        pZ3 <- mkZ3IntVar pv
        mkIntNum 0 >>= mkEq pZ3 >>= assert

    newColor hp = do
        colorCnt <- gets colorNb
        placeMap <- gets place2variable
        transMap <- gets transition2id
        hoMap <- gets ho2id
        colorMap <- gets color2variable
        tsMap <- gets time2variable
        let color = HashMap.lookupDefault colorCnt (funName hp) hoMap
        uncolorTr <- mkIntNum $ findVariable (funName hp ++ "|uncolor") transMap
        hoPlace <- mkZ3IntVar $ findVariable (funName hp, t) placeMap
        hoPlace' <- mkZ3IntVar $ findVariable (funName hp, t + 1) placeMap
        let tsVar = findVariable t tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        assignedColor <- mkIntNum color
        negUno <- mkIntNum (-1)
        uno <- mkIntNum 1

        -- each higher-order is able to consume returned colored tokens
        let retToken = head (funReturn hp) ++ "_colored"
        let retDef = head (funReturn hp)
        retCurrCidx <- mkZ3IntVar $ findVariable (retDef, t) colorMap
        retNextCidx <- mkZ3IntVar $ findVariable (retDef, t + 1) colorMap
        retCurr <- mkZ3IntVar $ findVariable (retDef, t) placeMap
        retNext <- mkZ3IntVar $ findVariable (retDef, t + 1) placeMap
        retTokenCurr <- mkZ3IntVar $ findVariable (retToken, t) placeMap
        retTokenNext <- mkZ3IntVar $ findVariable (retToken, t + 1) placeMap
        fireUncolor <- mkEq tsZ3Var uncolorTr
        correctColor <- mkEq retCurrCidx assignedColor
        enoughColor <- mkGe retTokenCurr uno
        hoChange <- mkAdd [hoPlace, uno] >>= mkEq hoPlace'
        retChange <- mkAdd [retTokenCurr, negUno] >>= mkEq retTokenNext
        hasDefault <- mkIntNum defaultColor >>= mkEq retNextCidx
        onlyOne <- mkEq retTokenCurr uno
        colorChange <- mkImplies onlyOne hasDefault
        moreThanOne <- mkGt retTokenCurr uno
        colorRemain <- mkEq retNextCidx retCurrCidx >>= mkImplies moreThanOne
        sameUncolor <- mkEq retCurr retNext
        postCond <- mkAnd [correctColor, enoughColor, hoChange, retChange, colorChange, colorRemain, sameUncolor]
        mkImplies fireUncolor postCond >>= assert

        -- update status
        st <- get
        let color' = if color /= (colorNb st) then colorNb st else color + 1
        put $ st { colorNb = color'
                 , ho2id = HashMap.insert (funName hp) color (ho2id st) 
                 }


mustFireTransitions ::  Encoder ()
mustFireTransitions = do
    must <- gets mustFirers
    transitions <- gets transition2id
    let mustTrans = HashMap.filterWithKey (\k _ -> nameInMust must k) transitions
    mapM_ fireTransition mustTrans
  where
    nameInMust must name = foldr ((||) . flip isInfixOf name) False must
    fireTransition tid = do
        l <- gets loc
        tsMap <- gets time2variable
        trId <- mkIntNum tid
        tsVars <- mapM (\t -> mkZ3IntVar (findVariable t tsMap)) [0..(l-1)]
        trVars <- mapM (mkEq trId) tsVars
        mkOr trVars >>= assert
