{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PetriNet.PNEncoder(
     encoderInit
    , encoderSolve
    , encoderRefine
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
import Synquid.Util

instance MonadZ3 Encoder where
    getSolver = gets (envSolver . z3env)
    getContext = gets (envContext . z3env)
    getOptimize = gets (envOptimize . z3env)

defaultColor = 0

-- | create a new encoder in z3
createEncoder :: [Id] -> Id -> [FunctionCode] -> Encoder ()
createEncoder inputs ret sigs = do
    places <- gets (Map.keys . ty2tr)
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
    let typeCounts = inputCounts ++ (map (\t -> (t, 0)) nonInputs)
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
    when (l == 4) (do
        solverStr <- solverToString
        liftIO $ putStrLn solverStr
        transMap <- transition2id <$> get
        liftIO $ print transMap
        placeMap <- place2variable <$> get
        liftIO $ print placeMap
        transMap' <- id2transition <$> get
        liftIO $ print transMap'
                  )
    case res of
        Sat -> do
            model <- solverGetModel
            modelStr <- modelToString model
            selected <- mapM (checkLit model) [0..(l-1)]
            blockTrs <- mapM (uncurry blockTr) (zip [0..(l-1)] selected)
            -- placeMap <- place2variable <$> get
            -- blockP <- mapM (checkIntLit model) $ HashMap.toList placeMap
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
        lv <- abstractionLv <$> get
        transMap <- id2transition <$> get
        let transNames = map (\id -> findVariable (fromIntegral id) transMap) selected
        let selectedTrs = map fst (filter ((==) lv . snd) transNames)
        return selectedTrs

    checkLit model t = do
        lv <- abstractionLv <$> get
        tsMap <- time2variable <$> get
        tsVar <- mkZ3IntVar (findVariable (t, lv) tsMap)
        bMay <- evalInt model tsVar
        case bMay of
            Just b -> return b
            Nothing -> error $ "cannot eval the variable" ++ show (t, lv)

    blockTr t tr = do
        lv <- abstractionLv <$> get
        tsMap <- time2variable <$> get
        tsVar <- mkZ3IntVar (findVariable (t, lv) tsMap)
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
    let initialState = EncodeState z3Env false loc 0 0 1 1 HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty hoArgs [] t2tr False
    execStateT (createEncoder inputs ret sigs) initialState

encoderSolve :: EncodeState -> IO ([(Id, Int)], EncodeState)
encoderSolve st = runStateT solveAndGetModel st

-- | wrap some action with time measuring and print out the execution time
withTime :: String -> Encoder a -> Encoder a
withTime desc f = do
    start <- liftIO getCPUTime
    res <- f
    end <- liftIO getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    let time = printf "%s time: %0.3f sec\n" desc (diff :: Double)
    liftIO $ putStrLn time
    return res

    -- f

encoderRefine :: SplitInfo -> [FunctionCode] -> Map Id [Id] -> [Id] -> Id -> Encoder ()
encoderRefine info sigs t2tr inputs ret = do
    {- update the abstraction level -}
    st <- get
    put $ st { abstractionLv = abstractionLv st + 1 
             , ty2tr = t2tr
             }

    {- operation on places -}
    let newPlaces = map show (concat (snd (unzip (splitedPlaces info))))
    let newHos = (Map.keys t2tr) \\ (Map.keys (ty2tr st))
    let typeClones = map (\(p, ps) -> (show p ++ "|clone", map (\n -> show n ++ "|clone") ps)) (splitedPlaces info)

    {- operation on transitions -}
    let transIds = concat (snd (unzip (splitedGroup info)))
    let allTransIds = fst (unzip (splitedGroup info)) ++ transIds ++ (fst (unzip typeClones)) ++ (concat (snd (unzip typeClones)))
    -- some of the transitions are splitted
    let existingTrans = findVariable (abstractionLv st) (transition2id st)
    let newTransId = concat (snd (unzip typeClones)) ++ transIds
    -- other transition have no change but need to be copied to the next level
    let noSplit k _ = not (elem k allTransIds)
    let oldTrans = HashMap.keys (HashMap.filterWithKey noSplit existingTrans)
    let splitTrans = typeClones ++ splitedGroup info

    l <- loc <$> get

    -- add new place, transition and timestamp variables
    mapM_ addPlaceVar (newPlaces ++ newHos)
    addTransitionVar (newTransId ++ oldTrans)
    mapM_ addTimestampVar [0..(l-1)]

    -- newTrans <- mapM foldFunc sigs
    let newTrans = sigs
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- newTrans ]

    -- refine the sequential constraints
    -- but enable transitions between abstraction levels fired simultaneously
    withTime "sequentialTransitions" sequentialTransitions

    withTime "not negative" (nonnegativeTokens (newPlaces ++ newHos))

    -- refine the postcondition constraints
    withTime "fire conditions" $ mapM_ (uncurry fireTransitions) allTrans

    withTime "assign colors" $ (assignColors newTrans)

    -- refine the no transition constraints
    let posMerge = map (\(p, ps) -> (show p, map show ps)) (splitedPlaces info)
    let toMerge = posMerge
    withTime "placeMerge" $ mapM_ (\t -> mapM_ (uncurry (placeMerge t)) toMerge) [0..l]
    withTime "transMerge" $ mapM_ (uncurry transMerge) [ (tr, t) | tr <- splitTrans, t <- [0..(l-1)] ]
    withTime "transKeep" $ mapM_ (uncurry transClone) [ (tr, t) | tr <- oldTrans, t <- [0..(l-1)] ]
    withTime "noTransitionTokens" $ mapM_ (uncurry noTransitionTokens) [(t, p) | p <- newPlaces, t <- [0..(l-1)]]

    -- refine the must firers
    withTime "mustFireTransitions" mustFireTransitions

    -- set new initial and final state
    setInitialState inputs newPlaces
    setFinalState ret newPlaces
  where
    placeMerge t p nps = do
        placeMap <- place2variable <$> get
        -- default tokens
        let npVars = map (\np -> findVariable (np, t) placeMap) nps
        npZ3Vars <- mapM mkZ3IntVar npVars
        let pVar = findVariable (p, t) placeMap
        pZ3Var <- mkZ3IntVar pVar
        mkAdd npZ3Vars >>= mkEq pZ3Var >>= assert
        -- colored tokens
        let npColored = map (\np -> findVariable (np ++ "_colored", t) placeMap) nps
        npZ3Colored <- mapM mkZ3IntVar npColored
        let pColorVar = findVariable (p ++ "_colored", t) placeMap
        pColorZ3 <- mkZ3IntVar pColorVar
        mkAdd npZ3Colored >>= mkEq pColorZ3 >>= assert
        -- TODO: sanity check here: is it necessary to add constraints here for color indices?
        -- let npColors = map (\np -> findVariable (np ++ "_cidx", t) placeMap) nps
        

    transMerge (tr, ntrs) t = do
        transMap <- transition2id <$> get
        tsMap <- time2variable <$> get
        lv <- abstractionLv <$> get
        let ntrIds = map (\ntr -> findVariable ntr (findVariable lv transMap)) ntrs
        let trId = findVariable tr (findVariable (lv - 1) transMap)
        let tsVar = findVariable (t, lv) tsMap
        let lastTsVar = findVariable (t, lv - 1) tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        lastTsZ3Var <- mkZ3IntVar lastTsVar
        selectTr <- mkIntNum trId >>= mkEq lastTsZ3Var
        selectNtrs <- mapM mkIntNum ntrIds >>= mapM (mkEq tsZ3Var)
        mkOr selectNtrs >>= mkIff selectTr >>= assert

    transClone tr t = do
        transMap <- transition2id <$> get
        tsMap <- time2variable <$> get
        lv <- abstractionLv <$> get
        trVar <- mkIntNum (findVariable tr (findVariable lv transMap))
        lastTrVar <- mkIntNum (findVariable tr (findVariable (lv - 1) transMap))
        tsVar <- mkZ3IntVar (findVariable (t, lv) tsMap)
        lastTsVar <- mkZ3IntVar (findVariable (t, lv - 1) tsMap)
        selectLastTr <- mkEq lastTsVar lastTrVar
        selectTr <- mkEq tsVar trVar
        mkIff selectLastTr selectTr >>= assert

-- | add variables for each place:
-- 1) an integer variable for number of default tokens
-- 2) an integer variable for number of colored tokens
-- 3) an integer variable for color index
-- in this design, we allow only one color existing in the petri net
-- so there is no nested higher order functions
addPlaceVar ::  Id -> Encoder ()
addPlaceVar p = do
    l <- loc <$> get
    mapM_ addPlaceVarAt [0..l]
  where
    addPlaceVarAt t = do
        st <- get
        let placeVar = Variable (variableNb st) (p ++ "_" ++ show t) t 0 VarPlace
        let coloredVar = Variable (variableNb st + 1) (p ++ "_c_" ++ show t) t 0 VarPlace
        let colorVar = Variable (variableNb st + 2) (p ++ "_color_" ++ show t) t 0 VarPlace
        let p2v = HashMap.insert (p, t) placeVar
                $ HashMap.insert (p ++ "_colored", t) coloredVar
                $ HashMap.insert (p ++ "_cidx", t) colorVar
                $ place2variable st
        when (not (HashMap.member (p, t) (place2variable st)))
             (put $ st { place2variable = p2v
                       , variableNb = variableNb st + 3
                       })

-- | add transition mapping from (tr, lv) to integer id
-- 1) an integer variable for each transition
-- 2) an fresh integer variable for each higher-order argument
addTransitionVar :: [Id] -> Encoder ()
addTransitionVar trs = do
    lv <- gets abstractionLv
    start <- gets transitionNb -- inclusive start
    mapM_ addTransitionVarFor trs
    end <- gets transitionNb -- exclusive end
    st <- get
    put $ st { lv2range = HashMap.insert lv (start, end) (lv2range st) }
  where
    addTransitionVarFor tr = do
        lv <- abstractionLv <$> get
        st <- get
        let tid = transitionNb st
        -- liftIO $ putStrLn $ show tid ++ ": " ++ tr
        when (not (HashMap.member tr (HashMap.lookupDefault HashMap.empty lv (transition2id st))))
             (put $ st { transitionNb = 1 + transitionNb st
                       , transition2id = HashMap.insertWith HashMap.union lv (HashMap.singleton tr tid) (transition2id st)
                       , id2transition = HashMap.insert tid (tr, lv) (id2transition st)
                       })

addTimestampVar :: Int -> Encoder ()
addTimestampVar t = do
    absLv <- gets abstractionLv
    mapM_ addTimestampVarAt [0..absLv]
  where
    addTimestampVarAt l = do
        st <- get
        let tsVar = Variable (variableNb st) ("ts_" ++ show t ++ "_" ++ show l) t l VarTimestamp
        when (not (HashMap.member (t, l) (time2variable st)))
             (put $ st { time2variable = HashMap.insert (t, l) tsVar (time2variable st)
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

createConstraints :: [Id] -> [FunctionCode] -> Encoder ()
createConstraints places transitions = do
    -- prepare constraint parameters
    l <- gets loc
    let allTrans = [(t, tr) | t <- [0..(l-1)], tr <- transitions]
    let allPlaces = [(t, p) | t <- [0..(l-1)], p <- places]

    -- we assume we only have the first abstraction level each time when we call this method
    withTime "sequantial transitions:" sequentialTransitions
    -- we do not have parallel transitions in the first time encoding
    withTime "non negative:" (nonnegativeTokens places)

    withTime "fire conditions:" (mapM_ (uncurry fireTransitions) allTrans)

    withTime "no fire:" (mapM_ (uncurry noTransitionTokens) allPlaces)

    withTime "must fire:" mustFireTransitions

    withTime "assign colors:" (assignColors transitions)

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
sequentialTransitions :: Encoder ()
sequentialTransitions = do
    l <- gets loc
    lv <- gets abstractionLv
    lvRange <- gets lv2range
    let rng = findVariable lv lvRange
    mapM_ (fireAt rng) [0..(l-1)]
  where
    fireAt rng t = do
        tsMap <- time2variable <$> get
        lv <- abstractionLv <$> get
        let tsVar = findVariable (t, lv) tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        start <- mkIntNum (fst rng)
        mkGe tsZ3Var start >>= assert
        end <- mkIntNum (snd rng)
        mkLt tsZ3Var end >>= assert

-- | if this place has no connected transition fired,
-- it has the same # of tokens
noTransitionTokens :: Int -> Id -> Encoder ()
noTransitionTokens t p = do
    lv <- gets abstractionLv
    trans <- gets (findVariable lv . transition2id)
    t2tr <- gets ty2tr
    let transitions = map (\i -> findVariable i trans) (Map.findWithDefault [] p t2tr)
    noFireLvs <- noFireAt transitions p t
    noFire <- mkOr noFireLvs >>= mkNot
    placeMap <- place2variable <$> get
    curr <- mkZ3IntVar $ findVariable (p, t) placeMap
    next <- mkZ3IntVar $ findVariable (p, t + 1) placeMap
    tokenSame <- mkEq curr next
    currColor <- mkZ3IntVar $ findVariable (p ++ "_colored", t) placeMap
    nextColor <- mkZ3IntVar $ findVariable (p ++ "_colored", t + 1) placeMap
    coloredSame <- mkEq currColor nextColor
    currCidx <- mkZ3IntVar $ findVariable (p ++ "_cidx", t) placeMap
    nextCidx <- mkZ3IntVar $ findVariable (p ++ "_cidx", t + 1) placeMap
    cidxSame <- mkEq currCidx nextCidx
    mkAnd [tokenSame, coloredSame, cidxSame] >>= mkImplies noFire >>= assert
  where
    noFireAt transitions p t = do
        idVars <- mapM mkIntNum transitions
        tsMap <- time2variable <$> get
        lv <- abstractionLv <$> get
        let tsVar = findVariable (t, lv) tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        mapM (mkEq tsZ3Var) idVars

assignColors :: [FunctionCode] -> Encoder ()
assignColors trs = do
    -- build a mapping from arg types to their higher order args
    paramMap <- foldM collectTypes HashMap.empty trs
    mapM_ (uncurry assignInitialColor) (HashMap.toList paramMap)
  where
    collectTypes m (FunctionCode _ [] _ _) = return m
    collectTypes m (FunctionCode _ hoParams _ _) = foldM assignParam m hoParams

    assignParam m (FunctionCode name _ params _) = do
        colorMap <- gets ho2id
        let color = findVariable name colorMap
        let m' = foldr (\p -> HashMap.insertWith union p [color]) m params
        return m'

    assignInitialColor param colors = do
        -- each higher-order type is able to produce colored tokens
        -- this is encoded in the initial state
        placeMap <- gets place2variable
        let paramToken = param ++ "_colored"
        let paramColor = param ++ "_cidx"
        zero <- mkIntNum 0
        colorIdxs <- mapM mkIntNum colors
        paramInit <- mkZ3IntVar $ findVariable (paramToken, 0) placeMap
        colorInit <- mkZ3IntVar $ findVariable (paramColor, 0) placeMap
        hasTokens <- mkGt paramInit zero
        correctColors <- mapM (mkEq colorInit) colorIdxs
        correctColor <- mkOr correctColors
        mkImplies hasTokens correctColor >>= assert

fireTransitions :: Int -> FunctionCode -> Encoder ()
fireTransitions t (FunctionCode name [] params rets) = do
    transMap <- gets transition2id
    placeMap <- gets place2variable
    lv <- gets abstractionLv
    tsMap <- gets time2variable
    -- accumulate counting for parameters and return types
    let tid = findVariable name (findVariable lv transMap)
    let pcnt = map (\l -> (head l, length l)) (group (sort params))
    let pmap = Map.fromList pcnt
    let rmap = foldl' (\acc t -> Map.insertWith (+) t (-1) acc) pmap rets
    let rcnt = Map.toList rmap
    let tsVar = findVariable (t, lv) tsMap
    tsZ3Var <- mkZ3IntVar tsVar
    trVar <- mkIntNum tid
    changes <- mapM (mkChange False t) rcnt
    -- all parameter tokens must have the same color
    -- if one place has the id of p, then p + 1 indicates its colorful token number
    -- p + 2 indicates its color index
    let places = fst (unzip pcnt)
    let foPlaces = filter (not . ("AFunctionT" `isInfixOf`)) places
    let colors = map (\p -> findVariable (p ++ "_cidx", t) placeMap) foPlaces
    let currResColors = map (\p -> findVariable (p ++ "_cidx", t) placeMap) rets
    let resColors = map (\p -> findVariable (p ++ "_cidx", t + 1) placeMap) rets
    cids <- mapM mkZ3IntVar (colors ++ resColors ++ currResColors)
    dcolor <- mkIntNum defaultColor
    sameColors <- maybe (return [])
                        (\(x, xs) -> mapM (mkEq x) xs)
                        (uncons cids)
    fire <- mkEq tsZ3Var trVar
    hasDefault <- if null cids then mkTrue else mkEq (head cids) dcolor
    enoughTokens <- mapM (getSatisfiedPlace False) pcnt
    defaultPost <- mkAnd (hasDefault : enoughTokens ++ changes ++ sameColors)
    if places == foPlaces
       then do
           colorChanges <- mapM (mkChange True t) rcnt
           notDefault <- if null cids then mkTrue else mkGt (head cids) dcolor
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
    -- mapM_ addPlaceVarAt [(funName hp, v) | hp <- hoParams, v <- [0..(t+1)]]
    -- mapM_ initialHo (map funName hoParams)
    mapM_ newColor hoParams
    fireTransitions t (FunctionCode name [] params rets)
  where
    -- add higher-order places in demand
    addPlaceVarAt (p, v) = do
        st <- get
        let placeVar = Variable (variableNb st) (p ++ "_" ++ show v) v 0 VarPlace
        let p2v = HashMap.insert (p, v) placeVar $ place2variable st
        when (not (HashMap.member (p, v) (place2variable st)))
             (put $ st { place2variable = p2v
                       , variableNb = variableNb st + 1
                       })

    initialHo p = do
        placeMap <- gets place2variable
        let pv = findVariable (p, 0) placeMap
        pZ3 <- mkZ3IntVar pv
        mkIntNum 0 >>= mkEq pZ3 >>= assert

    newColor hp = do
        colorCnt <- gets colorNb
        lv <- gets abstractionLv
        placeMap <- gets place2variable
        transMap <- gets (findVariable lv . transition2id)
        colorMap <- gets ho2id
        tsMap <- gets time2variable
        let color = HashMap.lookupDefault colorCnt (funName hp) colorMap
        uncolorTr <- mkIntNum $ findVariable (funName hp ++ "|uncolor") transMap
        hoPlace <- mkZ3IntVar $ findVariable (funName hp, t) placeMap
        hoPlace' <- mkZ3IntVar $ findVariable (funName hp, t + 1) placeMap
        let tsVar = findVariable (t, lv) tsMap
        tsZ3Var <- mkZ3IntVar tsVar
        assignedColor <- mkIntNum color
        negUno <- mkIntNum (-1)
        uno <- mkIntNum 1

        -- each higher-order is able to consume returned colored tokens
        let retColor = head (funReturn hp) ++ "_cidx"
        let retToken = head (funReturn hp) ++ "_colored"
        retCurr <- mkZ3IntVar $ findVariable (retColor, t) placeMap
        retNext <- mkZ3IntVar $ findVariable (retColor, t + 1) placeMap
        retTokenCurr <- mkZ3IntVar $ findVariable (retToken, t) placeMap
        retTokenNext <- mkZ3IntVar $ findVariable (retToken, t + 1) placeMap
        fireUncolor <- mkEq tsZ3Var uncolorTr
        correctColor <- mkEq retCurr assignedColor
        enoughColor <- mkGe retTokenCurr uno
        hoChange <- mkAdd [hoPlace, uno] >>= mkEq hoPlace'
        retChange <- mkAdd [retTokenCurr, negUno] >>= mkEq retTokenNext
        hasDefault <- mkIntNum defaultColor >>= mkEq retNext
        onlyOne <- mkEq retTokenCurr uno
        colorChange <- mkImplies onlyOne hasDefault
        moreThanOne <- mkGt retTokenCurr uno
        colorRemain <- mkEq retNext retCurr >>= mkImplies moreThanOne
        postCond <- mkAnd [correctColor, enoughColor, hoChange, retChange, colorChange, colorRemain]
        mkImplies fireUncolor postCond >>= assert

        -- update status
        st <- get
        let color' = if color /= (colorNb st) then colorNb st else color + 1
        put $ st { colorNb = color'
                 , ho2id = HashMap.insert (funName hp) color (ho2id st) 
                 }


mustFireTransitions ::  Encoder ()
mustFireTransitions = do
    must <- mustFirers <$> get
    lv <- abstractionLv <$> get
    transitions <- findVariable lv . transition2id <$> get
    let mustTrans = HashMap.filterWithKey (\k _ -> nameInMust must k) transitions
    mapM_ fireTransition mustTrans
  where
    nameInMust must name = foldr ((||) . flip isInfixOf name) False must
    fireTransition tid = do
        l <- loc <$> get
        lv <- abstractionLv <$> get
        tsMap <- time2variable <$> get
        trId <- mkIntNum tid
        tsVars <- mapM (\t -> mkZ3IntVar (findVariable (t, lv) tsMap)) [0..(l-1)]
        trVars <- mapM (mkEq trId) tsVars
        mkOr trVars >>= assert
