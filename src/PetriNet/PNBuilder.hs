{-# LANGUAGE DeriveGeneric #-}

module PetriNet.PNBuilder(
   buildPetriNet
  , addFunction
  , removeTransition
  , addArgClone
  , areEqFuncs
)
where

import Types.Common
import Types.PetriNet
import Types.Program

import Synquid.Util
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Control.Lens
import Data.Aeson (ToJSON(..), genericToEncoding, defaultOptions)
import Data.Serialize (Serialize)
import GHC.Generics
import Data.Hashable


buildPetriNet :: [FunctionCode] -> [Id] -> PetriNet
buildPetriNet fs inputs = foldr addArgClone addFuncs (filter notNeg (HashMap.keys (pnPlaces addFuncs)))
  where
    notNeg n = (head n) /= '-'
    emptyPN = PetriNet HashMap.empty HashMap.empty HashMap.empty
    addFuncs = foldr addFunction emptyPN fs
    -- addArgs = foldr addArgClone addFuncs inputs

-- | create a new place in petri net
mkPlace :: Id -> Place
mkPlace id = Place id Set.empty Set.empty 1

-- | get the unique id of a place
getPlaceId :: Place -> Id
getPlaceId = placeId

-- | add a place to the petri net
-- if it already exists, do nothing
-- if it is a new place, create and add it
addPlace :: Id -> PetriNet -> PetriNet
addPlace id pn = pn {
  pnPlaces = if id `elem` (map getPlaceId $ HashMap.elems places)
               then places
               else HashMap.insert id (mkPlace id) places
  }
  where
    places = pnPlaces pn

-- | create a new transition
mkTransition :: Id -> Transition
mkTransition id = Transition id Set.empty Set.empty

-- | get the unique id of a transition
getTransitionId :: Transition -> Id
getTransitionId = transitionId

-- | add a transition to the petri net
addTransition :: Id -> PetriNet -> PetriNet
addTransition id pn = pn {
  pnTransitions = if id `elem` (map getTransitionId $ HashMap.elems transitions)
                    then transitions
                    else HashMap.insert id (mkTransition id) transitions
  }
  where
    transitions = pnTransitions pn

-- | remove a transition from the petri net
removeTransition :: Id -> PetriNet -> PetriNet
removeTransition id pn = pn {
    pnTransitions = HashMap.filterWithKey (\k _ -> k /= id) (pnTransitions pn),
    pnPlaces = HashMap.map clearSets (pnPlaces pn),
    pnFlows = HashMap.filter ((/=) id . flowTransition) (pnFlows pn)
  }
  where
    clearSets p = p {
        placePreset = Set.delete id (placePreset p),
        placePostset = Set.delete id (placePostset p)
    }

-- | create a flow id by the from to id
mkFlowId :: PetriNet -> Id -> Id -> Id
mkFlowId pn from to = unlines ["(", fromLabel, ",", toLabel, ")"]
  where
    prefixLabel id | id `HashMap.member` (pnPlaces pn) = "Place:"
    prefixLabel id | id `HashMap.member` (pnTransitions pn) = "Transition:"
    prefixLabel id = error $ id ++ " is neither a place id or a transition id"

    fromLabel = (prefixLabel from) ++ from
    toLabel = (prefixLabel to) ++ to

mkFlow :: PetriNet -> Id -> Id -> Int -> Flow
mkFlow pn from to w = Flow flowId from to placeId transId w
  where
    flowId = mkFlowId pn from to
    placeId = if from `HashMap.member` (pnPlaces pn) then from else to
    transId = if from `HashMap.member` (pnTransitions pn) then from else to

-- | add a flow to the petri net
-- 1) we need to add the flow to petri net set, maybe only change the weight of flow
-- 2) update the preset and postset of relevant transitions
-- 3) update the preset and postset of relevant places
addFlow :: Id -> Id -> Int -> PetriNet -> PetriNet
addFlow from to w pn = PetriNet places transitions flows
  where
    flowId = mkFlowId pn from to

    currFlow = if flowId `HashMap.member` (pnFlows pn)
                 then let f = fromJust (HashMap.lookup flowId (pnFlows pn))
                       in f { flowWeight = w + flowWeight f }
                 else  mkFlow pn from to w

    updatePlacePost tid p = p { placePostset = Set.insert tid (placePostset p) }
    updatePlacePre  tid p = p { placePreset  = Set.insert tid (placePreset  p) }
    updateTransitionPost tr = tr {
        transitionPostset = Set.insert flowId (transitionPostset tr)
      }
    updateTransitionPre  tr = tr {
        transitionPreset  = Set.insert flowId (transitionPreset  tr)
      }

    -- update the places so that it has correct preset and postset
    (placeId, place, transId, transition) =
      if from `HashMap.member` (pnPlaces pn) -- if the added flow goes out of a place, update its postset
        then ( from
             , updatePlacePost to (case (HashMap.lookup from (pnPlaces pn)) of
                                       Just s -> s
                                       Nothing -> error $ "cannot find place " ++ from)
             , to
             , updateTransitionPre (case (HashMap.lookup to (pnTransitions pn)) of
                                       Just t -> t
                                       Nothing -> error $ "cannot find transition " ++ to)
             )
        else ( to
             , updatePlacePre from (case (HashMap.lookup to (pnPlaces pn)) of
                                        Just p -> p
                                        Nothing -> error $ "cannot find place " ++ to)
             , from
             , updateTransitionPost (case (HashMap.lookup from (pnTransitions pn)) of
                                        Just t -> t
                                        Nothing -> error $ "cannot find transition " ++ from)
             )

    places = HashMap.insert placeId place (pnPlaces pn)
    transitions = HashMap.insert transId transition (pnTransitions pn)
    flows = HashMap.insert flowId currFlow (pnFlows pn)

addFunction :: FunctionCode -> PetriNet -> PetriNet
addFunction (FunctionCode name [] params ret) pn = pn'
  where
    placedPn = foldr addPlace pn ("void" : (ret ++ params))
    transitionedPn = addTransition name placedPn
    assignedParams = map (\p -> (p , name, 1))
                         $ if null params then ["void"] else params
    retedPn = foldr (\r p -> addFlow name r 1 p) transitionedPn ret
    flowedPn = foldr (uncurry3 addFlow) retedPn assignedParams
    pn' = flowedPn

addFunction (FunctionCode name hoParams params ret) pn = pn'
  where
    -- negative types go into the higher order functions
    paramToAdd = filter (not . isPrefixOf "f") params
    -- assume there is no nested higher order parameters
    hoRets = concatMap funReturn hoParams
    negParams = map ((:) '-') (concatMap funParams hoParams)
    places = paramToAdd ++ hoRets ++ negParams
    placedPn = foldr addPlace pn (places ++ ret)
    transitionedPn = addTransition name placedPn
    retedPn = foldr (\r p -> addFlow name r 1 p) transitionedPn ret
    flowedPn = foldr (\t p -> addFlow t name 1 p) retedPn places
    pn' = flowedPn -- flowedPn (concatMap funParams hoParams)

addArgClone tArg pn = pn'
  where
    placedPn = addPlace tArg pn
    transitionedPn = addTransition (tArg ++ "|clone") placedPn
    retedPn = addFlow (tArg ++ "|clone") tArg 2 transitionedPn
    flowedPn = addFlow tArg (tArg ++ "|clone") 1 retedPn
    pn' = flowedPn
        {-
setMaxToken :: [Id] -> PetriNet -> PetriNet
setMaxToken inputs pn = pn {
        pnPlaces = HashMap.foldrWithKey (\pid mt ->
            HashMap.insert pid (setMaxToken (pnPlaces pn) pid mt))
            HashMap.empty inputedCnt
    }
  where
    counts = HashMap.map (\p -> 1) $ pnPlaces pn

    transitions = HashMap.elems $ pnTransitions pn

    inputCounts = map (\t -> (head t, length t)) $ group $ sort inputs

    updateMt p mt = p { placeMaxToken = max (placeMaxToken p) mt }

    setMaxToken places pid mt =
        updateMt (case HashMap.lookup pid places of
                      Just p -> p
                      Nothing -> error $ "cannot find place " ++ pid) mt

    checkTransition tr cntMap =
        let flows    = pnFlows pn
            preFlows = map (\f -> case HashMap.lookup f flows of
                                      Just ff -> ff
                                      Nothing -> error $ "cannot find flow " ++ f)
                           $ Set.toList $ transitionPreset tr
        in foldr updateByFlow cntMap preFlows
    updateByFlow f cntMap =
        let places = pnPlaces pn
            src    = case HashMap.lookup (flowPlace f) places of
                        Just p -> p
                        Nothing -> error $ "cannot find place " ++ (flowPlace f)
            w      = flowWeight f
        in HashMap.insertWith max (flowPlace f) w cntMap
    tredCnt = foldr checkTransition counts transitions
    inputedCnt = foldr (uncurry (HashMap.insertWith max)) tredCnt inputCounts
-}

equating a b f = f a == f b

areEqFuncs fc1 fc2 = let
  ho = equating fc1 fc2 (sort . hoParams)
  params = equating fc1 fc2 (sort . funParams)
  rets = equating fc1 fc2 (sort . funReturn)
  in ho && params && rets