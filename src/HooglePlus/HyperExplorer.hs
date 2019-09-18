module HooglePlus.HyperExplorer where

import Types.HyperGraph
import Types.Abstract
import Types.Common
import PetriNet.AbstractType
import Synquid.Pretty

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Tree
import Debug.Trace
import Control.Monad.Logic

mkPetriNet :: Int -> [FunctionCode] -> PetriNet
mkPetriNet maxIndex = foldr (addTransition maxIndex) $ PetriNet {
        transitions = Map.empty,
        consumptionTree = Node (Left (-1)) [],
        productionTree = Node (Left (-1)) []
    }

addTransition :: Int -> FunctionCode -> PetriNet -> PetriNet
addTransition maxIndex (FunctionCode f _ args res) net = net {
        transitions = Map.insert f tr (transitions net),
        consumptionTree = consumptionTree',
        productionTree = productionTree'
    }
    where
        consumptionMap = setFromLists baseList $ listToMap args
        productionMap = setFromLists baseList $ listToMap res
        tr = Transition {
                consumesFrom = consumptionMap,
                producesAt = productionMap,
                transitionId = f
            }
        baseList = replicate maxIndex 0
        consumptionTree' = insertTransition f consumptionMap (consumptionTree net)
        productionTree' = insertTransition f productionMap (productionTree net)

insertTransition :: Id -> [Int] -> StateTree -> StateTree
insertTransition f [] (Node lb forest) = Node lb (Node (Right f) [] : forest)
insertTransition f (cnt:res) (Node lb forest) =
    case findIndex eqCount forest of
        Nothing -> Node lb (insertTransition f res (Node (Left cnt) []) : forest)
        Just childIndex -> let newChild = insertTransition f res (forest !! childIndex)
                            in Node lb (setIth forest childIndex newChild)
    where
        eqCount (Node (Left n) _) = n == cnt
        eqCount (Node (Right _) _) = False

fireTransitions :: PNState -> PNState -> StateTree -> Logic (PNState, Id)
fireTransitions [] acc (Node lb forest) = msum $ map (\node ->
    case node of
        Node (Left n) children | n == 0 -> fireTransitions [] acc node
                               | otherwise -> mzero
        Node (Right f) _ -> return (reverse acc, f)
        ) forest
fireTransitions (curr:nexts) acc (Node lb forest) = msum $ map (\node ->
    case node of
        Node (Left n) children | curr >= n -> fireTransitions nexts ((curr-n):acc) node
                               | otherwise -> mzero
        Node (Right f) _ -> return (reverse acc ++ (curr : nexts), f)
        ) forest

applyTransition :: PetriNet -> Direction -> (PNState, Id) -> (PNState, Id)
applyTransition net dir (st, f) = let
    tr = transitions net Map.! f
    in case dir of
        Forward -> (addList st (producesAt tr), f)
        Backward -> (addList st (consumesFrom tr), f)

type QueueNode = [(PNState, Id)]

searchPetriNet :: PetriNet -> PNState -> PNState -> [Id]
searchPetriNet net initial final = bisearchPN net 0 [[(initial, "")]] [[(final, "")]]

bisearchPN :: PetriNet -> Int -> [QueueNode] -> [QueueNode] -> [Id]
bisearchPN net lv forwards backwards =
    case isConnected forwards of
        [] | lv < 6 -> case bisearchPN net (lv + 1) forwards' backwards of
                            [] -> bisearchPN net (lv + 2) forwards' backwards'
                            res -> res
           | otherwise -> error "cannot find a path"
        res -> res
    where
        checkStates = map (fst . head) backwards

        isConnected [] = []
        isConnected (f:fs) = case elemIndex (fst (head f)) checkStates of
            Nothing -> isConnected fs
            Just i -> map snd (reverse f) ++ map snd (drop 1 (backwards !! i))

        forwards' = concatMap (expandOne net Forward) forwards
        backwards' = concatMap (expandOne net Backward) backwards

expandOne :: PetriNet -> Direction -> QueueNode -> [QueueNode]
expandOne net dir initial = traceShow expandStates $ map (:initial) expandStates
    where
        states = case dir of
            Forward -> observeAll $ fireTransitions (fst (head initial)) [] (consumptionTree net)
            Backward -> observeAll $ fireTransitions (fst (head initial)) [] (productionTree net)
        expandStates = map (applyTransition net dir) states

listToMap :: Ord a => [a] -> [(a, Int)]
listToMap lst = map (\l -> (head l, length l)) (group (sort lst))

setIth :: [a] -> Int -> a -> [a]
setIth xs n x = reverse $ snd $ foldl fold_fun base xs
    where
        base = (0, [])
        fold_fun (i, acc) elmt = (i + 1, (if i == n then x else elmt) : acc)

setFromLists :: [a] -> [(Int, a)] -> [a]
setFromLists = foldl (uncurry . setIth)

addList :: [Int] -> [Int] -> [Int]
addList xs ys = zipWith (+) xs' ys'
    where
        xlen = length xs
        ylen = length ys
        xs' = if xlen < ylen then xs ++ replicate (ylen - xlen) 0 else xs
        ys' = if xlen < ylen then ys else ys ++ replicate (xlen - ylen) 0