module HyperGraph.HyperExplorer where

import HyperGraph.HyperGraph
import Types.Abstract

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad.Logic
import Data.List

data Direction = Forward | Backward

visit :: Direction -- forward or backward visit
      -> AbstractSkeleton -- source node
      -> HyperGraph -- the graph to visit
      -> Map AbstractSkeleton Bool -- labels of each node
visit dir s graph = go initLabels initTailCnts (Seq.singleton s)
    where
        initLabels = Map.fromSet (== s) (nodesOf graph)
        initTailCnts = Map.fromSet (const 0) (Set.fromList $ concat $ Map.elems graph)

        go :: Map AbstractSkeleton Bool -- blabels
           -> Map Edge Int -- count of edge tails
           -> Seq AbstractSkeleton -- bfs queue
           -> Map AbstractSkeleton Bool
        go blabel tcnts q 
            | Seq.null q = blabel
            | otherwise = case Seq.viewl q of
                Seq.EmptyL -> error "empty queue"
                u Seq.:< remains -> let
                    connectedGraph = Map.filterWithKey (\k _ -> Set.member u k) graph
                    allEdges = concat (Map.elems graph)
                    connectedEdges = case dir of
                        Backward -> concat (Map.elems connectedGraph)
                        Forward -> filter (\(_,_,hs) -> Set.member u hs) allEdges
                    (tcnts', blabel', q') = foldr (updateQueue dir) (tcnts, blabel, remains) connectedEdges 
                    in go blabel' tcnts' q'

        updateLabel :: AbstractSkeleton 
            -> (Map AbstractSkeleton Bool, Seq AbstractSkeleton)
            -> (Map AbstractSkeleton Bool, Seq AbstractSkeleton)
        updateLabel v (blabel, q) = if not (blabel Map.! v) 
            then let blabel' = Map.insert v True blabel
                     q' = q Seq.|> v
                 in (blabel', q')
            else (blabel, q)

        updateQueue :: Direction
            -> Edge 
            -> (Map Edge Int, Map AbstractSkeleton Bool, Seq AbstractSkeleton)
            -> (Map Edge Int, Map AbstractSkeleton Bool, Seq AbstractSkeleton)
        updateQueue dir e@(tl, _, hd) (tcnt, blabel, q) = let
            tcnt' = Map.insertWith (+) e 1 tcnt
            checkSet = case dir of Backward -> tl; Forward -> hd
            traverSet = case dir of Backward -> hd; Forward -> tl
            in if tcnt' Map.! e == Set.size checkSet
                    then let (l, q') = Set.foldr updateLabel (blabel, q) traverSet
                          in (tcnt', l, q')
                    else (tcnt', blabel, q)

bvisit :: AbstractSkeleton -> HyperGraph -> Map AbstractSkeleton Bool
bvisit = visit Backward

fvisit :: AbstractSkeleton -> HyperGraph -> Map AbstractSkeleton Bool
fvisit = visit Forward

frontier :: HyperGraph -> AbstractSkeleton -> AbstractSkeleton -> HyperGraph
frontier graph src dst = go graph
    where
        go :: HyperGraph -> HyperGraph
        go g = let
            blabel = bvisit src g
            flabel = fvisit dst g
            nodes = nodesOf g
            (change', g') = foldr (updateFrontier blabel flabel) (False, g) nodes
            nodes' = nodesOf g'
            in if not (Set.member src nodes') || not (Set.member dst nodes')
                then Map.empty
                else if change' then go g' else g'

        updateFrontier :: Map AbstractSkeleton Bool -- labels from bvisit
            -> Map AbstractSkeleton Bool -- labels from fvisit
            -> AbstractSkeleton -- current node (to be decided)
            -> (Bool, HyperGraph) -- accumulated pruned graph
            -> (Bool, HyperGraph)
        updateFrontier blabel flabel v (change, g) = 
            if not (blabel Map.! v) || not (flabel Map.! v)
                then (True, deleteNode v g)
                else (change, g)

getBFPath :: AbstractSkeleton -> AbstractSkeleton -> HyperGraph -> LogicT IO HyperGraph
getBFPath src dst graph = do
    let graph' = frontier graph src dst
    if Map.null graph'
        then return graph'
        else do
            let nodes = Set.toList $ Set.delete src (Set.delete dst (nodesOf graph))
            nodes <- perm nodes
            let graph = foldr minimizeNode graph' nodes
            let edges = concat (Map.elems graph)
            edges <- perm edges
            let graph' = foldr minimizeEdge graph edges
            return graph'
    where
        minimizeNode :: AbstractSkeleton -> HyperGraph -> HyperGraph
        minimizeNode v g = let
            g' = deleteNode v g
            g'' = frontier g' src dst
            in if Map.null g'' then g else g''

        minimizeEdge :: Edge -> HyperGraph -> HyperGraph
        minimizeEdge e g = let
            g' = deleteEdge e g
            blabel = bvisit src g'
            flabel = fvisit dst g'
            visited v = blabel Map.! v && flabel Map.! v
            in if all visited (nodesOf g) then g' else g

{- Helper functions -}
nodesOf :: HyperGraph -> Set AbstractSkeleton
nodesOf graph = Set.unions (concatMap (map nodesInEdges) (Map.elems graph))
    where
        nodesInEdges (ts, f, hs) = ts `Set.union` hs

deleteNode :: AbstractSkeleton -> HyperGraph -> HyperGraph
deleteNode v = Map.filterWithKey notHasNode
    where
        edgeHasNode (tl, _, hd) = Set.member v tl || -- member of the edge tail
                                  Set.member v hd -- member of the edge head
        notHasNode s es = not (Set.member v s || -- member of the key (tail)
                               any edgeHasNode es) -- member of any edge

deleteEdge :: Edge -> HyperGraph -> HyperGraph
deleteEdge e g = Map.filter (not . null) (Map.map (delete e) g)

perm :: [a] -> LogicT IO [a]
perm [] = return []
perm (x:xs) = perm xs >>= ins x
    where
        ins :: a -> [a] -> LogicT IO [a]
        ins x []     = return [x]
        ins x (y:ys) = return (x:y:ys) `mplus` ((y:) <$> ins x ys)