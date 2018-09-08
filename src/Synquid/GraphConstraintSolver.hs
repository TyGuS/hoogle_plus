module Synquid.GraphConstraintSolver where

import Synquid.Succinct
import Synquid.Graph

import qualified Z3.Base as Z3
import Z3.Base (Context, Optimize, AST)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

--* path constaints
-- constraints for each node
-- For a simple node: V ⇒ (E1 ∨ … ∨ En) for all outgoing edges and similar for incoming edges. 
-- For a compound node V ⇒ (E1 ∧ … ∧ En) for incoming edges.
nodeConstraint :: Context -> SuccinctGraph -> SuccinctType -> IO [AST]
nodeConstraint ctx graph v = do
    let ins = Set.toList $ allIncomingEdges graph v
    let outs = Set.toList $ allOutgoingEdges graph v
    boolS <- Z3.mkBoolSort ctx
    inSyms <- mapM (Z3.mkStringSymbol ctx) ins
    outSyms <- mapM (Z3.mkStringSymbol ctx) outs
    vSym <- Z3.mkStringSymbol ctx (show v)
    vConst <- Z3.mkConst ctx vSym boolS
    inConsts <- mapM (\sym -> Z3.mkConst ctx sym boolS) inSyms
    outConst <- mapM (\sym -> Z3.mkConst ctx sym boolS) outSyms
    -- our graph here has reversed arrows as our design, be careful when reading this
    -- all incoming edges here are randomly selected
    inCond <- Z3.mkOr ctx inConsts
    nodeIn <- Z3.mkImplies ctx vConst inCond

    -- predicates for outgoing edges depend on node type
    outCond <- case v of
                SuccinctComposite _ _ -> Z3.mkAnd ctx outConsts
                _ -> Z3.mkOr ctx inConsts
    nodeOut <- Z3.mkImplies ctx vConst outCond
    return [nodeIn, nodeOut]

-- For every edge: E ⇒ (V1 ∧ V2) for its start and end node. 
edgeConstraint :: Context -> SuccinctGraph -> Id -> IO AST
edgeConstraint ctx graph e = do
    let nodes = verticesOf graph e
    boolS <- Z3.mkBoolSort ctx
    edgeSym <- Z3.mkStringSymbol ctx e
    edgeConst <- Z3.mkConst ctx edgeSym boolS
    nodeSyms <- mapM (Z3.mkStringSymbol ctx) nodes
    nodeConsts <- mapM (\sym -> Z3.mkConst ctx sym boolS) nodeSyms
    nodeCond <- Z3.mkAnd ctx nodeConsts
    edgeCond <- Z3.mkImplies edgeConst nodeCond
    return edgeCond

solveGraphConstraints :: SuccinctGraph -> [SuccinctType] -> IO ()
solveGraphConstraints graph mustContain = do
    let nodes = allNodes graph -- list of succinct types
    let edges = allEdges graph -- list of succinct edges
    cfg <- Z3.mkConfig
    ctx <- Z3.mkContext cfg
    opt <- Z3.mkOptimize ctx

    -- generate all related constraints
    boolS <- Z3.mkBoolSort ctx
    nodeSyms <- mapM (Z3.mkStringSymbol ctx . show) nodes
    nodeConsts <- mapM (\sym -> Z3.mkConst ctx sym boolS) nodeSyms
    edgeSyms <- mapM (Z3.mkStringSymbol ctx . getEdgeId) edges
    edgeConsts <- mapM (\sym -> Z3.mkConst ctx sym boolS) edgeSyms
    nodeConts <- mapM (nodeConstraint ctx graph) nodes
    edgeConts <- mapM (edgeConstraint ctx graph . getEdgeId) edges

    -- add assertions to the optimization
    -- hard constraints for path connectivity
    mapM (Z3.optimizeAssert ctx opt) (concat nodeConts ++ edgeConts)
    -- hard constraints for contain both the goal node and the arg nodes
    mustContainSyms <- mapM (Z3.mkStringSymbol ctx . show) mustContain
    mustContainConsts <- mapM (\sym -> Z3.mkConst ctx sym boolS) mustContainSyms
    mapM (Z3.optimizeAssert ctx opt) mustContainConsts
    -- soft constraints for contain edges
    groupSym <- Z3.mkStringSymbol ctx "path"
    mapM (\(ast, weight) -> Z3.optimizeAssertSoft ctx opt ast weight groupSym) 
         $ zip edgeConsts (map (show . getEdgeWeight) edges)

    -- print the results
    Z3.optimizeCheck ctx opt >>= print
    Z3.optimizeGetModel ctx opt >>= Z3.modelToString ctx >>= putStrLn

-- util functions
allIncomingEdges graph v = 
    HashMap.foldrWithKey (\from m res -> 
        res `Set.union` HashMap.filterWithKey (\to set -> if to == v then Set.map getEdgeId set else Set.empty) m
        ) Set.empty graph

allOutgoingEdges graph v = 
    Set.map getEdgeId $ Set.unions $ HashMap.elems $ HashMap.lookupDefault HashMap.empty v graph

-- | get the from and to nodes for an edge
verticesOf graph e = 
    HashMap.foldrWithKey (\from m res ->
        HashMap.foldrWithKey (\to set res' -> if e `Set.member` Set.map getEdgeId set then [from,to] else res')
        ) [] graph

allNodes graph = nub $ HashMap.keys ++ (concat . elems $ HashMap.map HashMap.keys graph)
allEdges graph = Set.toList . Set.unions . concat . map HashMap.elems $ HashMap.elems graph