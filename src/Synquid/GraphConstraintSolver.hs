module Synquid.GraphConstraintSolver
(
    solveGraphConstraints
) where

import Synquid.Succinct
import Synquid.Graph hiding (nodes, edges)
import Synquid.Program
import Synquid.Util

import Data.List
import Data.Maybe
import qualified Z3.Monad as Z3
import Z3.Base (Context, Optimize)
import Z3.Monad (AST, MonadZ3)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Control.Monad.State

-- * path constaints
-- constraints for each node
-- For a simple node: V ⇒ (E1 ∨ … ∨ En) for all outgoing edges and similar for incoming edges. 
-- For a compound node V ⇒ (E1 ∧ … ∧ En) for incoming edges.
nodeConstraint :: MonadZ3 z3 => SuccinctGraph -> SuccinctType -> z3 ()
nodeConstraint graph v = do
    let ins = allIncomingEdges graph v
    let outs = allOutgoingEdges graph v
    boolS <- Z3.mkBoolSort
    inSyms <- mapM Z3.mkStringSymbol ins
    outSyms <- mapM Z3.mkStringSymbol outs
    vSym <- Z3.mkStringSymbol (show v)
    vConst <- Z3.mkConst vSym boolS
    inConsts <- mapM (\sym -> Z3.mkConst sym boolS) inSyms
    outConsts <- mapM (\sym -> Z3.mkConst sym boolS) outSyms
    -- our graph here has reversed arrows as our theory design, be careful when reading this
    -- all incoming edges here are randomly selected
    if length inConsts > 0 
        then Z3.mkOr inConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
        else return ()

    -- predicates for outgoing edges depend on node type
    if length outConsts > 0
        then case v of
            SuccinctComposite _ -> Z3.mkAnd outConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
            _                   -> Z3.mkOr  outConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
        else return ()
    -- return [nodeIn, nodeOut]

-- For every edge: E ⇒ (V1 ∧ V2) for its start and end node. 
edgeConstraint :: MonadZ3 z3 => SuccinctGraph -> Id -> z3 ()
edgeConstraint graph e = do
    let nodes = verticesOf graph e
    -- liftIO $ print e
    -- liftIO $ print nodes
    boolS <- Z3.mkBoolSort
    edgeSym <- Z3.mkStringSymbol e
    edgeConst <- Z3.mkConst edgeSym boolS
    nodeSyms <- mapM (Z3.mkStringSymbol . show) nodes
    nodeConsts <- mapM (\sym -> Z3.mkConst sym boolS) nodeSyms
    nodeCond <- Z3.mkAnd nodeConsts
    Z3.mkImplies edgeConst nodeCond >>= Z3.optimizeAssert

getKSolutions :: MonadZ3 z3 => [AST] -> [AST] -> z3 ()
getKSolutions edgeConsts nodeConsts = getKSolutions' 10
  where
    getKSolutions' n | n == 0 = return ()
    getKSolutions' n = do
        -- print the results
        
        liftIO $ putStrLn $ "Searching for the next path..."
        res <- Z3.optimizeCheck
        model <- Z3.optimizeGetModel
        satEdges <- filterM (liftM fromJust . Z3.evalBool model) edgeConsts
        satNodes <- filterM (liftM fromJust . Z3.evalBool model) nodeConsts
        es <- mapM Z3.astToString satEdges
        ns <- mapM Z3.astToString satNodes
        liftIO $ putStrLn $ "Selected edges:"
        liftIO $ mapM putStrLn es
        liftIO $ putStrLn $ "Selected nodes:"
        liftIO $ mapM putStrLn ns
        let satPath = satEdges ++ satNodes
        Z3.mkAnd satPath >>= Z3.mkNot >>= Z3.optimizeAssert
        getKSolutions' (n-1)

solveGraphConstraints :: MonadZ3 z3 => SuccinctGraph -> SuccinctType -> [Id] -> z3 ()
solveGraphConstraints graph goal args = do
    let nodes = allNodes graph -- list of succinct types
    let edges = allEdges graph -- list of succinct edges
    -- liftIO $ print edges
    -- generate all related constraints
    boolS <- Z3.mkBoolSort
    nodeSyms <- mapM (Z3.mkStringSymbol . show) nodes
    nodeConsts <- mapM (\sym -> Z3.mkConst sym boolS) nodeSyms
    edgeSyms <- mapM (Z3.mkStringSymbol . getEdgeId) edges
    edgeConsts <- mapM (\sym -> Z3.mkConst sym boolS) edgeSyms

    -- add assertions to the optimization
    -- hard constraints for path connectivity
    mapM_ (nodeConstraint graph) nodes
    mapM_ (edgeConstraint graph . getEdgeId) edges    
    -- mapM_ Z3.optimizeAssert (concat nodeConts ++ edgeConts)

    -- hard constraints for contain both the goal node and the arg nodes
    let mustContain = (show goal):args
    mustContainSyms <- mapM Z3.mkStringSymbol mustContain
    mustContainConsts <- mapM (\sym -> Z3.mkConst sym boolS) mustContainSyms
    mapM_ Z3.optimizeAssert mustContainConsts

    -- soft constraints for contain edges
    groupSym <- Z3.mkStringSymbol "path"
    mapM_ (\(ast, weight) -> do
            notAst <- Z3.mkNot ast
            Z3.optimizeAssertSoft notAst weight groupSym) 
         $ zip edgeConsts (map (show . getEdgeWeight) edges)

    asserts <- Z3.optimizeGetAssertions >>= mapM Z3.astToString 
    -- liftIO $ mapM putStrLn asserts
    getKSolutions edgeConsts nodeConsts

-- util functions
allIncomingEdges graph v = Set.toList $ Set.filter (not . isInfixOf "__goal__") $
    HashMap.foldrWithKey (\from m res -> 
        HashMap.foldrWithKey (\to set res' -> if to == v then res' `Set.union` Set.map getEdgeId set else res') res m
        ) Set.empty graph

allOutgoingEdges graph v = Set.toList $ Set.filter (not . isInfixOf "__goal__") $
    Set.map getEdgeId $ Set.unions $ HashMap.elems $ HashMap.lookupDefault HashMap.empty v graph

-- | get the from and to nodes for an edge
verticesOf graph e = 
    HashMap.foldrWithKey (\from m res ->
        HashMap.foldrWithKey (\to set res' -> if e `Set.member` Set.map getEdgeId set then [from,to] else res') res m
        ) [] graph

allNodes graph = nub $ HashMap.keys graph ++ (concat . HashMap.elems $ HashMap.map HashMap.keys graph)
allEdges graph = -- filter (not . isInfixOf "__goal__" . getEdgeId) $ 
    Set.toList . Set.unions . concat . map HashMap.elems $ HashMap.elems graph