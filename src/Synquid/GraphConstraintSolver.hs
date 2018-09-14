module Synquid.GraphConstraintSolver
(
    addGraphConstraints
  , getPathSolution
  , EdgeType(..)
) where

import Synquid.Succinct
import Synquid.Graph hiding (nodes, edges)
import Synquid.Program
import Synquid.Util

import Data.List
import Data.Maybe
import qualified Z3.Monad as Z3
import Z3.Base (Context, Optimize)
import Z3.Monad (AST, MonadZ3, Result(..))
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Extra

data EdgeType = BoolVar 
              | IntSet 
              | BoolSet -- encode X(e,arg) as a bool value which means the edge `e` comes from inhabited `arg`

-- * path constaints
-- constraints for each node
-- For a simple node: V ⇒ (E1 ∨ … ∨ En) for all outgoing edges and similar for incoming edges. 
-- For a compound node V ⇒ (E1 ∧ … ∧ En) for incoming edges.
nodeConstraint :: MonadZ3 z3 => SuccinctGraph -> SuccinctType -> [Id] -> EdgeType -> SuccinctType -> z3 ()
nodeConstraint graph goal args edgeType v = do
    let ins = allIncomingEdges graph v
    let outs = allOutgoingEdges graph v
    boolS <- Z3.mkBoolSort
    intS <- Z3.mkIntSort
    intSet <- Z3.mkSetSort intS
    inSyms <- mapM Z3.mkStringSymbol ins
    outSyms <- mapM Z3.mkStringSymbol outs
    vSym <- Z3.mkStringSymbol (show v)
    vConst <- Z3.mkConst vSym boolS
    case edgeType of
        BoolVar -> do
            inConsts <- mapM (\sym -> Z3.mkConst sym boolS) inSyms
            outConsts <- mapM (\sym -> Z3.mkConst sym boolS) outSyms
            when (length inConsts > 0 && goal /= v) (Z3.mkOr inConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert)
            when (length outConsts > 0)
                (case v of
                    SuccinctComposite _ -> Z3.mkAnd outConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
                    _                   -> Z3.mkOr  outConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert)
        IntSet -> do
            inConsts <- mapM (\sym -> Z3.mkConst sym intSet) inSyms
            outConsts <- mapM (\sym -> Z3.mkConst sym intSet) outSyms
    
            emptySet <- Z3.mkEmptySet intS
            -- our graph here has reversed arrows as our theory design, be careful when reading this
            -- at least one of the incoming edges is not empty (selected)
            -- TODO: for inhabited nodes, what if we have several different candidates
            when (length inConsts > 0 && goal /= v)
                 (mapM (flip (>>=) Z3.mkNot . Z3.mkEq emptySet) inConsts >>= Z3.mkOr >>= Z3.mkImplies vConst >>= Z3.optimizeAssert)

            -- for inhabited nodes, we trace the source node information in it
            -- if isSuccinctInhabited v
            --     then mapM (\name -> do
            --                     edgeVar <- Z3.mkStringSymbol name >>= flip Z3.mkConst intSet
            --                     idxVar <- Z3.mkStringSymbol name >>= flip Z3.mkConst intS
            --                     Z3.mkSetAdd emptySet idxVar >>= Z3.mkEq edgeVar
            --                 ) ins >>= Z3.mkOr >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
            --     else if length inConsts > 0 && goal /= v
            --         then mapM (flip (>>=) Z3.mkNot . Z3.mkEq emptySet) inConsts >>= Z3.mkOr >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
            --         else return ()

            -- predicates for outgoing edges depend on node type
            outNonempty <- mapM (flip (>>=) Z3.mkNot . Z3.mkEq emptySet) outConsts
            when (length outConsts > 0)
                 (case v of
                    SuccinctComposite _ -> do
                        -- all the outgoing edges of a compound node cannot be empty
                        Z3.mkAnd outNonempty >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
                        -- incoming edges for compound nodes have all the arguments from outgoing edges or it is empty(not selected)
                        -- mapM_ (\inEdge -> do
                        --             hasAll <- Z3.mkSetUnion outConsts >>= Z3.mkEq inEdge
                        --             isEmpty <- Z3.mkEq emptySet inEdge
                        --             Z3.mkOr [hasAll, isEmpty] >>= Z3.optimizeAssert) inConsts
                    _                    -> do
                        -- at least one of the outgoing edges of a simple node is not empty
                        Z3.mkOr outNonempty >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
                        -- outgoing edges should have the subset relationship between any two edges for simple nodes
                        -- mapM (uncurry subsetOrSuperset) [(x, y) | x <- outConsts, y <- outConsts, x < y] >>= Z3.mkAnd >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
                        -- for simple nodes that incoming edges and outgoing edges have the same information
                        -- inSet <- Z3.mkSetUnion inConsts
                        -- outSet <- Z3.mkSetUnion outConsts
                        -- Z3.mkEq inSet outSet >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
                 )
        BoolSet -> do
            inSyms <- concatMapM (\s -> mapM (Z3.mkStringSymbol . ((s ++ "_") ++)) args) ins
            outSyms <- concatMapM (\s -> mapM (Z3.mkStringSymbol . ((s ++ "_") ++)) args) outs
            inConsts <- mapM (\sym -> Z3.mkConst sym boolS) inSyms
            outConsts <- mapM (\sym -> Z3.mkConst sym boolS) outSyms
            when (length inConsts > 0 && goal /= v) (Z3.mkOr inConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert)
            when (length outConsts > 0)
                (case v of
                    SuccinctComposite _ -> Z3.mkAnd outConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
                    _                   -> Z3.mkOr  outConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert)

    where
        subsetOrSuperset s1 s2 = do
            r1 <- Z3.mkSetSubset s1 s2
            r2 <- Z3.mkSetSubset s2 s1
            Z3.mkOr [r1, r2]

-- For every edge: E ⇒ (V1 ∧ V2) for its start and end node. 
edgeConstraint :: MonadZ3 z3 => SuccinctGraph -> [Id] -> EdgeType -> Id -> z3 ()
edgeConstraint graph args edgeType e = do
    let nodes = verticesOf graph e
    -- liftIO $ print e
    -- liftIO $ print nodes
    boolS <- Z3.mkBoolSort
    intS <- Z3.mkIntSort
    intSet <- Z3.mkSetSort intS
    edgeSym <- Z3.mkStringSymbol e
    nodeSyms <- mapM (Z3.mkStringSymbol . show) nodes
    nodeConsts <- mapM (\sym -> Z3.mkConst sym boolS) nodeSyms
    nodeCond <- Z3.mkAnd nodeConsts

    case edgeType of
        BoolVar -> do
            edgeConst <- Z3.mkConst edgeSym boolS
            Z3.mkImplies edgeConst nodeCond >>= Z3.optimizeAssert
        IntSet -> do
            edgeConst <- Z3.mkConst edgeSym intSet
            edgeNonempty <- Z3.mkEmptySet intS >>= Z3.mkEq edgeConst >>= Z3.mkNot
            Z3.mkImplies edgeNonempty nodeCond >>= Z3.optimizeAssert
        BoolSet -> do
            edgeSyms <- mapM (Z3.mkStringSymbol . ((e ++ "_") ++)) args
            edgeConsts <- mapM (\sym -> Z3.mkConst sym boolS) edgeSyms
            mapM_ (\ec -> Z3.mkImplies ec nodeCond >>= Z3.optimizeAssert) edgeConsts
    

getPathSolution :: MonadZ3 z3 => [AST] -> [AST] -> EdgeType -> z3 ()
getPathSolution edgeConsts nodeConsts edgeType = do
    -- print the results
    liftIO $ putStrLn $ "Searching for the next path..."
    res <- Z3.optimizeCheck
    case res of
        Sat -> do
            liftIO $ putStrLn "Sat"
            model <- Z3.optimizeGetModel
            -- str <- Z3.modelToString model
            -- liftIO $ putStrLn str 
            intS <- Z3.mkIntSort
            emptySet <- Z3.mkEmptySet intS

            satNodes <- filterM (liftM fromJust . Z3.evalBool model) nodeConsts
            satEdges <- case edgeType of
                            BoolVar -> filterM (liftM fromJust . Z3.evalBool model) edgeConsts
                            IntSet -> filterM (liftM fromJust . (=<<) (Z3.evalBool model) . (=<<) Z3.mkNot . Z3.mkEq emptySet) edgeConsts
                            BoolSet -> filterM (liftM fromJust . Z3.evalBool model) edgeConsts
            es <- mapM Z3.astToString satEdges
            ns <- mapM Z3.astToString satNodes
            liftIO $ putStrLn $ "Selected edges:"
            liftIO $ mapM putStrLn es
            liftIO $ putStrLn $ "Selected nodes:"
            liftIO $ mapM putStrLn ns
            
            satPath <- case edgeType of
                            BoolVar -> return $ satEdges ++ satNodes                            
                            IntSet -> do
                                edgesNonempty <- mapM ((=<<) Z3.mkNot . Z3.mkEq emptySet) satEdges
                                return $ edgesNonempty ++ satNodes
                            BoolSet -> return $ satEdges ++ satNodes
            Z3.mkAnd satPath >>= Z3.mkNot >>= Z3.optimizeAssert
        Unsat -> do
            liftIO $ putStrLn "Unsat"
            return ()
        Undef -> error "undefined error when running z3"

addGraphConstraints :: MonadZ3 z3 => SuccinctGraph -> SuccinctType -> [Id] -> EdgeType -> z3 ([AST], [AST])
addGraphConstraints graph goal args edgeType = do
    -- disable mbqi
    -- params <- Z3.mkParams
    -- symb <- Z3.mkStringSymbol "mbqi"
    -- Z3.paramsSetBool params symb False
    -- Z3.optimizeSetParams params
    
    let nodes = allNodes graph -- list of succinct types
    let edges = allEdges graph -- list of succinct edges
    -- liftIO $ print edges
    -- generate all related constraints
    boolS <- Z3.mkBoolSort
    intS <- Z3.mkIntSort
    intSet <- Z3.mkSetSort intS
    emptySet <- Z3.mkEmptySet intS

    nodeSyms <- mapM (Z3.mkStringSymbol . show) nodes
    nodeConsts <- mapM (\sym -> Z3.mkConst sym boolS) nodeSyms
    edgeSyms <- mapM (Z3.mkStringSymbol . getEdgeId) edges
    edgeWeights <- return $ map (show . getEdgeWeight) edges
    (edgeConsts, constWeights) <- case edgeType of 
                                    BoolVar -> (,) <$> (mapM (\sym -> Z3.mkConst sym boolS) edgeSyms) <*> return edgeWeights
                                    IntSet -> (,) <$> mapM (\sym -> Z3.mkConst sym intSet) edgeSyms <*> return edgeWeights
                                    BoolSet -> (,) <$> concatMapM (\e -> mapM (\arg -> Z3.mkStringSymbol (getEdgeId e ++ "_" ++ arg) >>= flip Z3.mkConst boolS) args) edges 
                                                   <*> return (concatMap (replicate $ length args) edgeWeights)

    -- add assertions to the optimization
    -- hard constraints for path connectivity
    mapM_ (nodeConstraint graph goal args edgeType) nodes
    mapM_ (edgeConstraint graph args edgeType . getEdgeId) edges    
    -- mapM_ Z3.optimizeAssert (concat nodeConts ++ edgeConts)

    -- hard constraints for contain both the goal node
    Z3.mkStringSymbol (show goal) >>= flip Z3.mkConst boolS >>= Z3.optimizeAssert
    -- all arg edges must non empty
    mustContainSyms <- mapM Z3.mkStringSymbol args
    mustContainConsts <- case edgeType of
                            BoolVar -> mapM (\sym -> Z3.mkConst sym boolS) mustContainSyms
                            IntSet -> mapM (\sym -> Z3.mkConst sym intSet >>= Z3.mkEq emptySet >>= Z3.mkNot) mustContainSyms
                            BoolSet -> mapM (\arg -> Z3.mkStringSymbol (arg ++ "_" ++ arg) >>= flip Z3.mkConst boolS) args
    mapM_ Z3.optimizeAssert mustContainConsts

    -- soft constraints for contain edges
    groupSym <- Z3.mkStringSymbol "path"
    mapM_ (\(ast, weight) -> do
        notAst <- case edgeType of
                    BoolVar -> Z3.mkNot ast
                    IntSet  -> Z3.mkEq emptySet ast
                    BoolSet -> Z3.mkNot ast
        Z3.optimizeAssertSoft notAst weight groupSym)
        $ zip edgeConsts constWeights

    -- hard constraints for containing compound nodes when the argument number is greater than 2
    when (length args > 1) (do
        compSyms <- mapM (Z3.mkStringSymbol . show) $ filter isSuccinctComposite nodes
        compConsts <- mapM (\sym -> Z3.mkConst sym boolS) compSyms
        Z3.mkOr compConsts >>= Z3.optimizeAssert)

    -- asserts <- Z3.optimizeGetAssertions >>= mapM Z3.astToString 
    -- liftIO $ mapM putStrLn asserts
    return (edgeConsts, nodeConsts)

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
allEdges graph = filter (not . isInfixOf "__goal__" . getEdgeId) $ 
    Set.toList . Set.unions . concat . map HashMap.elems $ HashMap.elems graph