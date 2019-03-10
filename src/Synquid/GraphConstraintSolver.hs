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
import Data.Time.Clock

data EdgeType = BoolVar 
              | IntSet 
              | BoolSet -- encode X(e,arg) as a bool value which means the edge `e` comes from inhabited `arg`

-- * path constaints
-- constraints for each node
-- For a simple node: V ⇒ (E1 ∨ … ∨ En) for all outgoing edges and similar for incoming edges. 
-- For a compound node V ⇒ (E1 ∧ … ∧ En) for incoming edges.
nodeConstraint :: MonadZ3 z3 => SuccinctGraph -> SuccinctType -> [Id] -> EdgeType -> SuccinctType -> z3 ()
nodeConstraint graph goal args edgeType v = do
    let ins = allInOrOutEdges graph True v
    let outs = allInOrOutEdges graph False v
    let nodes = allNodes graph
    boolS <- Z3.mkBoolSort
    intS <- Z3.mkIntSort
    intSet <- Z3.mkSetSort intS
    -- inSyms <- mapM Z3.mkIntSymbol ins
    -- outSyms <- mapM Z3.mkIntSymbol outs
    inSyms <- mapM Z3.mkStringSymbol ins
    outSyms <- mapM Z3.mkStringSymbol outs
    -- vSym <- Z3.mkIntSymbol (fromJust $ elemIndex v $ allNodes graph)
    let vIdx = fromJust $ v `elemIndex` nodes
    let argSet = subsequences [0..(length args - 1)]
    vSyms <- mapM (Z3.mkStringSymbol . makeNodeName nodes v) argSet
    vConst <- mapM (\s -> Z3.mkConst s boolS) vSyms >>= Z3.mkOr

    case edgeType of
        BoolVar -> do
            inConsts <- mapM (\sym -> Z3.mkConst sym boolS) inSyms
            outConsts <- mapM (\sym -> Z3.mkConst sym boolS) outSyms
            when (length inConsts > 0) (do
                Z3.mkOr inConsts >>= Z3.mkImplies vConst >>= Z3.optimizeAssert
                let outNodes = allInOrOutNodes graph False v
                case v of
                    SuccinctComposite _ -> do
                        -- outgoes shall come from incomes, incomes shall come from 
                        inNout <- mapM (\arg -> do
                                    vArg <- Z3.mkStringSymbol (makeNodeName nodes v arg) >>= flip Z3.mkConst boolS
                                    let superArgSets = map (\a -> map ((:) a) $ subsequences (delete a [0..(length argSet - 1)])) arg
                                    outArgs <- mapM (\out -> mapM (\ss -> Z3.mkStringSymbol (makeNodeName nodes out ss) >>= flip Z3.mkConst boolS) superArgSets >>= Z3.mkOr) outNodes
                                    anyOutArg <- Z3.mkAnd outArgs
                                    Z3.mkImplies vArg anyOutArg
                                    ) argSet
                        Z3.mkAnd inNout >>= Z3.optimizeAssert
                    _ -> do
                        -- for simple nodes, the incoming and outgoing information shall be exactly the same
                        inNout <- concatMapM (\out -> 
                                    mapM (\arg -> do
                                        oArg <- Z3.mkStringSymbol (makeNodeName nodes out arg) >>= flip Z3.mkConst boolS
                                        vArg <- Z3.mkStringSymbol (makeNodeName nodes v   arg) >>= flip Z3.mkConst boolS
                                        Z3.mkImplies oArg vArg
                                        ) argSet
                                    ) outNodes
                        Z3.mkAnd inNout >>= Z3.optimizeAssert
                )
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
            when (length inConsts > 0)
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
    where
        checkEdgePair e1 e2 = do
            boolS <- Z3.mkBoolSort
            mapM_ (\arg -> do
                e1Argi <- Z3.mkStringSymbol (e1 ++ "_" ++ arg) >>= flip Z3.mkConst boolS
                e2Argi <- Z3.mkStringSymbol (e2 ++ "_" ++ arg) >>= flip Z3.mkConst boolS
                subsetImplies <- mapM (\argj -> do
                    e2Argj <- Z3.mkStringSymbol (e2 ++ "_" ++ argj) >>= flip Z3.mkConst boolS
                    e1Argj <- Z3.mkStringSymbol (e1 ++ "_" ++ argj) >>= flip Z3.mkConst boolS
                    Z3.mkImplies e2Argj e1Argj
                    ) $ delete arg args
                (flip (:) [e2Argi]) <$> Z3.mkAnd subsetImplies >>= Z3.mkOr >>= Z3.mkImplies e1Argi >>= Z3.optimizeAssert
                ) args
        subsetOrSuperset s1 s2 = do
            r1 <- Z3.mkSetSubset s1 s2
            r2 <- Z3.mkSetSubset s2 s1
            Z3.mkOr [r1, r2]

dropConstraint :: MonadZ3 z3 => SuccinctGraph -> [Id] -> SuccinctType -> z3 ()
dropConstraint graph args v = do
    let nodes = allNodes graph
    let ins = allInOrOutEdges graph True v
    boolS <- Z3.mkBoolSort
    -- inSyms <- mapM Z3.mkIntSymbol ins
    let vIdx = fromJust $ v `elemIndex` nodes
    let argSet = subsequences [0..(length args - 1)]
    vSyms <- mapM (Z3.mkStringSymbol . makeNodeName nodes v) argSet
    vConst <- mapM (\s -> Z3.mkConst s boolS) vSyms >>= Z3.mkOr
    inSyms <- mapM Z3.mkStringSymbol ins
    -- return ()
    -- vSym <- Z3.mkIntSymbol (fromJust $ elemIndex v $ allNodes graph)
    -- vConst <- Z3.mkConst vSym boolS
    inConsts <- mapM (\sym -> Z3.mkConst sym boolS) inSyms
    when (length inConsts > 0) (Z3.mkOr inConsts >>= flip Z3.mkImplies vConst >>= Z3.optimizeAssert)

-- For every edge: E ⇒ (V1 ∧ V2) for its start and end node. 
-- edgeConstraint :: MonadZ3 z3 => SuccinctGraph -> [Id] -> EdgeType -> (Int, Int, Id) -> (Int, Double) -> z3 ()
-- edgeConstraint graph args edgeType (from, to, e) (eidx, _) = do
--     let nodes = [from, to]
--     -- liftIO $ print e
--     -- liftIO $ print nodes
--     boolS <- Z3.mkBoolSort
--     intS <- Z3.mkIntSort
--     intSet <- Z3.mkSetSort intS
--     edgeSym <- Z3.mkIntSymbol eidx
--     nodeSyms <- mapM Z3.mkIntSymbol nodes
--     nodeConsts <- mapM (\sym -> Z3.mkConst sym boolS) nodeSyms
--     nodeCond <- Z3.mkAnd nodeConsts

--     case edgeType of
--         BoolVar -> do
--             edgeConst <- Z3.mkConst edgeSym boolS
--             Z3.mkImplies edgeConst nodeCond >>= Z3.optimizeAssert
--         IntSet -> do
--             edgeConst <- Z3.mkConst edgeSym intSet
--             edgeNonempty <- Z3.mkEmptySet intS >>= Z3.mkEq edgeConst >>= Z3.mkNot
--             Z3.mkImplies edgeNonempty nodeCond >>= Z3.optimizeAssert
edgeConstraint :: MonadZ3 z3 => SuccinctGraph -> [Id] -> EdgeType -> Id -> z3 ()
edgeConstraint graph args edgeType e = do
    let nodes = verticesOf graph e
    boolS <- Z3.mkBoolSort
    intS <- Z3.mkIntSort
    intSet <- Z3.mkSetSort intS
    edgeSym <- Z3.mkStringSymbol e
    -- nodeSyms <- mapM Z3.mkIntSymbol nodes
    let argSet = subsequences [0..(length args - 1)]
    nodeConsts <- mapM (\v -> mapM (Z3.mkStringSymbol . makeNodeName nodes v) argSet >>= mapM (flip Z3.mkConst boolS) >>= Z3.mkOr) nodes
    -- nodeConsts <- mapM (\sym -> Z3.mkConst sym boolS) nodeSyms
    nodeCond <- Z3.mkAnd nodeConsts

    case edgeType of
        BoolVar -> do
            edgeConst <- Z3.mkConst edgeSym boolS
            Z3.mkImplies edgeConst nodeCond >>= Z3.optimizeAssert
        IntSet -> do
            edgeConst <- Z3.mkConst edgeSym intSet
            edgeNonempty <- Z3.mkEmptySet intS >>= Z3.mkEq edgeConst >>= Z3.mkNot
            Z3.mkImplies edgeNonempty nodeCond >>= Z3.optimizeAssert
    

getPathSolution :: MonadZ3 z3 => SuccinctGraph -> [AST] -> [AST] -> EdgeType -> z3 ()
getPathSolution graph edgeConsts nodeConsts edgeType = do
    -- print the results
    liftIO $ putStrLn $ "Searching for the next path..."
    start <- liftIO $ getCurrentTime
    res <- Z3.optimizeCheck
    end <- liftIO $ getCurrentTime
    liftIO $ print $ diffUTCTime end start
    case res of
        Sat -> do
            liftIO $ putStrLn "Sat"
            model <- Z3.optimizeGetModel
            -- str <- Z3.modelToString model
            -- liftIO $ putStrLn str 
            -- liftIO $ putStrLn "======================="
            intS <- Z3.mkIntSort
            emptySet <- Z3.mkEmptySet intS

            satNodes <- filterM (liftM fromJust . Z3.evalBool model) nodeConsts
            -- satNodes <- filterM (checkNode model) (allNodes graph)
            satEdges <- case edgeType of
                            BoolVar -> filterM (liftM fromJust . Z3.evalBool model) edgeConsts
                            IntSet -> filterM (liftM fromJust . (=<<) (Z3.evalBool model) . (=<<) Z3.mkNot . Z3.mkEq emptySet) edgeConsts
                            BoolSet -> filterM (liftM fromJust . Z3.evalBool model) edgeConsts
            -- satEdges <- filterM (checkEdge model) (HashMap.toList $ allEdges graph)
            edgeStrs <- mapM Z3.astToString satEdges
            nodeStrs <- mapM Z3.astToString satNodes
            let edgeNames = map (getEdgeId . getEdge) edgeStrs
            liftIO $ putStrLn $ "Selected edges:"
            liftIO $ mapM_ putStrLn edgeNames
            liftIO $ putStrLn $ "Selected nodes:"
            liftIO $ mapM_ putStrLn nodeStrs
            -- assrts <- Z3.optimizeGetAssertions >>= mapM Z3.astToString 
            -- liftIO $ mapM putStrLn assrts
            -- error "Stop!"
            satPath <- case edgeType of
                            BoolVar -> return $ satEdges ++ satNodes                            
                            IntSet -> do
                                edgesNonempty <- mapM ((=<<) Z3.mkNot . Z3.mkEq emptySet) satEdges
                                return $ edgesNonempty ++ satNodes
                            BoolSet -> return $ satEdges ++ satNodes
            Z3.mkAnd satPath >>= Z3.mkNot >>= Z3.optimizeAssert
            return ()
        Unsat -> do
            liftIO $ putStrLn "Unsat"
            return ()
        Undef -> error "undefined error when running z3"
    where
        checkNode model node = do
            let nodes = allNodes graph
            let idx = fromJust $ elemIndex node nodes
            boolS <- Z3.mkBoolSort
            liftM fromJust $ Z3.mkIntSymbol idx >>= flip Z3.mkConst boolS >>= Z3.evalBool model
        checkEdge model edge = do
            boolS <- Z3.mkBoolSort
            liftM fromJust $ Z3.mkIntSymbol (fst $ snd edge) >>= flip Z3.mkConst boolS >>= Z3.evalBool model
        -- getEdge e = let edges = allEdges graph
        --             in HashMap.foldrWithKey (\(_,_,id) (idx,_) acc -> if idx == e then id else acc) "NOT FOUND" edges
        getEdge e = let fidx : tidx : eidx : _ = parseEdgeName e
                        nodes = allNodes graph
                        from = nodes !! fidx
                        to = nodes !! tidx
                    in Set.elemAt eidx $ HashMap.lookupDefault Set.empty to $ HashMap.lookupDefault HashMap.empty from graph

addGraphConstraints :: MonadZ3 z3 => SuccinctGraph -> SuccinctType -> [Id] -> EdgeType -> z3 ([AST], [AST])
addGraphConstraints graph goal args edgeType = do
    start <- liftIO $ getCurrentTime
    -- disable mbqi
    -- params <- Z3.mkParams
    -- symb <- Z3.mkStringSymbol "mbqi"
    -- Z3.paramsSetBool params symb False
    -- Z3.optimizeSetParams params
    
    let nodes = allNodes graph -- list of succinct types
    let (edges, constWeights) = unzip $ allEdges graph -- again change back to string //-- hashmap from succinct edges to their weights and indices
    -- liftIO $ print edges
    -- generate all related constraints
    boolS <- Z3.mkBoolSort
    intS <- Z3.mkIntSort
    intSet <- Z3.mkSetSort intS
    emptySet <- Z3.mkEmptySet intS

    let argSet = map (map show) $ subsequences [0..(length args - 1)]
    nodeSyms <- concatMapM (\v -> mapM (Z3.mkStringSymbol . makeNodeName nodes v) argSet) nodes
    nodeConsts <- mapM (\sym -> Z3.mkConst sym boolS) nodeSyms
    -- let edgeIndices = map fst $ HashMap.elems edges
    -- edgeSyms <- mapM Z3.mkIntSymbol edgeIndices
    edgeSyms <- mapM Z3.mkStringSymbol edges
    edgeConsts <- case edgeType of
                    BoolVar -> mapM (\sym -> Z3.mkConst sym boolS) edgeSyms
                    IntSet -> mapM (\sym -> Z3.mkConst sym intSet) edgeSyms
    -- let constWeights = map (show . snd) $ HashMap.elems edges

    -- add assertions to the optimization
    -- hard constraints for path connectivity
    -- mapM_ (uncurry (edgeConstraint graph args edgeType)) $ HashMap.toList edges
    mapM_ (edgeConstraint graph args edgeType) edges
    mapM_ (nodeConstraint graph goal args edgeType) nodes

    -- soft constraints for contain edges
    groupSym <- Z3.mkStringSymbol "path"
    mapM_ (\(ast, weight) -> do
        notAst <- case edgeType of
                    BoolVar -> Z3.mkNot ast
                    IntSet  -> Z3.mkEq emptySet ast
        Z3.optimizeAssertSoft notAst weight groupSym)
        $ zip edgeConsts $ map show constWeights

    Z3.mkStringSymbol (makeNodeName nodes goal [0..(length args-1)]) >>= flip Z3.mkConst boolS >>= Z3.optimizeAssert
    dropConstraint graph args goal
    -- all arg edges must non empty
    mustContainSyms <- mapM (Z3.mkStringSymbol . toEdgeSymbol graph) args
    mustContainConsts <- case edgeType of
                            BoolVar -> mapM (\sym -> Z3.mkConst sym boolS) mustContainSyms
                            IntSet -> mapM (\sym -> Z3.mkConst sym intSet >>= Z3.mkEq emptySet >>= Z3.mkNot) mustContainSyms
                            BoolSet -> mapM (\arg -> Z3.mkStringSymbol (arg ++ "_" ++ arg) >>= flip Z3.mkConst boolS) args
    mapM_ Z3.optimizeAssert mustContainConsts
    end <- liftIO $ getCurrentTime
    liftIO $ putStrLn "Time for adding constraints"
    liftIO $ print $ diffUTCTime end start

    -- First, define the function partition
    -- Then, define the function thereIsPath, this function is recursive, but I believe we may unroll the recursive function
    -- Now we find several segments for our hyperpath and we work with two parameters first
    -- From goal to some compound node
    -- assert a partition of the types in the compound node
    -- assert there is a path from the partition to some argument
    -- This is only going to work with BoolVar edges
    -- constraints <- mapM (\comp -> do
    --     let SuccinctComposite tys = comp
    --     -- encode the types in a compound node as a set of integers
    --     let tyIdx = map (fromJust . flip elemIndex nodes) $ Set.toList tys
    --     tySet <- foldM (\s t -> Z3.mkIntNum t >>= Z3.mkSetAdd s) emptySet tyIdx
    --     -- encode the args into another set of integers
    --     let argIdx = map (toEdgeSymbol graph) args
    --     argSet <- foldM (\s argi -> Z3.mkIntNum argi >>= Z3.mkSetAdd s) emptySet argIdx
    --     -- assert there exists a partition of the set
    --     -- symbols to be quantified in the exists clause
    --     boundSyms <- mapM Z3.mkStringSymbol ["x","x'","y","y'"]
    --     boundSets <- mapM (flip Z3.mkConst intSet) boundSyms
    --     let (tyVars, argVars) = splitAt 2 boundSets
    --     emptyInterTy <- Z3.mkSetIntersect tyVars >>= Z3.mkEq emptySet
    --     fullUnionTy <- Z3.mkSetUnion tyVars >>= Z3.mkEq tySet
    --     emptyInterArg <- Z3.mkSetIntersect argVars >>= Z3.mkEq emptySet
    --     fullUnionArg <- Z3.mkSetUnion argVars >>= Z3.mkEq argSet

    --     tyImpls <- mapM (\ti -> do
    --         selected0 <- Z3.mkIntNum ti >>= Z3.mkSelect (boundSets !! 0)
    --         selected1 <- Z3.mkIntNum ti >>= Z3.mkSelect (boundSets !! 1)
    --         toType <- Z3.mkIntSymbol ti >>= flip Z3.mkConst boolS
    --         impl1 <- Z3.mkImplies selected0 toType
    --         impl2 <- Z3.mkImplies selected1 toType
    --         return [impl1, impl2]) tyIdx
    --     argImpls <- mapM (\argi -> do
    --         selected0 <- Z3.mkIntNum argi >>= Z3.mkSelect (boundSets !! 2)
    --         selected1 <- Z3.mkIntNum argi >>= Z3.mkSelect (boundSets !! 3)
    --         toType <- Z3.mkIntSymbol argi >>= flip Z3.mkConst boolS
    --         impl1 <- Z3.mkImplies selected0 toType
    --         impl2 <- Z3.mkImplies selected1 toType
    --         return [impl1, impl2]) argIdx
    --     -- TODO: some debugging code
    --     -- strArg1 <- Z3.astToString arg1
    --     -- liftIO $ putStrLn strArg1
    --     -- strArg2 <- Z3.astToString arg2
    --     -- liftIO $ putStrLn strArg2
    --     Z3.mkAnd ([emptyInterTy, fullUnionTy, emptyInterArg, fullUnionArg]++concat tyImpls ++ concat argImpls) >>= Z3.mkExists [] boundSyms (replicate 4 intSet)

    --     -- liftIO $ putStrLn $ "Trying the compound node " ++ show comp
    --     -- consider all the possible partitions for this set
    --     -- let partitions = if Set.size tys == 1 
    --     --                 then [replicate (length args) (Set.findMin tys)]
    --     --                 else permutations $ Set.toList tys
    --     -- mapM_ (\partition -> do
    --     --     -- s0 for arg0, s1 for arg1, remains for finding paths to other inhabited nodes
    --     --     -- each partition should have at least the number of args elmts in them
    --     --     let (topTier, btmTier) = splitAt (length args) partition
    --     --     -- for the first (length args) tys, find them to a corresponding argument
    --     --     mapM_ (\(arg, ty) -> do
    --     --         start <- liftIO $ getCurrentTime
    --     --         Z3.optimizePush
    --     --         -- set the including of the goal type
    --     --         dropConstraint graph ty
    --     --         Z3.mkIntSymbol (fromJust $ ty `elemIndex` nodes) >>= flip Z3.mkConst boolS >>= Z3.optimizeAssert
    --     --         Z3.mkStringSymbol (toEdgeSymbol graph arg) >>= flip Z3.mkConst boolS >>= Z3.optimizeAssert
    --     --         getPathSolution graph edgeConsts nodeConsts edgeType
    --     --         Z3.optimizePop 1
    --     --         end <- liftIO $ getCurrentTime
    --     --         liftIO $ print $ diffUTCTime end start
    --     --         ) $ zip args topTier
    --     --     -- for the rest of them, find a path that includes them
    --     --     mapM_ (\ty -> do
    --     --         Z3.optimizePush
    --     --         Z3.mkStringSymbol (show ty) >>= flip Z3.mkConst boolS >>= Z3.optimizeAssert
    --     --         getPathSolution graph edgeConsts nodeConsts edgeType
    --     --         Z3.optimizePop 1
    --     --         ) btmTier
    --     --     ) partitions
    --     ) $ filter isSuccinctComposite nodes
    -- Z3.mkOr constraints >>= Z3.optimizeAssert
    
    -- hard constraints for containing compound nodes when the argument number is greater than 2
    -- when (length args > 1) (do
    --     compSyms <- mapM (Z3.mkStringSymbol . show) $ filter isSuccinctComposite nodes
    --     compConsts <- mapM (\sym -> Z3.mkConst sym boolS) compSyms
    --     Z3.mkOr compConsts >>= Z3.optimizeAssert)

    -- asserts <- Z3.optimizeGetAssertions >>= mapM Z3.astToString 
    -- liftIO $ mapM putStrLn asserts
    return (edgeConsts, nodeConsts)

-- util functions
-- inOrOut -> in :: True, out :: False
allInOrOutEdges graph inOrOut v =
    HashMap.foldrWithKey (\from m res -> 
        HashMap.foldrWithKey (\to set res' -> 
            if (if inOrOut then to == v else from == v) 
                -- then let edges = allEdges graph 
                --          nodes = allNodes graph
                     -- in (++) res' $ map (fst . \e -> HashMap.lookupDefault (0,0.0) (fromJust $ from `elemIndex` nodes, fromJust $ to `elemIndex` nodes, getEdgeId e) edges) 
                     --              $ Set.toList $ Set.filter (not . isInfixOf "__goal__" . getEdgeId) set 
                then (++) res' $ map (\e -> buildEdgeName (allNodes graph) from to set e)
                               $ Set.toList $ Set.filter (not . isInfixOf "__goal__" . getEdgeId) set 
                else res') res m
        ) [] graph

allInOrOutNodes graph inOrOut v =
    HashMap.foldrWithKey (\from m res -> 
        HashMap.foldrWithKey (\to set res' -> 
            if (if inOrOut then to == v else from == v)
                then (if inOrOut then to else from):res'
                else res') res m
        ) [] graph

toEdgeSymbol graph name = 
    HashMap.foldrWithKey (\from m res -> 
        HashMap.foldrWithKey (\to set res' -> 
            if name `Set.member` (Set.map getEdgeId set) 
                then buildEdgeName (allNodes graph) from to (Set.map getEdgeId set) name
                     -- in fst $ HashMap.lookupDefault (0, 0.0) (fromJust $ from `elemIndex` nodes, fromJust $ to `elemIndex` nodes, name) edges
                else res') res m
        ) "" graph

-- | get the from and to nodes for an edge
verticesOf graph e = take 2 $ parseEdgeName e

-- build edge name
buildEdgeName nodes from to edges e = 
    "e_" ++
    show (fromJust $ from `elemIndex` nodes) ++ "_" ++
    show (fromJust $ to `elemIndex` nodes) ++ "_" ++
    show (Set.findIndex e edges)

makeNodeName nodes v args = 
    "v_" ++
    show (fromJust $ v `elemIndex` nodes) ++ "_" ++
    (intercalate "_" $ map show args)

parseEdgeName name = map (fromIntegral . fromJust . asInteger) $ splitBy '_' $ drop 2 name

-- list of nodes
-- in SMT solver, we index all the nodes by their positions in the nodes list as v_idx
allNodes graph = nub $ HashMap.keys graph ++ (concat . HashMap.elems $ HashMap.map HashMap.keys graph)
-- list of edges
-- in SMT solver, we index all the edges by e_{v_from}_{v_to}_{position_in_edge_set}, here we get both its name and weight
-- This is a map from each edge to its weight and index
-- allEdges graph = 
--     let nodes = allNodes graph -- get nodes in the graph
--         (_, res) = HashMap.foldrWithKey (\from m acc -> 
--             HashMap.foldrWithKey (\to set acc' -> 
--                 Set.foldr (\e (i, hm) -> 
--                     (i+1, HashMap.insert (fromJust $ from `elemIndex` nodes, fromJust $ to `elemIndex` nodes, getEdgeId e) (i, getEdgeWeight e) hm)
--                     ) acc' $ Set.filter (not . isInfixOf "__goal__" . getEdgeId) set
--                 ) acc m) (length nodes, HashMap.empty) graph -- here edge indices starting after that for the nodes
--     in res
allEdges graph = 
    let nodes = allNodes graph in
    HashMap.foldrWithKey (\from m acc -> 
        HashMap.foldrWithKey (\to set acc' ->
            (++) acc' $ Set.toList 
                      $ Set.map (\e -> (buildEdgeName nodes from to set e, getEdgeWeight e)) set
            ) acc m
        ) [] graph