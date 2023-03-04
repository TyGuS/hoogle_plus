{-# LANGUAGE OverloadedStrings #-}

module Data.ECTA.Internal.ECTA.Visualization (
    toDot
  ) where

import qualified Data.Text as Text

import qualified Data.Graph.Inductive as Fgl
import Data.List.Index ( imap )
import qualified Language.Dot.Syntax as Dot


import Data.ECTA.Internal.ECTA.Operations ( maxIndegree, crush )
import Data.ECTA.Internal.ECTA.Type
import Data.ECTA.Internal.Paths ( EqConstraints )
import Data.ECTA.Internal.Term
import Data.Interned.Extended.HashTableBased ( Id )
import Data.Text.Extended.Pretty

---------------------------------------------------------------
----------------------- Visualization -------------------------
---------------------------------------------------------------

-----------------------
------ Partial graph
-----------------------

-- | We identify an edge by its /source/ node 'Id' and the index of the edge
type EdgeId = (Id, Int)

-- | Partial graph
--
-- This is used as an intermediate stage in rendering the graph: we 'crush' the graph, constructing a 'PartialGraph' at
-- every node in the graph, 'mappend' them all together and then construct an @fgl@ graph from that (which we then
-- export to @dotty@ format). This first step is independent from any @fgl@ or @dotty@ specific decisions.
data PartialGraph = PartialGraph {
      -- | IDs of all regular nodes in the graph
      partialNormal :: [Id]

      -- | IDs of all Mu nodes in the graph, along with the ID of their child
      --
      -- For now we explicitly assume that Mu nodes must have a regular node as a child node, and error out otherwise;
      -- see 'partialFromEdge' for motivation.
    , partialMu :: [(Id, Id)]

      -- | Edge nodes
    , partialEdges :: [(EdgeId, Symbol, EqConstraints)]

      -- | Transitions from nodes to edges
      --
      -- The 'Int' here is the index of the edge (i.e., the @i@th edge)
      --
      -- Invariant: The node 'Id' will be the Id of a /normal/ (non-Mu) node
    , partialFromNode :: [(Id, EdgeId)]

      -- | Transitions from edges to nodes
      --
      -- As for 'partialFromNode', the 'Int' is the index of the edge, but the node 'Id' here /might/ refer to a 'Mu'.
      -- This means that when rendering these edges, 'partialMu' should be taken into account: edges to Mu nodes should
      -- instead be rendered as edges to their regular 'Node' child (this motivates the assumption on 'partialMu').
    , partialFromEdge :: [(EdgeId, Id)]
    }
  deriving (Show)

instance Semigroup PartialGraph where
  a <> b = PartialGraph {
        partialNormal   = combine partialNormal
      , partialMu       = combine partialMu
      , partialEdges    = combine partialEdges
      , partialFromNode = combine partialFromNode
      , partialFromEdge = combine partialFromEdge
      }
    where
      combine :: Semigroup a => (PartialGraph -> a) -> a
      combine f = f a <> f b

instance Monoid PartialGraph where
  mempty = PartialGraph {
        partialNormal   = []
      , partialMu       = []
      , partialEdges    = []
      , partialFromNode = []
      , partialFromEdge = []
      }

mkPartialGraph :: Node -> PartialGraph
mkPartialGraph = crush onNode
  where
    onNode :: Node -> PartialGraph
    onNode EmptyNode           = error "mkPartialGraph: impossible (crush does not invoke function on EmptyNode)"
    onNode (InternedNode node) = let (edgeNodes, fr, to) = unzip3 $ imap (onEdge nid) es in
                                   mempty {
                                       partialNormal   = [nid]
                                     , partialEdges    = edgeNodes
                                     , partialFromNode = fr
                                     , partialFromEdge = concat to
                                     }
      where
        nid = internedNodeId    node
        es  = internedNodeEdges node
    onNode (InternedMu mu)     = case internedMuBody mu of
                                   InternedNode node -> mempty {
                                         partialMu = [(internedMuId mu, internedNodeId node)]
                                       }
                                   _otherwise         -> error "mkPartialGraph: expected Node as a child of a Mu"
    onNode (Rec _)             = mempty

    onEdge :: Id                                  -- Id of the " from " node
           -> Int                                 -- Index of the edge
           -> Edge                                -- The edge itself
           -> (  (EdgeId, Symbol, EqConstraints)  -- The edge node
              ,  (Id, EdgeId)                     -- The " from " transition
              , [(EdgeId, Id)]                    -- The " to   " transitions
              )
    onEdge nid i e = (
          (eid, edgeSymbol e, edgeEcs e)
        , (nid, eid)
        , map (\n -> (eid, nodeIdentity n)) $ edgeChildren e
        )
      where
        eid = (nid, i)

-----------------------
------ FGL graph construction
-----------------------

data FglNodeLabel = IdLabel Id | TransitionLabel Symbol EqConstraints
  deriving ( Eq, Ord, Show )

partialToFgl :: Int -> PartialGraph -> Fgl.Gr FglNodeLabel ()
partialToFgl maxNodeIndegree p =
    Fgl.mkGraph (nodeNodes ++ transitionNodes) (nodeToTransitionEdges ++ transitionToNodeEdges)
  where
    nodeNodes, transitionNodes :: [Fgl.LNode FglNodeLabel]
    nodeNodes       = map (\ i         -> (fglNodeId i, IdLabel $ i))          $ partialNormal p
    transitionNodes = map (\(i, s, cs) -> (fglEdgeId i, TransitionLabel s cs)) $ partialEdges  p

    nodeToTransitionEdges, transitionToNodeEdges :: [Fgl.LEdge ()]
    nodeToTransitionEdges = map (\(nid, eid) -> (fglNodeId nid, fglEdgeId  eid, ())) $ partialFromNode p
    transitionToNodeEdges = map (\(eid, nid) -> (fglEdgeId eid, fglNodeId' nid, ())) $ partialFromEdge p

    fglNodeId :: Id -> Fgl.Node
    fglNodeId nid = nid * (maxNodeIndegree + 1)

    -- " To " edges might transition to Mu nodes, in which case we want to an edge to their child node instead
    fglNodeId' :: Id -> Fgl.Node
    fglNodeId' nid = maybe (fglNodeId nid) fglNodeId (lookup nid $ partialMu p)

    fglEdgeId :: EdgeId -> Fgl.Node
    fglEdgeId (nid, i) = nid * (maxNodeIndegree + 1) + (i + 1)

toFgl :: Node -> Fgl.Gr FglNodeLabel ()
toFgl root = partialToFgl (maxIndegree root) (mkPartialGraph root)

-----------------------
------ Translate to dotty
-----------------------

fglToDot :: Fgl.Gr FglNodeLabel () -> Dot.Graph
fglToDot g = Dot.Graph Dot.StrictGraph Dot.DirectedGraph Nothing (nodeStmts ++ edgeStmts)
  where
    nodeStmts :: [Dot.Statement]
    nodeStmts = map renderNode  $ Fgl.labNodes g

    edgeStmts :: [Dot.Statement]
    edgeStmts = map renderEdge $ Fgl.labEdges g

    renderNode :: Fgl.LNode FglNodeLabel -> Dot.Statement
    renderNode (fglId, l) = Dot.NodeStatement (Dot.NodeId (Dot.IntegerId $ toInteger fglId) Nothing)
                                              [ Dot.AttributeSetValue (Dot.NameId "label") (renderNodeLabel l)
                                              , Dot.AttributeSetValue (Dot.NameId "shape")
                                                                      (case l of
                                                                        IdLabel _           -> Dot.StringId "ellipse"
                                                                        TransitionLabel _ _ -> Dot.StringId "box")
                                              ]

    renderEdge :: Fgl.LEdge () -> Dot.Statement
    renderEdge (a, b, _) = Dot.EdgeStatement [ea, eb] []
      where
        ea = Dot.ENodeId Dot.NoEdge       (Dot.NodeId (Dot.IntegerId $ toInteger a) Nothing)
        eb = Dot.ENodeId Dot.DirectedEdge (Dot.NodeId (Dot.IntegerId $ toInteger b) Nothing)

    renderNodeLabel :: FglNodeLabel -> Dot.Id
    renderNodeLabel (IdLabel l)             = Dot.StringId ("q" ++ show l)
    renderNodeLabel (TransitionLabel s ecs) =
         Dot.StringId (Text.unpack $ pretty s <> " (" <> pretty ecs <> ")")

-- | To visualize an FTA:
-- 1) Call `prettyPrintDot $ toDot fta` from GHCI
-- 2) Copy the output to viz-js.jom or another GraphViz implementation
toDot :: Node -> Dot.Graph
toDot = fglToDot . toFgl

