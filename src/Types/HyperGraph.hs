module Types.HyperGraph where

import Types.Type

data VType = AndNode | OrNode | SingleNode
    deriving(Eq, Ord, Show)

data Vertex v = Vertex {
      vertexLabel :: v
    , vertexChildren :: [Vertex v]
    , vertexParents :: [Vertex v]
    , vertexType :: VType
    , vertexPrev :: [Vertex v]
    , vertexTruth :: [Vertex v]
} deriving(Eq, Ord, Show)

mkVertex :: v -> VType -> Vertex v
mkVertex l t = Vertex {
      vertexLabel = l
    , vertexChildren = []
    , vertexParents = []
    , vertexType = t
    , vertexPrev = []
    , vertexTruth = []
}

type Graph v = [Vertex v]