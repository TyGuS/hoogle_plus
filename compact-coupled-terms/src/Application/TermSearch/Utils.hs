{-# LANGUAGE OverloadedStrings #-}

module Application.TermSearch.Utils where

import           Data.Map                     ( Map  )
import qualified Data.Map                    as Map
import           Data.Text                    ( Text )
import qualified Data.Text                   as Text

import           Data.ECTA
import           Data.ECTA.Paths
import           Data.ECTA.Term

import           Application.TermSearch.Type

--------------------------------------------------------------------------------
------------------------------- Type Constructors ------------------------------
--------------------------------------------------------------------------------

typeConst :: Text -> Node
typeConst s = Node [Edge (Symbol s) []]

constrType0 :: Text -> Node
constrType0 s = Node [Edge (Symbol s) []]

constrType1 :: Text -> Node -> Node
constrType1 s n = Node [Edge (Symbol s) [n]]

constrType2 :: Text -> Node -> Node -> Node
constrType2 s n1 n2 = Node [Edge (Symbol s) [n1, n2]]

maybeType :: Node -> Node
maybeType = constrType1 "Maybe"

listType :: Node -> Node
listType = constrType1 "List"

theArrowNode :: Node
theArrowNode = Node [Edge "(->)" []]

arrowType :: Node -> Node -> Node
arrowType n1 n2 = Node [Edge "->" [theArrowNode, n1, n2]]

appType :: Node -> Node -> Node
appType n1 n2 = Node [Edge "TyApp" [n1, n2]]

mkDatatype :: Text -> [Node] -> Node
mkDatatype s ns = Node [Edge (Symbol s) ns]

--------------------
------- Functions and arguments
--------------------

constFunc :: Symbol -> Node -> Edge
constFunc s t = Edge s [t]

constArg :: Symbol -> Node -> Edge
constArg = constFunc

var1, var2, var3, var4, varAcc :: Node
var1 = Node [Edge "var1" []]
var2 = Node [Edge "var2" []]
var3 = Node [Edge "var3" []]
var4 = Node [Edge "var4" []]
varAcc = Node [Edge "acc" []]

--------------------------------------------------------------------------------

--------------------
------- Component Grouping
--------------------

mkGroups :: [(Text, TypeSkeleton)] -> (Map TypeSkeleton Text, Map Text Text)
mkGroups [] = (Map.empty, Map.empty)
mkGroups ((name, typ):comps) = let (groups, nameToRepresentative) = mkGroups comps
                                   freshName = Text.pack ("f" <> show (Map.size groups))
                                in if typ `Map.member` groups 
                                  then (groups, Map.insert name (groups Map.! typ) nameToRepresentative)
                                  else (Map.insert typ freshName groups, Map.insert name freshName nameToRepresentative)

getRepOf :: [(Text, [Text])] -> Text -> Text
getRepOf [] fname = error $ "cannot find " ++ show fname ++ " in any group"
getRepOf ((x, fnames):xs) fname
  | fname `elem` fnames = x
  | otherwise = getRepOf xs fname


--------------------
------- Different cases of loops
--------------------

replicatorTau :: Node
replicatorTau = createMu
  (\n -> union
    ([var1, var2] ++ map (Node . (: []) . constructorToEdge n) usedConstructors)
  )
 where
  constructorToEdge :: Node -> (Text, Int) -> Edge
  constructorToEdge n (nm, arity) = Edge (Symbol nm) (replicate arity n)

  usedConstructors = [("Pair", 2)]

replicator :: Node
replicator = Node
  [ mkEdge
      "Pair"
      [ Node
        [ mkEdge "Pair"
                 [replicatorTau, replicatorTau]
                 (mkEqConstraints [[path [0, 0], path [0, 1], path [1]]])
        ]
      , Node [
        Edge "Pair" [replicatorTau, replicatorTau]]
      ]
      (mkEqConstraints [[path [0, 0], path [0, 1], path [1]]])
  ]

loop1 :: Node
loop1 = Node
  [ mkEdge
      "f"
      [ Node
          [ mkEdge
            "g"
            [ Node
                [ Edge
                    "h"
                    [ Node
                      [ Edge "Pair" [replicatorTau, replicatorTau]
                      , Edge "var2" []
                      ]
                    , Node [Edge "Pair" [replicatorTau, replicatorTau]]
                    ]
                ]
            ]
            (mkEqConstraints [[path [0, 0], path [0, 1, 0]]])
          , Edge "gg" [Node [Edge "Pair" [var2, var2]]]
          ]
      ]
      (mkEqConstraints [[path [0, 0, 0], path [0, 0, 1]]])
  ]

loop2 :: Node
loop2 = Node
  [ mkEdge
      "g"
      [ Node
        [ mkEdge "Pair"
                 [Node [Edge "List" [replicatorTau]], replicatorTau]
                 (mkEqConstraints [[path [0, 0], path [1]]])
        , Edge "f" [var1, Node [Edge "List" [var1]]]
        ]
      , Node
        [ mkEdge "Pair"
                 [Node [Edge "List" [replicatorTau]], replicatorTau]
                 (mkEqConstraints [[path [0], path [1]]])
        , Edge "f" [var1, var1]
        ]
      ]
      (mkEqConstraints
        [[path [0, 1, 0], path [1, 1]], [path [0, 0], path [1, 0]]]
      )
  ]
