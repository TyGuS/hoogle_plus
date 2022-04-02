module Hectare.TermSearch
  ( synthesize
  ) where

import           Control.Monad                  ( forM
                                                , msum
                                                , when
                                                )
import           Data.Bifunctor                 ( bimap
                                                , second
                                                )
import           Data.List                      ( (\\)
                                                , permutations
                                                )
import           Data.List.Extra                ( nubOrd )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                )
import           Data.Text                      ( Text )
import           Data.Tuple                     ( swap )
import           System.IO                      ( hFlush
                                                , stdout
                                                )

import           Data.ECTA
import           Data.ECTA.Paths
import           Data.ECTA.Term
import           Data.Text.Extended.Pretty
import           Utility.Fixpoint

import           Database.Dataset
import           Hectare.Type
import           Hectare.Utils
import           Types.Environment
import           Types.Program
import           Types.Type

--------------------------------------------------------------------------------
-------------------------------- Top level calls -------------------------------
--------------------------------------------------------------------------------

synthesize :: Goal -> IO [TProgram]
synthesize (Goal env goalType _) = do
  let args        = getArguments env
  let destination = lastType goalType
  let argNodes    = map (bimap Symbol typeToFta) args
  let resNode     = typeToFta destination
  concat <$> forM [1 ..] (doSynthesize argNodes resNode)

doSynthesize :: [Argument] -> Node -> Int -> IO [TProgram]
doSynthesize argNodes resNode sz = do
  let anyArg      = Node (map (uncurry constArg) argNodes)
  let !filterNode = filterType (relevantTermsOfSize anyArg argNodes sz) resNode
  reducedNode <- reduceFullyAndLog filterNode
  let foldedNode = refold reducedNode
  let terms      = getAllTerms foldedNode
  return $ map termToProgram terms

termToProgram :: Term -> TProgram
termToProgram (Term (Symbol x) []) = untyped (PSymbol x)
termToProgram (Term (Symbol f) xs) = untyped (PApp f (map termToProgram xs))

---------- Grouping

hoogleComponents :: Map TypeSkeleton Text
hoogleComponents = fst (mkGroups $ map (second toMonotype) hplusComponents)

groupMapping :: Map Text Text
groupMapping = snd (mkGroups $ map (second toMonotype) hplusComponents)

------------------------------------------------------------------------------

tau :: Node
tau = createMu
  (\n -> union
    (  [arrowType n n, var1, var2, var3, var4]
    ++ map (Node . (: []) . constructorToEdge n) usedConstructors
    )
  )
 where
  constructorToEdge :: Node -> (Text, Int) -> Edge
  constructorToEdge n (nm, arity) = Edge (Symbol nm) (replicate arity n)

  usedConstructors = allConstructors

allConstructors :: [(Text, Int)]
allConstructors =
  nubOrd (concatMap getConstructors (Map.keys hoogleComponents)) \\ [("Fun", 2)]
 where
  getConstructors :: TypeSkeleton -> [(Text, Int)]
  getConstructors (TypeVarT _) = []
  getConstructors (FunctionT _ t1 t2) =
    getConstructors t1 ++ getConstructors t2
  getConstructors (DatatypeT nm ts) =
    (nm, length ts) : concatMap getConstructors ts

generalize :: Node -> Node
generalize n@(Node [_]) = Node
  [mkEdge s ns' (mkEqConstraints $ map pathsForVar vars)]
 where
  vars                = [var1, var2, var3, var4, varAcc]
  nWithVarsRemoved    = mapNodes (\x -> if x `elem` vars then tau else x) n
  (Node [Edge s ns']) = nWithVarsRemoved

  pathsForVar :: Node -> [Path]
  pathsForVar v = pathsMatching (== v) n
generalize n = error $ "cannot generalize: " ++ show n

-- Use of `getPath (path [0, 2]) n1` instead of `tau` effectively pre-computes some reduction.
-- Sometimes this can be desirable, but for enumeration,
app :: Node -> Node -> Node
app n1 n2 = Node
  [ mkEdge
      "app"
      [tau, theArrowNode, n1, n2]
      (mkEqConstraints
        [ [path [1], path [2, 0, 0]]
        , [path [3, 0], path [2, 0, 1]]
        , [path [0], path [2, 0, 2]]
        ]
      )
  ]

typeToFta :: TypeSkeleton -> Node
typeToFta (TypeVarT "a"  ) = var1
typeToFta (TypeVarT "b"  ) = var2
typeToFta (TypeVarT "c"  ) = var3
typeToFta (TypeVarT "d"  ) = var4
typeToFta (TypeVarT "acc") = varAcc
typeToFta (TypeVarT v) =
  error
    $ "Current implementation only supports function signatures with type variables a, b, c, d, and acc, but got "
    ++ show v
typeToFta (FunctionT _ t1 t2) = arrowType (typeToFta t1) (typeToFta t2)
typeToFta (DatatypeT s ts) = mkDatatype s (map typeToFta ts)
typeToFta _ = error "typeToFta: cannot turn the type into an FTA"

speciallyTreatedFunctions :: [Text]
speciallyTreatedFunctions =
  [ -- `($)` is hardcoded to only be in argument position
    "(Data.Function.$)"
  ,
    -- `id` is almost entirely useless, but clogs up the graph. Currently banned
    "Data.Function.id"
  ]

--------------------------------------------------------------------------------
------------------------------- Relevancy Encoding -----------------------------
--------------------------------------------------------------------------------

applyOperator :: Node
applyOperator = Node
  [ constFunc
    "$"
    (generalize $ arrowType (arrowType var1 var2) (arrowType var1 var2))
  , constFunc "id" (generalize $ arrowType var1 var1)
  ]

hoogleComps :: [Edge]
hoogleComps =
  filter
      (\e ->
        edgeSymbol e
          `notElem` map (Symbol . toMappedName) speciallyTreatedFunctions
      )
    $ map (uncurry parseHoogleComponent . swap)
    $ Map.toList hoogleComponents

anyFunc :: Node
anyFunc = Node hoogleComps

filterType :: Node -> Node -> Node
filterType n t =
  Node [mkEdge "filter" [t, n] (mkEqConstraints [[path [0], path [1, 0]]])]

termsK :: Node -> Bool -> Int -> [Node]
termsK _      _     0 = []
termsK anyArg False 1 = [anyArg, anyFunc]
termsK anyArg True  1 = [anyArg, anyFunc, applyOperator]
termsK anyArg _ 2 =
  [ app anyListFunc (union [anyNonNilFunc, anyArg, applyOperator])
  , app fromJustFunc (union [anyNonNothingFunc, anyArg, applyOperator])
  , app (union [anyNonListFunc, anyArg]) (union (termsK anyArg True 1))
  ]
termsK anyArg _ k = map constructApp [1 .. (k - 1)]
 where
  constructApp :: Int -> Node
  constructApp i =
    app (union (termsK anyArg False i)) (union (termsK anyArg True (k - i)))

relevantTermK :: Node -> Bool -> Int -> [Argument] -> [Node]
relevantTermK anyArg includeApplyOp k []       = termsK anyArg includeApplyOp k
relevantTermK _      _              1 [(x, t)] = [Node [constArg x t]]
relevantTermK anyArg _ k argNames
  | k < length argNames = []
  | otherwise = concatMap (\i -> map (constructApp i) allSplits) [1 .. (k - 1)]
 where
  allSplits = map (`splitAt` argNames) [0 .. (length argNames)]

  constructApp :: Int -> ([Argument], [Argument]) -> Node
  constructApp i (xs, ys) =
    let f = union (relevantTermK anyArg False i xs)
        x = union (relevantTermK anyArg True (k - i) ys)
    in  app f x

relevantTermsOfSize :: Node -> [Argument] -> Int -> Node
relevantTermsOfSize anyArg args k =
  union $ concatMap (relevantTermK anyArg True k) (permutations args)

relevantTermsUptoK :: Node -> [Argument] -> Int -> Node
relevantTermsUptoK anyArg args k =
  union (map (relevantTermsOfSize anyArg args) [1 .. k])

prettyTerm :: Term -> Term
prettyTerm (Term "app" ns) = Term
  "app"
  [prettyTerm (ns !! (length ns - 2)), prettyTerm (ns !! (length ns - 1))]
prettyTerm (Term "filter" ns) = prettyTerm (last ns)
prettyTerm (Term s        _ ) = Term s []

dropTypes :: Node -> Node
dropTypes (Node es) = Node (map dropEdgeTypes es)
 where
  dropEdgeTypes (Edge "app" [_, _, a, b]) =
    Edge "app" [dropTypes a, dropTypes b]
  dropEdgeTypes (Edge "filter" [_, a]) = Edge "filter" [dropTypes a]
  dropEdgeTypes (Edge s        [_]   ) = Edge s []
  dropEdgeTypes e                      = e
dropTypes n = n

getText :: Symbol -> Text
getText (Symbol s) = s

--------------------------
-------- Remove uninteresting terms
--------------------------

fromJustFunc :: Node
fromJustFunc =
  Node $ filter (\e -> edgeSymbol e `elem` maybeFunctions) hoogleComps

maybeFunctions :: [Symbol]
maybeFunctions =
  [ "Data.Maybe.fromJust"
  , "Data.Maybe.maybeToList"
  , "Data.Maybe.isJust"
  , "Data.Maybe.isNothing"
  ]

listReps :: [Text]
listReps = map
  toMappedName
  [ "Data.Maybe.listToMaybe"
  , "Data.Either.lefts"
  , "Data.Either.rights"
  , "Data.Either.partitionEithers"
  , "Data.Maybe.catMaybes"
  , "GHC.List.head"
  , "GHC.List.last"
  , "GHC.List.tail"
  , "GHC.List.init"
  , "GHC.List.null"
  , "GHC.List.length"
  , "GHC.List.reverse"
  , "GHC.List.concat"
  , "GHC.List.concatMap"
  , "GHC.List.sum"
  , "GHC.List.product"
  , "GHC.List.maximum"
  , "GHC.List.minimum"
  , "(GHC.List.!!)"
  , "(GHC.List.++)"
  ]

isListFunction :: Symbol -> Bool
isListFunction (Symbol sym) = sym `elem` listReps

maybeReps :: [Text]
maybeReps = map
  toMappedName
  [ "Data.Maybe.maybeToList"
  , "Data.Maybe.isJust"
  , "Data.Maybe.isNothing"
  , "Data.Maybe.fromJust"
  ]

isMaybeFunction :: Symbol -> Bool
isMaybeFunction (Symbol sym) = sym `elem` maybeReps

anyListFunc :: Node
anyListFunc = Node $ filter (isListFunction . edgeSymbol) hoogleComps

anyNonListFunc :: Node
anyNonListFunc = Node $ filter
  (\e -> not (isListFunction (edgeSymbol e))
    && not (isMaybeFunction (edgeSymbol e))
  )
  hoogleComps

anyNonNilFunc :: Node
anyNonNilFunc =
  Node $ filter (\e -> edgeSymbol e /= Symbol (toMappedName "Nil")) hoogleComps

anyNonNothingFunc :: Node
anyNonNothingFunc = Node $ filter
  (\e -> edgeSymbol e /= Symbol (toMappedName "Data.Maybe.Nothing"))
  hoogleComps

--------------------------------------------------------------------------------

reduceFully :: Node -> Node
reduceFully = fixUnbounded (withoutRedundantEdges . reducePartially)

reduceFullyAndLog :: Node -> IO Node
reduceFullyAndLog = go 0
 where
  go :: Int -> Node -> IO Node
  go i n = do
    putStrLn
      $  "Round "
      ++ show i
      ++ ": "
      ++ show (nodeCount n)
      ++ " nodes, "
      ++ show (edgeCount n)
      ++ " edges"
    hFlush stdout
    -- putStrLn $ renderDot $ toDot n
    -- print n
    let n' = withoutRedundantEdges (reducePartially n)
    if n == n' || i >= 30 then return n else go (i + 1) n'

--------------------------
------ Util functions
--------------------------

toMappedName :: Text -> Text
toMappedName x = fromMaybe x (Map.lookup x groupMapping)

prettyPrintAllTerms :: Maybe Term -> Node -> IO ()
prettyPrintAllTerms mbSol n = do
  when (isJust mbSol) $ putStrLn $ "Expected: " ++ show
    (pretty $ fromJust mbSol)
  let ts = getAllTerms n
  print ts

substTerm :: Term -> Term
substTerm (Term (Symbol sym) ts) =
  Term (Symbol $ fromMaybe sym (Map.lookup sym groupMapping)) (map substTerm ts)

parseHoogleComponent :: Text -> TypeSkeleton -> Edge
parseHoogleComponent name t =
  constFunc (Symbol name) (generalize $ typeToFta t)
