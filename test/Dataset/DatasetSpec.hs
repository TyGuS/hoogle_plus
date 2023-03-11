module Dataset.DatasetSpec
  ( spec
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text

import Test.Hspec ( Spec, describe, it, shouldBe)
import Pipes
import qualified Pipes.Prelude as P

import Types.Type
import Types.Pretty
import Types.Program
import Types.Common
import Dataset.Generation
import Dataset.Dataset
import Dataset.HyperGraph

maybeType :: TypeSkeleton -> TypeSkeleton
maybeType t = DatatypeT "Maybe" [t]

data GenAppTestcase = GenAppTestcase {
  genAppDesc :: String,
  genAppGraph :: HyperGraph,
  genAppWantGraph :: HyperGraph
}

appTestcases :: [GenAppTestcase]
appTestcases = [
  GenAppTestcase
    "generate apps with available arguments"
    (Map.fromList [ (FunctionT "xs" (listType $ vart "a") (maybeType $ vart "a"), Set.fromList [Edge "listToMaybe" []])
                  , (listType boolType, Set.fromList [Edge "bs" []])
                  ])
    (Map.fromList [ (FunctionT "xs" (listType $ vart "a") (maybeType $ vart "a"), Set.fromList [Edge "listToMaybe" []])
                  , (listType boolType, Set.fromList [Edge "bs" []])
                  , (maybeType boolType, Set.fromList [Edge "listToMaybe" [listType boolType]])
                  ]),

  GenAppTestcase
    "generate apps with two available arguments"
    (Map.fromList [ (FunctionT "xs" (listType $ vart "a") (maybeType $ vart "a"), Set.fromList [Edge "listToMaybe" []])
                  , (listType (maybeType boolType), Set.fromList [Edge "xs" []])
                  , (listType boolType, Set.fromList [Edge "bs" [], Edge "catMaybes" [listType (maybeType boolType)]])
                  ])
    (Map.fromList [ (FunctionT "xs" (listType $ vart "a") (maybeType $ vart "a"), Set.fromList [Edge "listToMaybe" []])
                  , (listType (maybeType boolType), Set.fromList [Edge "xs" []])
                  , (listType boolType, Set.fromList [Edge "bs" [], Edge "catMaybes" [listType (maybeType boolType)]])
                  , (maybeType boolType, Set.fromList [Edge "listToMaybe" [listType boolType]])
                  , (maybeType (maybeType boolType), Set.fromList [Edge "listToMaybe" [listType (maybeType boolType)]])
                  ]),

  GenAppTestcase
    "generate apps for two functions"
    (Map.fromList [ (FunctionT "xs" (listType $ vart "a") (maybeType $ vart "a"), Set.fromList [Edge "listToMaybe" []])
                  , (FunctionT "xs" (listType $ vart "a") (listType $ vart "b"), Set.fromList [Edge "map" [FunctionT "" (vart "a") (vart "b")]])
                  , (listType boolType, Set.fromList [Edge "bs" [], Edge "g" []])
                  ])
    (Map.fromList [ (FunctionT "xs" (listType $ vart "a") (maybeType $ vart "a"), Set.fromList [Edge "listToMaybe" []])
                  , (FunctionT "xs" (listType $ vart "a") (listType $ vart "b"), Set.fromList [Edge "map" [FunctionT "" (vart "a") (vart "b")]])
                  , (listType boolType, Set.fromList [Edge "bs" [], Edge "g" []])
                  , (maybeType boolType, Set.fromList [Edge "listToMaybe" [listType boolType]])
                  , (listType $ vart "t0", Set.fromList [Edge "map" [FunctionT "" (vart "a") (vart "b"), listType boolType]])
                  ]),

  GenAppTestcase
    "generate apps for two functions"
    (Map.fromList [ (FunctionT "xs" (listType $ vart "t0") (maybeType $ vart "t0"), Set.fromList [Edge "listToMaybe" []])
                  , (FunctionT "xs" (listType $ vart "t0") (listType $ vart "t1"), Set.fromList [Edge "map" [FunctionT "" (vart "t0") (vart "t1")]])
                  , (vart "t0", Set.fromList [Edge "bs" [], Edge "g" []])
                  ])
    (Map.fromList [ (FunctionT "xs" (listType $ vart "t0") (maybeType $ vart "t0"), Set.fromList [Edge "listToMaybe" []])
                  , (FunctionT "xs" (listType $ vart "t0") (listType $ vart "t1"), Set.fromList [Edge "map" [FunctionT "" (vart "t0") (vart "t1")]])
                  , (vart "t0", Set.fromList [Edge "bs" [], Edge "g" []])
                  , (maybeType $ vart "t0", Set.fromList [Edge "listToMaybe" [listType $ vart "t0"]])
                  , (listType $ vart "t0", Set.fromList [Edge "map" [FunctionT "" (vart "t0") (vart "t1"), listType $ vart "t0"]])
                  ])
  ]

data GenTestcase = GenTestcase {
  genDesc :: String,
  genRepeat :: Int,
  genComponents :: [(Text, SchemaSkeleton)],
  genWantNum :: Int,
  genWantStr :: [String]
}

testComponents :: [(Text, SchemaSkeleton)]
testComponents = [
    ( "Nil", ForallT "a" (Monotype (DatatypeT "List" [vart "a"])))
  , ( "Nothing"
    , ForallT "a" (Monotype (maybeType $ vart "a"))
    )
  , ( "catMaybes"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg343"
                  (DatatypeT "List" [maybeType $ vart "a"])
                  (DatatypeT "List" [vart "a"])
        )
      )
    )
  , ( "fromMaybe"
    , ForallT
      "a"
      (Monotype
        (FunctionT
          "arg339"
          (vart "a")
          (FunctionT "arg340" (maybeType $ vart "a") (vart "a"))
        )
      )
    )
  , ( "listToMaybe"
    , ForallT
      "a"
      (Monotype
        (FunctionT "arg341"
                  (DatatypeT "List" [vart "a"])
                  (maybeType $ vart "a")
        )
      )
    )
  ]

genTestcases :: [GenTestcase]
genTestcases = [
  GenTestcase
    "generate once with five components"
    1
    testComponents
    19
    [ "[]"
    , "str"
    , "c"
    , "i"
    , "Nothing"
    , "catMaybes"
    , "fromMaybe"
    , "listToMaybe"
    , "fromMaybe []"
    , "fromMaybe Nothing"
    , "fromMaybe catMaybes"
    , "fromMaybe fromMaybe"
    , "fromMaybe listToMaybe"
    , "fromMaybe str"
    , "fromMaybe c"
    , "fromMaybe i"
    , "listToMaybe []"
    , "listToMaybe str"
    , "catMaybes []"
    ],

  GenTestcase
    "generate twice with five components"
    2
    testComponents
    50
    [ "[]"
    , "str"
    , "c"
    , "i"
    , "Nothing"
    , "catMaybes"
    , "fromMaybe"
    , "listToMaybe"
    , "fromMaybe []"
    , "fromMaybe Nothing"
    , "fromMaybe catMaybes"
    , "fromMaybe fromMaybe"
    , "fromMaybe listToMaybe"
    , "fromMaybe str"
    , "fromMaybe c"
    , "fromMaybe i"
    , "listToMaybe []"
    , "listToMaybe str"
    , "catMaybes []"
    , "fromMaybe [] Nothing"
    , "fromMaybe Nothing Nothing"
    , "fromMaybe catMaybes Nothing"
    , "fromMaybe fromMaybe Nothing"
    , "fromMaybe listToMaybe Nothing"
    , "fromMaybe str Nothing"
    , "fromMaybe c Nothing"
    , "fromMaybe i Nothing"
    , "fromMaybe [] (listToMaybe [])"
    , "fromMaybe Nothing (listToMaybe [])"
    , "fromMaybe catMaybes (listToMaybe [])"
    , "fromMaybe fromMaybe (listToMaybe [])"
    , "fromMaybe listToMaybe (listToMaybe [])"
    , "fromMaybe str (listToMaybe [])"
    , "fromMaybe c (listToMaybe [])"
    , "fromMaybe i (listToMaybe [])"
    , "fromMaybe c (listToMaybe str)"
    , "fromMaybe (fromMaybe [])"
    , "fromMaybe (fromMaybe Nothing)"
    , "fromMaybe (fromMaybe catMaybes)"
    , "fromMaybe (fromMaybe fromMaybe)"
    , "fromMaybe (fromMaybe listToMaybe)"
    , "fromMaybe (fromMaybe str)"
    , "fromMaybe (fromMaybe c)"
    , "fromMaybe (fromMaybe i)"
    , "fromMaybe (listToMaybe [])"
    , "fromMaybe (listToMaybe str)"
    , "fromMaybe (catMaybes [])"
    , "listToMaybe (catMaybes [])"
    , "catMaybes (catMaybes [])"
    ],

  GenTestcase
    "generate three times with five components"
    3
    testComponents
    309
    [ "[]"
    , "str"
    , "c"
    , "i"
    , "Nothing"
    , "catMaybes"
    , "fromMaybe"
    , "listToMaybe"
    , "fromMaybe []"
    , "fromMaybe Nothing"
    , "fromMaybe catMaybes"
    , "fromMaybe fromMaybe"
    , "fromMaybe listToMaybe"
    , "fromMaybe str"
    , "fromMaybe c"
    , "fromMaybe i"
    , "listToMaybe []"
    , "listToMaybe str"
    , "catMaybes []"
    , "fromMaybe [] Nothing"
    , "fromMaybe Nothing Nothing"
    , "fromMaybe catMaybes Nothing"
    , "fromMaybe fromMaybe Nothing"
    , "fromMaybe listToMaybe Nothing"
    , "fromMaybe str Nothing"
    , "fromMaybe c Nothing"
    , "fromMaybe i Nothing"
    , "fromMaybe [] (listToMaybe [])"
    , "fromMaybe Nothing (listToMaybe [])"
    , "fromMaybe catMaybes (listToMaybe [])"
    , "fromMaybe fromMaybe (listToMaybe [])"
    , "fromMaybe listToMaybe (listToMaybe [])"
    , "fromMaybe str (listToMaybe [])"
    , "fromMaybe c (listToMaybe [])"
    , "fromMaybe i (listToMaybe [])"
    , "fromMaybe c (listToMaybe str)"
    , "fromMaybe (fromMaybe [])"
    , "fromMaybe (fromMaybe Nothing)"
    , "fromMaybe (fromMaybe catMaybes)"
    , "fromMaybe (fromMaybe fromMaybe)"
    , "fromMaybe (fromMaybe listToMaybe)"
    , "fromMaybe (fromMaybe str)"
    , "fromMaybe (fromMaybe c)"
    , "fromMaybe (fromMaybe i)"
    , "fromMaybe (listToMaybe [])"
    , "fromMaybe (listToMaybe str)"
    , "fromMaybe (catMaybes [])"
    , "listToMaybe (catMaybes [])"
    , "catMaybes (catMaybes [])"
    , "fromMaybe (fromMaybe [] Nothing)"
    , "fromMaybe (fromMaybe Nothing Nothing)"
    , "fromMaybe (fromMaybe catMaybes Nothing)"
    , "fromMaybe (fromMaybe fromMaybe Nothing)"
    , "fromMaybe (fromMaybe listToMaybe Nothing)"
    , "fromMaybe (fromMaybe str Nothing)"
    , "fromMaybe (fromMaybe c Nothing)"
    , "fromMaybe (fromMaybe i Nothing)"
    , "fromMaybe (fromMaybe [] (listToMaybe []))"
    , "fromMaybe (fromMaybe Nothing (listToMaybe []))"
    , "fromMaybe (fromMaybe catMaybes (listToMaybe []))"
    , "fromMaybe (fromMaybe fromMaybe (listToMaybe []))"
    , "fromMaybe (fromMaybe listToMaybe (listToMaybe []))"
    , "fromMaybe (fromMaybe str (listToMaybe []))"
    , "fromMaybe (fromMaybe c (listToMaybe []))"
    , "fromMaybe (fromMaybe i (listToMaybe []))"
    , "fromMaybe (fromMaybe c (listToMaybe str))"
    , "fromMaybe (fromMaybe (fromMaybe []))"
    , "fromMaybe (fromMaybe (fromMaybe Nothing))"
    , "fromMaybe (fromMaybe (fromMaybe catMaybes))"
    , "fromMaybe (fromMaybe (fromMaybe fromMaybe))"
    , "fromMaybe (fromMaybe (fromMaybe listToMaybe))"
    , "fromMaybe (fromMaybe (fromMaybe str))"
    , "fromMaybe (fromMaybe (fromMaybe c))"
    , "fromMaybe (fromMaybe (fromMaybe i))"
    , "fromMaybe (fromMaybe (listToMaybe []))"
    , "fromMaybe (fromMaybe (listToMaybe str))"
    , "fromMaybe (fromMaybe (catMaybes []))"
    , "fromMaybe (listToMaybe (catMaybes []))"
    , "fromMaybe (catMaybes (catMaybes []))"
    , "listToMaybe (catMaybes (catMaybes []))"
    , "catMaybes (catMaybes (catMaybes []))"
    , "fromMaybe [] (listToMaybe (catMaybes []))"
    , "fromMaybe Nothing (listToMaybe (catMaybes []))"
    , "fromMaybe catMaybes (listToMaybe (catMaybes []))"
    , "fromMaybe fromMaybe (listToMaybe (catMaybes []))"
    , "fromMaybe listToMaybe (listToMaybe (catMaybes []))"
    , "fromMaybe str (listToMaybe (catMaybes []))"
    , "fromMaybe c (listToMaybe (catMaybes []))"
    , "fromMaybe i (listToMaybe (catMaybes []))"
    ]
  ]

data GenArgTestcase = GenArgTestcase {
  genArgDesc :: String,
  genArgProgram :: TProgram,
  genArgWant :: [String]
}

genArgTestcases :: [GenArgTestcase]
genArgTestcases = [
  GenArgTestcase
    "generate arguments for applications"
    (untyped (PApp "fromMaybe" [varp "d", untyped (PApp "listToMaybe" [untyped (PApp "catMaybes" [varp "xs"])])]))
    [ "fromMaybe d (listToMaybe (catMaybes xs))"
      -- single argument
    , "\\arg0 -> fromMaybe arg0 (listToMaybe (catMaybes xs))"
    , "\\arg0 -> arg0 d (listToMaybe (catMaybes xs))"
    , "\\arg0 -> fromMaybe d (arg0 (catMaybes xs))"
    , "\\arg0 -> fromMaybe d (listToMaybe (arg0 xs))"
    , "\\arg0 -> fromMaybe d (listToMaybe (catMaybes arg0))"
    , "\\arg0 -> arg0 (listToMaybe (catMaybes xs))"
    , "\\arg0 -> fromMaybe d (listToMaybe arg0)"
    , "\\arg0 -> fromMaybe d arg0"
    , "\\arg0 -> arg0"
      -- two arguments
    , "\\arg0 arg1 -> arg0 arg1 (listToMaybe (catMaybes xs))"
    , "\\arg0 arg1 -> arg0 d (arg1 (catMaybes xs))"
    , "\\arg0 arg1 -> arg0 d (listToMaybe (arg1 xs))"
    , "\\arg0 arg1 -> arg0 d (listToMaybe (catMaybes arg1))"
    , "\\arg0 arg1 -> arg0 d (listToMaybe arg1)"
    , "\\arg0 arg1 -> arg0 d arg1"
    , "\\arg0 arg1 -> fromMaybe arg0 (arg1 (catMaybes xs))"
    , "\\arg0 arg1 -> fromMaybe arg0 (listToMaybe (arg1 xs))"
    , "\\arg0 arg1 -> fromMaybe arg0 (listToMaybe (catMaybes arg1))"
    , "\\arg0 arg1 -> fromMaybe d (arg0 (arg1 xs))"
    , "\\arg0 arg1 -> fromMaybe d (arg0 (catMaybes arg1))"
    , "\\arg0 arg1 -> fromMaybe d (listToMaybe (arg0 arg1))"
    , "\\arg0 arg1 -> arg0 (arg1 (catMaybes xs))"
    , "\\arg0 arg1 -> arg0 (listToMaybe (arg1 xs))"
    , "\\arg0 arg1 -> arg0 (listToMaybe (catMaybes arg1))"
    , "\\arg0 arg1 -> fromMaybe d (arg0 arg1)"
    , "\\arg0 arg1 -> arg0 (listToMaybe arg1)"
    , "\\arg0 arg1 -> fromMaybe arg0 arg1"
    , "\\arg0 arg1 -> fromMaybe arg0 (listToMaybe arg1)"
    , "\\arg0 arg1 -> arg0 arg1"
    ]
  ]

data GenQueryTestcase = GenQueryTestcase {
  genQueryDesc :: String,
  genQueryComponents :: [(Text, SchemaSkeleton)],
  genQueryConfig :: Configuration,
  genQueryWant :: [(String, String)]
}

genQueryTestcases :: [GenQueryTestcase]
genQueryTestcases = [
  GenQueryTestcase
    "generate queries within two iteration"
    testComponents
    (Configuration 2 2 2 2 50 False)
    [("t0 -> [t0] -> t0","\\arg0 arg1 -> fromMaybe arg0 (listToMaybe arg1)")
    ,("[Maybe t0] -> Maybe t0","\\arg0 -> listToMaybe (catMaybes arg0)")
    ,("[Maybe t0] -> Maybe [t0] -> [t0]","\\arg0 -> fromMaybe (catMaybes arg0)")
    ]
  ]

data PostfilterTestcase = PostfilterTestcase {
  postfilterDesc :: String,
  postfilterProgram :: TProgram,
  postfilterSig :: String,
  postfilterWant :: Bool
}

postfilterTestcases :: [PostfilterTestcase]
postfilterTestcases = [
  PostfilterTestcase
    "rule out always crash"
    (untyped $ PFun "x" (untyped $ PApp "(,)" [varp "x", untyped $ PApp "GHC.List.head" [varp "[]"]]))
    "a -> (a, a)"
    False,

  PostfilterTestcase
    "rule out by demand analysis"
    (untyped $ PFun "x" (varp "1"))
    "Int -> Int"
    False,

  PostfilterTestcase
    "keep good programs"
    (untyped $ PFun "x" (untyped $ PApp "Data.Maybe.listToMaybe" [untyped $ PApp "Data.Maybe.catMaybes" [varp "x"]]))
    "[Maybe a] -> Maybe a"
    True,

  PostfilterTestcase
    "keep cycles"
    (untyped $ PFun "x" (untyped $ PApp "GHC.List.repeat" [varp "x"]))
    "a -> [a]"
    True
  ]

spec :: Spec
spec = do
  describe "test generateApp" $
    mapM_ (\tc ->
      it (genAppDesc tc) $ do
        graph <- runGenerateT (Generator Map.empty (genAppGraph tc) Map.empty (Configuration 2 2 2 2 2 False)) (generateApp $ genAppGraph tc)
        graph `shouldBe` genAppWantGraph tc
      ) appTestcases

--   describe "test generate" $
--     mapM_ (\tc ->
--       it (genDesc tc) $ do
--         let progs = generate (genComponents tc) (genRepeat tc)
--         length progs `shouldBe` genWantNum tc
--         (Set.fromList (genWantStr tc) `Set.isSubsetOf` Set.fromList (map plainShow progs)) `shouldBe` True
--       ) genTestcases

  describe "test argument abstraction" $ do
    let config = Configuration 2 2 2 2 2 False
    mapM_ (\tc ->
      it (genArgDesc tc) $ do
        let lambdas = evalState (P.toListM $ every $ assignArgs config (genArgProgram tc)) (Generator Map.empty Map.empty Map.empty config)
        let lambdas' = filter (\p -> numArguments p <= 2) lambdas
        let lambdas = map (\p -> plainShow (evalState (canonicalize p) Map.empty)) lambdas'
        Set.fromList lambdas `shouldBe` Set.fromList (genArgWant tc)
      ) genArgTestcases

  describe "test generate queries" $
    mapM_ (\tc ->
      it (genQueryDesc tc) $ do
        pairs <- evalStateT (P.toListM $ every $ generateQAPairs (genQueryComponents tc) (genQueryConfig tc)) Set.empty
        let results = map (\(t, p) -> (t, plainShow p)) pairs
        Set.fromList (genQueryWant tc) `Set.isSubsetOf` Set.fromList results `shouldBe` True
      ) genQueryTestcases

  describe "test postfilter" $
    mapM_ (\tc ->
      it (postfilterDesc tc) $ do
        result <- postfilter (postfilterProgram tc) (postfilterSig tc)
        result `shouldBe` (postfilterWant tc)
      ) postfilterTestcases