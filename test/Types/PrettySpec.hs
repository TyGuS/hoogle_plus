module Types.PrettySpec 
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Types.Pretty
import Types.Type

data PrettyTestcase a = PrettyTestcase {
  prettyDesc :: String,
  prettyItem :: a,
  prettyWant :: String
}

prettyTypeTestcases :: [PrettyTestcase TypeSkeleton]
prettyTypeTestcases = [
  PrettyTestcase {
    prettyDesc = "pretty a",
    prettyItem = TypeVarT "a",
    prettyWant = "a"
  },
  PrettyTestcase {
    prettyDesc = "pretty [a]",
    prettyItem = listType (TypeVarT "a"),
    prettyWant = "[a]"
  },
  PrettyTestcase {
    prettyDesc = "pretty TopT",
    prettyItem = TopT,
    prettyWant = "⊤"
  },
  PrettyTestcase {
    prettyDesc = "pretty BotT",
    prettyItem = BotT,
    prettyWant = "⊥"
  },
  PrettyTestcase {
    prettyDesc = "pretty (a, b)",
    prettyItem = pairType (TypeVarT "a") (TypeVarT "b"),
    prettyWant = "(a, b)"
  },
  PrettyTestcase {
    prettyDesc = "pretty Maybe a",
    prettyItem = DatatypeT "Maybe" [TypeVarT "a"],
    prettyWant = "Maybe a"
  },
  PrettyTestcase {
    prettyDesc = "pretty a -> b -> c",
    prettyItem = FunctionT "x" (TypeVarT "a") (FunctionT "y" (TypeVarT "b") (TypeVarT "c")),
    prettyWant = "a -> b -> c"
  },
  PrettyTestcase {
    prettyDesc = "pretty (a -> b) -> a -> b",
    prettyItem = FunctionT "f" (FunctionT "x" (TypeVarT "a") (TypeVarT "b")) (FunctionT "" (TypeVarT "a") (TypeVarT "b")),
    prettyWant = "(a -> b) -> a -> b"
  },
  PrettyTestcase {
    prettyDesc = "pretty (a -> b, Maybe a)",
    prettyItem = pairType (FunctionT "x" (vart "a") (vart "b")) (DatatypeT "Maybe" [vart "a"]),
    prettyWant = "(a -> b, Maybe a)"
  },
  PrettyTestcase {
    prettyDesc = "pretty [[a]]",
    prettyItem = listType (listType $ vart "a"),
    prettyWant = "[[a]]"
  },
  PrettyTestcase {
    prettyDesc = "pretty Maybe (a -> b)",
    prettyItem = DatatypeT "Maybe" [FunctionT "x" (vart "a") (vart "b")],
    prettyWant = "Maybe (a -> b)"
  },
  PrettyTestcase {
    prettyDesc = "pretty [a -> b]",
    prettyItem = listType (FunctionT "x" (vart "a") (vart "b")),
    prettyWant = "[a -> b]"
  },
  PrettyTestcase {
    prettyDesc = "pretty [a] -> Maybe b -> (a, b)",
    prettyItem = FunctionT "xs" (listType $ vart "a") (FunctionT "mb" (DatatypeT "Maybe" [vart "b"]) (pairType (vart "a") (vart "b"))),
    prettyWant = "[a] -> Maybe b -> (a, b)"
  },
  PrettyTestcase {
    prettyDesc = "pretty (a -> Either b b) -> [b -> b] -> [b]",
    prettyItem = FunctionT "f" (FunctionT "x" (vart "a") (DatatypeT "Either" [vart "b", vart "b"])) (FunctionT "fs" (listType $ FunctionT "x" (vart "b") (vart "b")) (listType $ vart "b")),
    prettyWant = "(a -> Either b b) -> [b -> b] -> [b]"
  },
  PrettyTestcase {
    prettyDesc = "pretty Either (a -> b) c",
    prettyItem = DatatypeT "Either" [FunctionT "x" (vart "a") (vart "b"), vart "c"],
    prettyWant = "Either (a -> b) c"
  },
  PrettyTestcase {
    prettyDesc = "pretty Maybe (Maybe [a])",
    prettyItem = DatatypeT "Maybe" [DatatypeT "Maybe" [listType $ vart "a"]],
    prettyWant = "Maybe (Maybe [a])"
  },
  PrettyTestcase {
    prettyDesc = "pretty Either (a, b) [a]",
    prettyItem = DatatypeT "Either" [pairType (vart "a") (vart "b"), listType (vart "a")],
    prettyWant = "Either (a, b) [a]"
  }
  ]

spec :: Spec
spec = do
  describe "test pretty types" $
    mapM_ (\tc ->
      it (prettyDesc tc) $ do
        plainShow (prettyItem tc) `shouldBe` prettyWant tc
    ) prettyTypeTestcases
