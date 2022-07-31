module Interpreter.InterpreterSpec (
    spec
) where

import Test.Hspec
import GHC.Paths (libdir)

import Interpreter.Interpreter

data InterpreterTestCase a = InterpreterTestCase {
    input :: String,
    output :: a
} deriving (Eq, Ord, Show)

spec :: Spec
spec = describe "Interpreter" $ do
    it "integer" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude"]
            interpret "1" (as :: Int)
        result `shouldBe` Right 1

    it "simple addition" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude"]
            interpret "1+1" (as :: Int)
        result `shouldBe` Right 2

    it "simple multiplication" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude"]
            interpret "2 * 2" (as :: Int)
        result `shouldBe` Right 4

    it "list operation" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude"]
            interpret "head [1,2,3,4]" (as :: Int)
        result `shouldBe` Right 1

    it "local function definition" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude"]
            interpret "let isEven x = x `mod` 2 == 0 in filter isEven [1,2,3,4]" (as :: [Int])
        result `shouldBe` Right [2,4]

    it "import Data.Maybe; execute `Just 1`" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude", "Data.Maybe"]
            interpret "Just 1" (as :: Maybe Int)
        result `shouldBe` Right (Just 1)

    it "import Data.Maybe; execute `catMaybes [Just \"a\", Nothing]`" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude", "Data.Maybe"]
            interpret "catMaybes [Just \"a\", Nothing]" (as :: [String])
        result `shouldBe` Right ["a"]

    it "import Data.Maybe; execute `fromMaybe 1 (listToMaybe (catMaybes [Nothing, Just 2, Just 3, Nothing]))`" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude", "Data.Maybe"]
            interpret "fromMaybe 1 (listToMaybe (catMaybes [Nothing, Just 2, Just 3, Nothing]))" (as :: Int)
        result `shouldBe` Right 2

    it "import System.Timeout; execute `timeout 10 (print [1,2,3])`" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude", "System.Timeout"]
            interpret "timeout 10 (print [1,2,3])" (as :: IO (Maybe ()))
        actual <- case result of
            Left _ -> error "result is not Right"
            Right r -> r
        actual `shouldBe` Just ()

    it "import System.Timeout; execute `timeout 10 (print $ repeat 1)`" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude", "System.Timeout"]
            interpret "timeout (1*10^6) (print $ repeat 1)" (as :: IO (Maybe ()))
        actual <- resolveIO result
        actual `shouldBe` Nothing

    it "test quickcheck" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude", "Test.QuickCheck"]
            interpret "let prop_reverse = (\\xs -> reverse (reverse xs) == xs) :: [Int] -> Bool in quickCheck prop_reverse" (as :: IO ())
        actual <- resolveIO result
        actual `shouldBe` ()

    it "test smallcheck" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude", "Test.SmallCheck"]
            interpret "let prop_reverse = (\\xs -> reverse (reverse xs) == xs) :: [Int] -> Bool in smallCheck 3 prop_reverse" (as :: IO ())
        actual <- resolveIO result
        actual `shouldBe` ()
    where
        resolveIO result = case result of
            Left _ -> error "result is not Right"
            Right r -> r