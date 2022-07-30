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

-- simpleExpressions :: [InterpreterTestCase]
-- simpleExpressions = 
--     [ InterpreterTestCase "1" "1"
--     , InterpreterTestCase "1 + 1" "2"
--     , InterpreterTestCase "2 * 2" "4"
--     , InterpreterTestCase "head [1,2,3]" "1"
--     , InterpreterTestCase "let isEven x = x `mod` 2 == 0 in filter isEven [1,2,3,4]" "[2,4]"
--     ]

-- runTest :: InterpreterTestCase -> IO ()
-- runTest (InterpreterTestCase input output) = do
--     result <- runInterpreter $ interpret input 
--     result `shouldBe` output

spec :: Spec
spec = describe "Interpreter" $ do
    it "integer" $ do
        result <- runInterpreter libdir $ do
            setImports ["Prelude"]
            interpret "1" (as :: Int)
        result `shouldBe` Right 1