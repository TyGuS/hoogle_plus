module Types.Common where
import qualified Language.Haskell.Exts.Syntax as HSE
import Data.Function

{-
This is a place for any top-level types that will be used by many modules.
This should have no dependencies on any other files in this project. This should be
the Top Level module.
-}
type Id = String

-- for encoding abstractions into JSON string
type Param = String -- parameter type

data FunctionCode = FunctionCode {
  funName   :: String,  -- function name
  hoParams  :: [FunctionCode],
  funParams :: [Param], -- function parameter types and their count
  funReturn :: [String]   -- function return type
} deriving(Show)

instance Eq FunctionCode where
  fc1 == fc2 = let
    areEq arg = on (==) arg fc1 fc2
    in areEq hoParams && areEq funParams && areEq funReturn

instance Ord FunctionCode where
  compare fc1 fc2 = let
    thenCmp EQ       ordering = ordering
    thenCmp ordering _        = ordering
    cmp arg = on compare arg fc1 fc2
    in foldr1 thenCmp [cmp hoParams, cmp funParams, cmp funReturn]

varName = "_v"
