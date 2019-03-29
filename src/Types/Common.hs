module Types.Common where

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
} deriving(Eq, Ord, Show)
