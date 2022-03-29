module Types.Filtering where

import           Control.Exception              ( Exception )
import           Control.Monad.State            ( StateT )
import           Data.List                      ( intercalate )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Text.Printf                    ( printf )

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Serialize                 ( Serialize )

import           Test.SmallCheck.Drivers        ( PropertyFailure )
import           Types.Pretty
import           Types.Program

--------------------------------------------------------------------------------
-------------------------------- Constants -------------------------------------
--------------------------------------------------------------------------------

defaultTimeoutMicro :: Int
defaultTimeoutMicro = 1 * 10 ^ 6

defaultDepth :: Int
defaultDepth = 3

defaultInterpreterTimeoutMicro :: Int
defaultInterpreterTimeoutMicro = 4 * 10 ^ 6

defaultMaxOutputLength :: Int
defaultMaxOutputLength = 10

defaultGenerationTimeoutMicro :: Int
defaultGenerationTimeoutMicro = 30 * 10 ^ 6

defaultGenerationDepth :: Int
defaultGenerationDepth = 4

frameworkModules :: [(String, Maybe String)]
frameworkModules =
  zip
      [ "Test.SmallCheck"
      , "Test.SmallCheck.Drivers"
      , "Test.LeanCheck.Function.ShowFunction"
      , "System.IO.Silently"
      , "Control.Exception"
      , "Control.Monad"
      , "Control.Monad.State"
      ]
      (repeat Nothing)

    ++ [("Test.ChasingBottoms", Just "CB")]

--------------------------------------------------------------------------------
---------------------------------- Types ---------------------------------------
--------------------------------------------------------------------------------


data Example = Example
  { inputs :: [String]
  , output :: String
  }
  deriving (Eq, Ord, Show, Generic)

instance Pretty Example where
  pretty e = hsep [hsep (map pretty $ inputs e), "==>", pretty (output e)]

instance ToJSON Example
instance FromJSON Example
instance Serialize Example

type SmallCheckResult = (Maybe PropertyFailure, [Example])
type GeneratorResult = [Example]
type SolutionPair = (TProgram, TProgram) -- qualified and unqualified
type AssociativeExamples = [(SolutionPair, [Example])]

data FunctionCrashDesc =
    AlwaysSucceed Example
  | AlwaysFail Example
  | PartialFunction [Example]
  | UnableToCheck String
  deriving (Eq)

instance Show FunctionCrashDesc where
  show (AlwaysSucceed   i ) = show i
  show (AlwaysFail      i ) = show i
  show (PartialFunction xs) = unlines (map show xs)
  show (UnableToCheck   ex) = "Exception: " ++ show ex

data ArgumentType =
    Concrete    String
  | Polymorphic String
  | ArgTypeList ArgumentType
  | ArgTypeTuple [ArgumentType]
  | ArgTypeApp  ArgumentType ArgumentType
  | ArgTypeFunc ArgumentType ArgumentType
  deriving (Eq)

instance Show ArgumentType where
  show (Concrete    name) = name
  show (Polymorphic name) = name
  show (ArgTypeList sub ) = printf "[%s]" (show sub)
  show (ArgTypeApp l r  ) = printf "((%s) (%s))" (show l) (show r)
  show (ArgTypeTuple types) =
    (printf "(%s)" . intercalate ", " . map show) types
  show (ArgTypeFunc src dst) = printf "((%s) -> (%s))" (show src) (show dst)

newtype NotSupportedException = NotSupportedException String
  deriving (Show, Typeable)

instance Exception NotSupportedException

newtype ClassConstraint = ClassConstraint { unClassConstraint :: String }
  deriving (Eq, Show)

instance Pretty ClassConstraint where
  pretty (ClassConstraint constraint) = pretty constraint

data FunctionSignature = FunctionSignature
  { _constraints :: [ClassConstraint]
  , _argsType    :: [ArgumentType]
  , _returnType  :: ArgumentType
  }

instance Show FunctionSignature where
  show (FunctionSignature constraints argsType returnType) = printf
    "(%s) => %s"
    constraintsExpr
    argsExpr
   where
    constraintsExpr = (intercalate ", " . map show) constraints
    argsExpr        = (intercalate " -> " . map show) (argsType ++ [returnType])

data FilterState = FilterState
  { filterInputs          :: [[String]]
  , solutions             :: [TProgram]
  , solutionExamples      :: [(SolutionPair, FunctionCrashDesc)]
  , differentiateExamples :: [(SolutionPair, Example)]
  }
  deriving Eq

emptyFilterState = FilterState { filterInputs          = []
                               , solutions             = []
                               , solutionExamples      = []
                               , differentiateExamples = []
                               }

type FilterTest m = StateT FilterState m
