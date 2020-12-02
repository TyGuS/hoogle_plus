module Encoder.PrologTypes where

import Data.List (intercalate)
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.State
import Data.Data
import Data.Typeable
import Data.Function
import Control.Lens
import Text.Printf

import Types.Common
import Types.Type
import Types.Experiments
import Encoder.ConstraintEncoder (FunctionCode)

data Direction = In | Out

instance Show Direction where
    show In = "in"
    show Out = "out"

type Place = Int
type Transition = Int
type Tokens = Int
type TimeStep = Int
type Marking = [(Int, Int)]
data Constraint = Node Id Int
                | Arc Direction Place Transition Tokens
                | FireAt TimeStep Transition
                | NotFireAt TimeStep Transition
                | MarkingAt TimeStep Marking
                | NotMarkingAt TimeStep Marking
                | Choices [Constraint]
                | Comment String

instance Show Constraint where
    show (Node name i) = printf "%s(%d)." name i
    show (Arc dir p tr w) = printf "arc(%d, %s, %d, %d)." tr (show dir) p w
    show (FireAt t tr) = printf "T%d = %d" t tr
    show (NotFireAt t tr) = printf "T%d \\= %d" t tr
    show (MarkingAt t m) = printf "M%d = %s" t (show $ map (\(a,b) -> [a,b]) m)
    show (NotMarkingAt t m) = printf "M%d \\= %s"  t (show $ map (\(a,b) -> [a,b]) m)
    show (Choices cs) = printf "(%s)" (intercalate "; " (map show cs))
    show (Comment str) = printf "%% %s" str