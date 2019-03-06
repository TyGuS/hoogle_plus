module Synquid.GraphConstraintSolver
(
  EdgeType(..)
) where

import Synquid.Graph hiding (nodes, edges)
import Synquid.Program
import Synquid.Util

import Data.List
import Data.Maybe
import qualified Z3.Monad as Z3
import Z3.Base (Context, Optimize)
import Z3.Monad (AST, MonadZ3, Result(..))
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Extra
import Data.Time.Clock

data EdgeType = BoolVar
              | IntSet
              | BoolSet -- encode X(e,arg) as a bool value which means the edge `e` comes from inhabited `arg`
