module HooglePlus.Synthesize(newsynthesize) where

import Synquid.Explorer
import Synquid.Util
import Synquid.Logic
import Synquid.Type
import Synquid.Program
import Synquid.SolverMonad
import Synquid.HornSolver
import Synquid.Z3
import Synquid.Pretty
import Synquid.Resolver
import Synquid.TypeConstraintSolver
import Synquid.Explorer
import Synquid.TypeChecker
import Synquid.Stats
import qualified PetriNet.PNSolver as PNSolver
import qualified PetriNet.Abstraction as Abstraction

import Data.Maybe
import Data.Either
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Reader
import Control.Monad.Logic
import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Applicative ((<$>))

import Data.Time.Clock
import Data.Time.Format


newsynthesize :: ExplorerParams -> HornSolverParams -> Goal -> IO RProgram
newsynthesize explorerParams solverParams goal = do
  let env = gEnvironment goal
  let spec = toMonotype $ gSpec goal
  let cnt = _solutionCnt explorerParams
  let useHO = _useHO explorerParams
  let env' = if useHO then env
                      else env { _symbols = Map.map (Map.filter (not . isHigherOrder . toMonotype)) $ env ^. symbols }
  let args = (Monotype spec):(Map.elems $ env' ^. arguments)
  -- start with all the datatypes defined in the components, first level abstraction
  let maxLevel = _explorerLogLevel explorerParams
  let rs = _useRefine explorerParams
  let maxDepth = _eGuessDepth explorerParams
  let is = PNSolver.emptySolverState {
             PNSolver._logLevel = maxLevel
           , PNSolver._maxApplicationDepth = maxDepth
           , PNSolver._refineStrategy = rs
           , PNSolver._abstractionTree = case rs of
               PNSolver.NoRefine -> Abstraction.firstLvAbs env' (Map.elems (allSymbols env))
               PNSolver.AbstractRefinement -> PNSolver.emptySolverState ^. PNSolver.abstractionTree
               PNSolver.Combination -> Abstraction.firstLvAbs env' (Map.elems (allSymbols env))
               PNSolver.QueryRefinement -> Abstraction.specificAbstractionFromTypes env' (args)
           }
  program <- evalStateT (PNSolver.runPNSolver env' cnt spec) is
  return program