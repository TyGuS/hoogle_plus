module HooglePlus.Synthesize(newsynthesize) where

import Types.Experiments
import Types.Environment
import Synquid.Util
import Synquid.Logic
import Synquid.Type
import Synquid.Program
import Synquid.Pretty
import Synquid.Resolver
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

updateEnvWithBoundTyVars :: RSchema -> Environment -> (Environment, RType)
updateEnvWithBoundTyVars (Monotype ty) env = (env, ty)
updateEnvWithBoundTyVars (ForallT x ty) env = updateEnvWithBoundTyVars ty (addTypeVar x env)

updateEnvWithSpecArgs :: RType -> Environment -> (Environment, RType)
updateEnvWithSpecArgs ty@(ScalarT _ _) env = (env, ty)
updateEnvWithSpecArgs (FunctionT x tArg tRes) env = updateEnvWithSpecArgs tRes $ unfoldAllVariables $ addVariable x tArg $ addArgument x tArg $ env

newsynthesize :: SearchParams  -> Goal -> IO RProgram
newsynthesize searchParams goal = do
  let env''' = gEnvironment goal
  let (env'', monospec) = updateEnvWithBoundTyVars (gSpec goal) env'''
  let (env', destinationType) = updateEnvWithSpecArgs monospec env''
  let useHO = _useHO searchParams
  let env = if useHO then env'
                      else env' { _symbols = Map.map (Map.filter (not . isHigherOrder . toMonotype)) $ env' ^. symbols }
  let args = (Monotype destinationType):(Map.elems $ env ^. arguments)
  -- start with all the datatypes defined in the components, first level abstraction
  let cnt = _solutionCnt searchParams
  let maxLevel = _explorerLogLevel searchParams
  let rs = _useRefine searchParams
  let maxDepth = _eGuessDepth searchParams
  let is = PNSolver.emptySolverState {
             PNSolver._logLevel = maxLevel
           , PNSolver._maxApplicationDepth = maxDepth
           , PNSolver._refineStrategy = rs
           , PNSolver._abstractionTree = case rs of
               PNSolver.NoRefine -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
               PNSolver.AbstractRefinement -> PNSolver.emptySolverState ^. PNSolver.abstractionTree
               PNSolver.Combination -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
               PNSolver.QueryRefinement -> Abstraction.specificAbstractionFromTypes env (args)
           }
  program <- evalStateT (PNSolver.runPNSolver env cnt destinationType) is
  -- will need to attach params
  return program
