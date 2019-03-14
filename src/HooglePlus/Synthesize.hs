module HooglePlus.Synthesize(synthesize) where

import Types.Common
import Types.Type
import Types.Experiments
import Types.Environment
import Types.Program
import Types.Solver
import Synquid.Util
import Synquid.Logic
import Synquid.Type
import Synquid.Program
import Synquid.Pretty
import Synquid.Resolver
import PetriNet.PNSolver
import qualified HooglePlus.Abstraction as Abstraction

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

synthesize :: SearchParams  -> Goal -> IO RProgram
synthesize searchParams goal = do
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
  let is = emptySolverState {
             _logLevel = maxLevel
           , _maxApplicationDepth = maxDepth
           , _refineStrategy = rs
           , _abstractionTree = case rs of
               NoRefine -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
               AbstractRefinement -> emptySolverState ^. abstractionTree
               Combination -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
               QueryRefinement -> Abstraction.specificAbstractionFromTypes env (args)
           }
  program <- evalStateT (runPNSolver env cnt destinationType) is
  -- will need to attach params
  return program
