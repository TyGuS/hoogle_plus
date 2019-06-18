module HooglePlus.Synthesize(synthesize, envToGoal) where

import Types.Common
import Types.Type
import Types.Experiments
import Types.Environment
import Types.Program
import Types.Solver
import Synquid.Util
import Synquid.Error
import Synquid.Logic
import Synquid.Type
import Synquid.Program
import Synquid.Pretty
import Synquid.Parser
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
import Control.Monad.Except
import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Applicative ((<$>))
import Text.Parsec.Pos
import Text.Parsec.Indent
import System.Exit
import Control.Concurrent.Chan
import Control.Exception

import Data.Time.Clock
import Data.Time.Format

updateEnvWithBoundTyVars :: RSchema -> Environment -> (Environment, RType)
updateEnvWithBoundTyVars (Monotype ty) env = (env, ty)
updateEnvWithBoundTyVars (ForallT x ty) env = updateEnvWithBoundTyVars ty (addTypeVar x env)

updateEnvWithSpecArgs :: RType -> Environment -> (Environment, RType)
updateEnvWithSpecArgs ty@(ScalarT _ _) env = (env, ty)
updateEnvWithSpecArgs (FunctionT x tArg tRes) env = updateEnvWithSpecArgs tRes $ unfoldAllVariables $ addVariable x tArg $ addArgument x tArg env

envToGoal :: Environment -> String -> IO Goal
envToGoal env queryStr = do
  let transformedSig = "goal :: " ++ queryStr ++ "\ngoal = ??"
  let parseResult = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" transformedSig
  case parseResult of
    Left parseErr -> putDoc (pretty $ toErrorMessage parseErr) >> putDoc empty >> error "uh oh"
    Right (funcDecl:decl:_) -> case decl of
      Pos _ (SynthesisGoal id uprog) -> do
        let Pos _ (FuncDecl _ sch) = funcDecl
        let goal = Goal id env sch uprog 3 $ initialPos "goal"
        let spec = runExcept $ evalStateT (resolveSchema (gSpec goal)) (initResolverState { _environment = env })
        case spec of
          Right sp -> return $ goal { gEnvironment = env, gSpec = sp }
          Left parseErr -> putDoc (pretty parseErr) >> putDoc empty >> exitFailure

      _ -> error "parse a signature for a none goal declaration"


synthesize :: SearchParams -> Goal -> Chan Message -> IO ()
synthesize searchParams goal messageChan = do
  let env''' = gEnvironment goal
  let (env'', monospec) = updateEnvWithBoundTyVars (gSpec goal) env'''
  let (env', destinationType) = updateEnvWithSpecArgs monospec env''
  let useHO = _useHO searchParams
  let env = if useHO then env'
                      else env' { _symbols = Map.map (Map.filter (not . isHigherOrder . toMonotype)) $ env' ^. symbols }
  let args = Monotype destinationType : Map.elems (env ^. arguments)
  -- start with all the datatypes defined in the components, first level abstraction
  let cnt = _solutionCnt searchParams
  let maxLevel = _explorerLogLevel searchParams
  let rs = _useRefine searchParams
  let maxDepth = _eGuessDepth searchParams
  let toStop = _earlyCut searchParams
  let threshold = _stopThresh searchParams
  let is = emptySolverState {
             _logLevel = maxLevel
           , _maxApplicationDepth = maxDepth
           , _refineStrategy = rs
           , _stopRefine = toStop
           , _threshold = threshold
           , _abstractionTree = case rs of
               SypetClone -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
               TyGar0 -> emptySolverState ^. abstractionTree
               TyGarQ -> Abstraction.specificAbstractionFromTypes env args
               NoGar -> Abstraction.specificAbstractionFromTypes env args
               NoGar0 -> emptySolverState ^. abstractionTree
               NoGarTyGar0 -> emptySolverState ^. abstractionTree
               NoGarTyGarQ -> Abstraction.specificAbstractionFromTypes env args
               NoGarTyGar0B -> emptySolverState ^. abstractionTree
               NoGarTyGarQB -> Abstraction.specificAbstractionFromTypes env args
           , _messageChan = messageChan
           }
  catch (evalStateT (runPNSolver env cnt destinationType) is)
    (\e -> print e >> writeChan messageChan (MesgClose (CSError e)))
  return ()
