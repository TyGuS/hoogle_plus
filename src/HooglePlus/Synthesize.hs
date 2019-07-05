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
import Database.Environment
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
import Text.Printf (printf)
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
updateEnvWithSpecArgs (FunctionT x tArg tRes) env = updateEnvWithSpecArgs tRes $ addVariable x tArg $ addArgument x tArg env

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
  let rawSyms = env' ^. symbols
  let hoCands = env' ^. hoCandidates
  env <- if useHO
            then do -- add higher order query arguments
                let args = env' ^. arguments
                let hoArgs = Map.filter (isFunctionType . toMonotype) args
                let hoFuns = map (\(k, v) -> (k ++ "'ho'", toFunType v)) (Map.toList hoArgs)
                return $ env' { _symbols = rawSyms `Map.union` Map.fromList hoFuns
                              , _hoCandidates = hoCands ++ map fst hoFuns }
            else do-- filter out hoArgs in the environment
                let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
                return $ env' { _symbols = Map.withoutKeys syms $ Set.fromList hoCands
                              , _hoCandidates = [] }
  putStrLn $ "Component number: " ++ show (Map.size $ allSymbols env)
  let args = Monotype destinationType : Map.elems (env ^. arguments)
  -- start with all the datatypes defined in the components, first level abstraction
  let rs = _refineStrategy searchParams
  let is = emptySolverState {
             _searchParams = searchParams
           , _abstractionCover = case rs of
               SypetClone -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
               TyGar0 -> emptySolverState ^. abstractionCover
               TyGarQ -> Abstraction.specificAbstractionFromTypes env args
               NoGar -> Abstraction.specificAbstractionFromTypes env args
               NoGar0 -> emptySolverState ^. abstractionCover
               NoGarTyGar0 -> emptySolverState ^. abstractionCover
               NoGarTyGarQ -> Abstraction.specificAbstractionFromTypes env args
               NoGarTyGar0B -> emptySolverState ^. abstractionCover
               NoGarTyGarQB -> Abstraction.specificAbstractionFromTypes env args
           , _messageChan = messageChan
           }
  catch (evalStateT (runPNSolver env destinationType) is)
    (\e -> writeChan messageChan (MesgLog 0 "error" (show e)) >> writeChan messageChan (MesgClose (CSError e)))
  return ()
