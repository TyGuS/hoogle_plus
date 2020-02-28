module HooglePlus.Synthesize(synthesize, envToGoal) where

import Database.Environment
import Database.Util
import qualified HooglePlus.Abstraction as Abstraction
import PetriNet.PNSolver
import Synquid.Error
import Synquid.Logic
import Synquid.Parser
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver
import Synquid.Type
import Synquid.Util
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Program
import Types.Solver
import Types.Type

import Control.Applicative ((<$>))
import Control.Concurrent.Chan
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock
import Data.Time.Format
import System.Exit
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Printf (printf)


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
    env <-
        if useHO -- add higher order query arguments
            then do
    --------------------------
    -- IGNORE PARTS BELOW THIS
    --------------------------
                let args = env' ^. arguments
                let hoArgs = Map.filter (isFunctionType . toMonotype) args
                let hoFuns = map (\(k, v) -> (k ++ hoPostfix, toFunType v)) (Map.toList hoArgs)
                return $
                    env'
                        { _symbols = rawSyms `Map.union` Map.fromList hoFuns
                        , _hoCandidates = hoCands ++ map fst hoFuns
                        }
    --------------------------
    -- IGNORE PARTS ABOVE THIS
    --------------------------
            else do
                let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
                return $
                    env'
                        {_symbols = Map.withoutKeys syms $ Set.fromList hoCands, _hoCandidates = []}

    putStrLn $ "Component number: " ++ show (Map.size $ allSymbols env)
    putStrLn $ "Hello world"
    let args = Monotype destinationType : Map.elems (env ^. arguments)
    print $ args
    print $ Monotype destinationType
  -- start with all the datatypes defined in the components, first level abstraction

    --------------------------
    -- trying code Zheng gave us 
    --------------------------

    -- make an empty solver state to use in evalState
    let initSolverState = emptySolverState 

    -- used trial just to get one type for testing (not real code)
    -- looking for SType
    let trial = shape destinationType
    let blah = solveTypeConstraint env 
    --st' <- evalStateT (solveTypeConstraint env trial trial) initSolverState
    --putStrLn $ "st': " ++ show st'

    --------------------------
    -- trying code Zheng gave us 
    --------------------------

    -- this is code we don't want I think below
    let rs = _refineStrategy searchParams
    let is =
            emptySolverState
                { _searchParams = searchParams
                , _abstractionCover =
                      case rs of
                          SypetClone -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
                          TyGar0 -> emptySolverState ^. abstractionCover
                          TyGarQ -> Abstraction.specificAbstractionFromTypes env args
                          NoGar -> Abstraction.specificAbstractionFromTypes env args
                          NoGar0 -> emptySolverState ^. abstractionCover
                , _messageChan = messageChan
                }
    catch
        (evalStateT (runPNSolver env destinationType) is)
        (\e ->
             writeChan messageChan (MesgLog 0 "error" (show e)) >>
             writeChan messageChan (MesgClose (CSError e)))
    -- this is code we don't want I think above
    return () 

