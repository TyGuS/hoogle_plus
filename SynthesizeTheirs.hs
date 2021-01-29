module HooglePlus.SynthesizeTheirs(synthesize, envToGoal) where

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
import Types.TypeChecker
import Types.Type
import Types.IOFormat
import HooglePlus.Utils
import HooglePlus.IOFormat
import Examples.ExampleChecker

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
import Data.List.Extra (nubOrdOn)
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


envToGoal :: Environment -> String -> IO Goal
envToGoal env queryStr = do
  let transformedSig = "goal :: " ++ queryStr ++ "\ngoal = ??"
  let parseResult = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" transformedSig
  case parseResult of
    Left parseErr -> let e = toErrorMessage parseErr
                      in putDoc (pretty e) >> putDoc linebreak >> error (prettyShow e)
    Right (funcDecl:decl:_) -> case decl of
      Pos _ (SynthesisGoal id uprog) -> do
        let Pos _ (FuncDecl _ sch) = funcDecl
        let goal = Goal id env sch uprog 3 $ initialPos "goal"
        let spec = runExcept $ evalStateT (resolveSchema (gSpec goal)) (initResolverState { _environment = env })
        case spec of
          Right sp -> do
            let (env', monospec) = updateEnvWithBoundTyVars sp env
            let (env'', destinationType) = updateEnvWithSpecArgs monospec env'
            return $ goal { gEnvironment = env'', gSpec = sp }
          Left parseErr -> putDoc (pretty parseErr) >> putDoc linebreak >> error (prettyShow parseErr)
      _ -> error "parse a signature for a none goal declaration"

synthesize :: SearchParams -> Goal -> [Example] -> Chan Message -> IO ()
synthesize searchParams goal examples messageChan = catch (do
    let rawEnv = gEnvironment goal
    let goalType = gSpec goal
    let destinationType = lastType (toMonotype goalType)
    let useHO = _useHO searchParams
    let rawSyms = rawEnv ^. symbols
    let hoCands = rawEnv ^. hoCandidates
    envWithHo <- if useHO -- add higher order query arguments
        then do
            let args = rawEnv ^. arguments
            let hoArgs = Map.filter (isFunctionType . toMonotype) args
            let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
            return $ rawEnv { 
                _symbols = rawSyms `Map.union` Map.fromList hoFuns, 
                _hoCandidates = hoCands ++ map fst hoFuns
                }
        else do
            let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
            return $ rawEnv {
                _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
                _hoCandidates = []
                }
    -- putStrLn $ "Component number: " ++ show (Map.size $ allSymbols env)
    let args = Monotype destinationType : Map.elems (envWithHo ^. arguments)
  -- start with all the datatypes defined in the components, first level abstraction
    let rs = _refineStrategy searchParams
    let initCover = case rs of
                        SypetClone -> Abstraction.firstLvAbs envWithHo (Map.elems (allSymbols envWithHo))
                        TyGar0 -> emptySolverState ^. (refineState . abstractionCover)
                        TyGarQ -> Abstraction.specificAbstractionFromTypes envWithHo args
                        NoGar -> Abstraction.specificAbstractionFromTypes envWithHo args
                        NoGar0 -> emptySolverState ^. (refineState . abstractionCover)
    let is =
            emptySolverState
                { _searchParams = searchParams
                , _refineState = emptyRefineState { _abstractionCover = initCover }
                , _messageChan = messageChan
                , _typeChecker = emptyChecker { _checkerChan = messageChan }
                }

    -- before synthesis, first check that user has provided valid examples
    let exWithOutputs = filter ((/=) "??" . output) examples
    checkResult <- checkExamples envWithHo goalType exWithOutputs messageChan
    -- preseedExamples <- augmentTestSet envWithHo goalType
    let augmentedExamples = examples -- nubOrdOn inputs $ examples ++ preseedExamples
    case checkResult of
      Right errs -> error (unlines ("examples does not type check" : errs))
      Left _ -> evalStateT (runPNSolver envWithHo goalType augmentedExamples) is)
    (\e ->
         writeChan messageChan (MesgLog 0 "error" (show e)) >>
         writeChan messageChan (MesgClose (CSError e)) >>
         printResult (encodeWithPrefix (QueryOutput [] (show e) [])) >>
         error (show e))
    -- return ()
