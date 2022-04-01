module HooglePlus.Synthesize
  ( synthesize
  , envToGoal
  ) where

import           Control.Applicative            ( (<$>) )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Lens                   ( (^.) )
import           Control.Monad.Except           ( runExcept )
import           Control.Monad.State            ( evalStateT )
import           Data.List.Extra                ( nubOrdOn )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           System.Timeout                 ( timeout )
import           Text.Parsec                    ( runParser )
import           Text.Printf                    ( printf )

import           Compiler.Parser
import           Compiler.Resolver
import           Database.Dataset
import           Examples.InferenceDriver
import           HooglePlus.Abstraction
import           HooglePlus.IOFormat
import           HooglePlus.Utils
import           PetriNet.PNSolver
import           Types.Environment
import           Types.Experiments
import           Types.Filtering
import           Types.Pretty
import           Types.Program
import           Types.Solver
import           Types.Type
import           Types.TypeChecker

envToGoal :: Environment -> String -> IO Goal
envToGoal env queryStr = do
  let parseResult = runParser parseType () "goal" queryStr
  case parseResult of
    Left parseErr ->
      let e = toErrorMessage parseErr
      in  putDoc (pretty e) >> putDoc linebreak >> error (prettyShow e)
    Right typ -> do
      let spec = runExcept $ evalStateT (resolveType [] typ) initResolverState
      case spec of
        Right sp -> do
          let (env', monospec) = updateEnvWithBoundTyVars (Monotype sp) env
          let (env'', destinationType) = updateEnvWithSpecArgs monospec env'
          return $ Goal { gEnvironment = env'', gSpec = sp }
        Left parseErr ->
          putDoc (pretty parseErr) >> putDoc linebreak >> error
            (prettyShow parseErr)

synthesize :: SearchParams -> Goal -> [Example] -> IO (Maybe ())
synthesize searchParams (Goal env goalType) examples = catch
  (do
    let destinationType = lastType goalType
    let useHO           = _useHO searchParams
    let args = map Monotype (destinationType : map snd (getArguments env))
  -- start with all the datatypes defined in the components, first level abstraction
    let rs              = _refineStrategy searchParams
    let initCover = case rs of
          SypetClone -> firstLvAbs env (Map.elems (allSymbols env))
          TyGar0     -> emptySolverState ^. (refineState . abstractionCover)
          TyGarQ     -> specificAbstractionFromTypes env args
          NoGar      -> specificAbstractionFromTypes env args
          NoGar0     -> emptySolverState ^. (refineState . abstractionCover)
    let is = emptySolverState
          { _searchParams = searchParams
          , _refineState  = emptyRefineState { _abstractionCover = initCover }
          , _typeChecker  = emptyChecker
          }

    -- before synthesis, first check that user has provided valid examples
    let exWithOutputs = filter ((/=) "??" . output) examples
    checkResult <- checkExamples includedModules
                                 env
                                 (Monotype goalType)
                                 exWithOutputs
    let augmentedExamples = examples -- nubOrdOn inputs $ examples ++ preseedExamples
    case checkResult of
      Left  errs -> error (unlines ("Examples does not type check" : errs))
      Right _    -> timeout (600 * 10 ^ 6)
        $ evalStateT (runPNSolver env goalType augmentedExamples) is
  )
  (\(e :: SomeException) -> do
    printResult (encodeWithPrefix (QueryOutput [] (show e) []))
    error (show e)
  )
