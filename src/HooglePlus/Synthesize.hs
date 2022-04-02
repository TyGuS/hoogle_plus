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
import           Control.Monad.State            ( evalStateT
                                                , runStateT
                                                )
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
import           Types.Pretty
import           Types.Program
import           Types.Solver
import           Types.Type
import           Types.TypeChecker

envToGoal :: Environment -> String -> [Example] -> IO Goal
envToGoal env queryStr examples = do
  let parseResult = runParser parseSchema () "goal" queryStr
  case parseResult of
    Left parseErr ->
      let e = toErrorMessage parseErr
      in  putDoc (pretty e) >> putDoc linebreak >> error (prettyShow e)
    Right typ -> do
      let spec =
            runExcept $ evalStateT (resolveSchema [] typ) resolver
      case spec of
        Right sp -> do
          -- before synthesis, first check that user has provided valid examples
          let exWithOutputs = filter ((/=) "??" . output) examples
          checkResult <- checkExamples includedModules env sp exWithOutputs
          case checkResult of
            Left errs ->
              error (unlines ("Examples does not type check" : errs))
            Right _ -> return ()

          let (env', monospec)         = updateEnvWithBoundTyVars sp env
          let (env'', destinationType) = updateEnvWithSpecArgs monospec env'
          return $ Goal { gEnvironment = env''
                        , gSpec        = toMonotype sp
                        , gExamples    = examples
                        }
        Left parseErr ->
          putDoc (pretty parseErr) >> putDoc linebreak >> error
            (prettyShow parseErr)

synthesize :: SearchParams -> Goal -> IO ([TProgram], SolverState)
synthesize searchParams (Goal env goalType _) = catch
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

    runStateT (runPNSolver env goalType) is
  )
  (\(e :: SomeException) -> do
    printResult (encodeWithPrefix (QueryOutput [] (show e) []))
    error (show e)
  )
