{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TopDown.Synthesize(synthesize, envToGoal) where

import Database.Environment
import Database.Util
import qualified HooglePlus.Abstraction as Abstraction
import PetriNet.PNSolver
import HooglePlus.TypeChecker
import Synquid.Error
import Synquid.Logic
import Synquid.Parser
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver
import Synquid.Type
import Synquid.Util
import Types.CheckMonad
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Program
import Types.Solver
import Types.TopDown
import Types.TypeChecker
import Types.Type
import Types.IOFormat
import HooglePlus.Utils
import HooglePlus.IOFormat
import Examples.ExampleChecker
import PetriNet.Util -- TODO can we do this? lol 

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
import System.CPUTime
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
synthesize searchParams goal examples messageChan = do
    let rawEnv = gEnvironment goal
    let goalType = gSpec goal
    let destinationType = lastType (toMonotype goalType)
    let useHO = _useHO searchParams
    let rawSyms = rawEnv ^. symbols
    let hoCands = rawEnv ^. hoCandidates
    envWithHo <- do

    --------------------------
    -- HIGHER ORDER STUFF 
    -- envWithHo <- if useHO -- add higher order query arguments
    --     then do
    --         let args = rawEnv ^. arguments
    --         let hoArgs = Map.filter (isFunctionType . toMonotype) args
    --         let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
    --         return $ rawEnv { 
    --             _symbols = rawSyms `Map.union` Map.fromList hoFuns, 
    --             _hoCandidates = hoCands ++ map fst hoFuns
    --             }
    --     else do
    --------------------------

      let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
      return $ rawEnv {
          _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
          _hoCandidates = []
          }

    -- used for figuring out which programs to filter (those without all arguments)
    let numArgs = length (Map.elems (envWithHo ^. arguments))

    -- TIMING STUF
    start <- getCPUTime
    printf "running dfsTop on %s\n" (show $ shape destinationType)

    foo <- dfsTop envWithHo messageChan 3 (shape destinationType) numArgs
    
    printf "done running dfsTop on %s\n" (show $ shape destinationType)

    end <- getCPUTime
    
    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
------------
    writeChan messageChan (MesgClose CSNormal)
    return ()


type CompsSolver m = StateT Comps (StateT CheckerState m)

instance Monad m => CheckMonad (CompsSolver m) where
    getNameCounter = lift getNameCounter
    setNameCounter = lift . setNameCounter
    getNameMapping = lift getNameMapping
    setNameMapping = lift . setNameMapping
    getIsChecked   = lift getIsChecked
    setIsChecked   = lift . setIsChecked
    getMessageChan = lift getMessageChan
    overStats      = lift . overStats


--
-- start off calling dfs with an empty memoize map
--
dfsTop :: Environment -> Chan Message -> Int -> SType -> Int -> IO [String] -- TODO change to RProgram
dfsTop env messageChan depth hole numArgs = helper `evalStateT` emptyComps `evalStateT` (emptyChecker { _checkerChan = messageChan })
  where
    helper :: CompsSolver IO [String]
    helper = do

      -- collect all the component types (which we might use to fill the holes)
      let components = Map.toList (env ^. symbols)

      -- map each hole ?? to a list of component types that unify with the hole
      unifiedFuncs <- getUnifiedFunctions env messageChan components hole :: CompsSolver IO [(Id, SType)]
      liftIO $ putStrLn $ show unifiedFuncs

      -- get the first valid program from each of the functions in unifiedFuncs
      fmap concat $ mapM getFirstValidProgram unifiedFuncs :: CompsSolver IO [String]
    
    -- get the first valid program that matches the given component
    -- TODO right now it returns a list of all of the functions as a [String]
    -- TODO   change it to return a single RProgram
    getFirstValidProgram :: (Id, SType) -> CompsSolver IO [String]
    getFirstValidProgram comp = do 
                      samplePrograms <- dfs env messageChan depth comp :: CompsSolver IO [RProgram]
                      let samplePrograms' = map show samplePrograms :: [String] -- TODO we should not map show; just return single RProgram
                      let filtered = filter (filterParams numArgs) samplePrograms'
                      unless (null filtered) (liftIO $ putStrLn $ head filtered) -- we print out the first result for each function
                      return samplePrograms'
    
    -- determines if the result has all the appropriate arguments given the number of args
    filterParams :: Int -> String -> Bool
    filterParams 0       _ = error "filterParams error: shouldn't have 0 args!" -- TODO maybe should be true here? 
    filterParams 1       x = "arg0" `isInfixOf` x
    filterParams numArgs x = isInfixOf ("arg" ++ (show (numArgs - 1))) x && filterParams (numArgs - 1) x

--
-- gets list of components/functions that unify with a given type
-- 
getUnifiedFunctions :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> CompsSolver IO [(Id, SType)]
getUnifiedFunctions envv messageChan xs goalType = do

  modify $ set components []

  st <- get
  let memoized = st ^. memoize :: Map SType [(Id, SType)]

  case Map.lookup goalType memoized of
    Just cs -> do
      return cs
    Nothing -> do
      helper envv messageChan xs goalType
      st <- get
      let cs = st ^. components
      modify $ set memoize (Map.insert goalType cs (st ^. memoize))
      return $ st ^. components
  
  where 
    helper :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> CompsSolver IO ()
    helper _ _ [] _ = return ()
    
    helper envv messageChan ( v@(id, schema) : ys) goalType = do
        (freshVars, st') <- lift $ do

          freshVars <- freshType (envv ^. boundTypeVars) schema

          let t1 = shape (lastType freshVars) :: SType
          let t2 = goalType :: SType

          modify $ set isChecked True
          modify $ set typeAssignment Map.empty

          solveTypeConstraint envv t1 t2 :: StateT CheckerState IO ()
          st' <- get
          
          return (freshVars, st') :: StateT CheckerState IO (RType, CheckerState)

        let sub =  st' ^. typeAssignment
        let checkResult = st' ^. isChecked
        -- liftIO $ putStrLn $ show (id, "      ", t1, "      ", t2, "      ",freshVars,"      ",checkResult)

        let schema' = stypeSubstitute sub (shape freshVars)

        st <- get

        -- if it unifies, add that particular unified compoenent to state's list of components
        if (checkResult) 
          then do
            modify $ set components ((id, schema') : st ^. components) 
          else return ()

        helper envv messageChan ys goalType

-- 
-- runs dfs of given depth and keeps trying to find complete programs (no filtering yet)
--
dfs :: Environment -> Chan Message -> Int -> (Id, SType) -> CompsSolver IO [RProgram]
dfs env messageChan depth (id, schema)
  | isGround schema = return $ [
      Program { content = PSymbol id, typeOf = refineTop env schema }
    ]
  | depth == 0 = return []  -- stop if depth is 0
  | otherwise = do

    st <- get

    -- collect all the argument types (the holes ?? we need to fill)
    let args = allArgTypes schema

    -- collect all the component types (which we might use to fill the holes)
    let components = Map.toList (env ^. symbols)

    -- map each hole ?? to a list of component types that unify with the hole
    argUnifiedFuncs <- mapM (getUnifiedFunctions env messageChan components) args :: CompsSolver IO [[(Id, SType)]]

    -- recurse, solving each unified component as a goal, solution is a list of programs
    -- the first element of solutionsPerArg is the list of solutions for the first argument
    -- e.g. 
    let recurse = dfs env messageChan (depth - 1)
    solutionsPerArg <- mapM (fmap concat . mapM recurse) argUnifiedFuncs :: CompsSolver IO [[RProgram]] -- [[a,b,c], [d,e,f]]
    
    -- each arg hole is a list of programs
    -- take cartesian product of args and prepend our func name
    -- to get the list of resulting programs solving our original goal
    -- the first element of programsPerArg is a list of programs that fit as first argument
    let programsPerArg = sequence solutionsPerArg :: [[RProgram]] -- [[a,d], [a,e], [a,f], [b,d], [b,e], [b,f], ...]
    let formatFn :: [RProgram] -> RProgram
        formatFn args = Program { content = PApp id args, typeOf = refineTop env schema }
    let finalResultList = map formatFn programsPerArg
    return finalResultList
  
  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True