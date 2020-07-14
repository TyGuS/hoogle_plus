{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TopDown.Synthesize(synthesize, envToGoal) where

import Database.Environment
import Database.Util
import qualified HooglePlus.Abstraction as Abstraction
import PetriNet.PNSolver
import HooglePlus.TypeChecker
import HooglePlus.GHCChecker (check)
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
import Types.Filtering
import Types.Program
import Types.Solver
import Types.TopDown
import Types.TypeChecker
import Types.Type
import Types.IOFormat
import HooglePlus.Utils
import HooglePlus.IOFormat
import Examples.ExampleChecker
import PetriNet.Util

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
    let goalType = gSpec goal :: RSchema
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

    start <- getCPUTime

    iterativeDeepening envWithHo messageChan searchParams examples goalType

    end <- getCPUTime

    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)

---------------
    
    writeChan messageChan (MesgClose CSNormal)
    return ()
---------------

-- determines if the result has all the appropriate arguments given the number of args
filterParams :: Int -> RProgram -> Bool
filterParams 0       _ = error "filterParams error: shouldn't have 0 args!" -- TODO maybe should be true here? 
filterParams 1       x = "arg0" `isInfixOf` (show x)
filterParams numArgs x = isInfixOf ("arg" ++ (show (numArgs - 1))) (show x) && filterParams (numArgs - 1) x


-- type CompsSolver m = StateT Comps (StateT CheckerState m)
-- evalCompsSolver messageChan m = m `evalStateT` emptyComps `evalStateT` (emptyChecker { _checkerChan = messageChan })

-- instance Monad m => CheckMonad (CompsSolver m) where
--     getNameCounter = lift getNameCounter
--     setNameCounter = lift . setNameCounter
--     getNameMapping = lift getNameMapping
--     setNameMapping = lift . setNameMapping
--     getIsChecked   = lift getIsChecked
--     setIsChecked   = lift . setIsChecked
--     getMessageChan = lift getMessageChan
--     overStats      = lift . overStats

-- type BTCompsSolver = LogicT (CompsSolver IO)

-- try to get solutions by calling dfs on depth 1 2 3 4... until we get an answer
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO ()
iterativeDeepening env messageChan searchParams examples goal = do

  solution <- (`evalStateT` emptyComps) $ observeT $ msum $ map helper [0..] :: IO RProgram
  print solution

  where

    -- calls dfs at a certain depth and checks to see if there is a solution
    helper :: Int -> LogicT (StateT Comps IO) RProgram
    helper depth = do
      liftIO $ printf "running dfs on %s at depth %d\n" (show goal) depth

      let goalType = shape $ lastType (toMonotype goal) :: SType
      solution <- dfs env messageChan depth goalType :: LogicT (StateT Comps IO) RProgram
      
      isChecked <- liftIO $ check' solution
      guard isChecked -- gets the first valid program

      -- liftIO $ print solution
      return solution
    
    check' :: RProgram -> IO Bool
    check' program = do
      -- printf "omg we are checking this program: %s\n" (show program)
      liftIO $ print "we are about to call check"
      checkResult <- evalStateT (check env searchParams examples program goal messageChan) emptyFilterState
      case checkResult of
        Nothing  -> return False
        Just exs -> do
          -- TODO add this back in
          -- out <- toOutput env program exs
          -- printResult $ encodeWithPrefix out
          return True

-- "Data.Tuple.fst (GHC.List.last arg0)
-- Data.Tuple.fst (GHC.List.head arg0)"

-- "(,) (GHC.List.repeat arg0) arg1
-- Data.Tuple.swap ((,) (GHC.List.repeat arg0) arg1)
-- GHC.List.splitAt (GHC.List.length arg1) (GHC.List.repeat arg0)
-- (,) ((:) arg0 arg1) ((:) arg0 arg1)
-- (,) (GHC.List.init arg1) (GHC.List.repeat arg0)"

-- a -> [a] -> ([a], [a])

-- ([] ++ []) !! (GHC.List.length arg0)

-- converts [a] to a Logic a
choices :: MonadPlus m => [a] -> m a
choices = msum . map return

--
-- does DFS stuff
--
dfs :: Environment -> Chan Message -> Int -> SType -> LogicT (StateT Comps IO) RProgram
dfs env messageChan depth goalType = do
  
  -- collect all the component types (which we might use to fill the holes)
  component <- choices $ Map.toList (env ^. symbols)

  -- liftIO $ print $ "dang we are here"
  -- find all functions that unify with goal type
  unifiedFunc <- (`evalStateT` emptyChecker {_checkerChan = messageChan}) $ getUnifiedFunctions env messageChan component goalType :: LogicT (StateT Comps IO) (Id, SType)
  -- for each of these functions, find solutions
  functionSolution <- turnFunctionIntoSolutions unifiedFunc :: LogicT (StateT Comps IO) RProgram
  return functionSolution
  
  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True

    turnFunctionIntoSolutions :: (Id, SType) -> LogicT (StateT Comps IO) RProgram
    turnFunctionIntoSolutions (id, schema)
      | isGround schema = return Program { content = PSymbol id, typeOf = refineTop env schema }
      | depth == 0 = mzero  -- stop if depth is 0
      | otherwise = do

---------
        -- -- TODO this is where we need to start changing things (!!!!!!!!!!!!!!!!!)

        -- -- collect all the argument types (the holes ?? we need to fill)
        let args = allArgTypes schema :: [SType]
        
        -- blah :: [SType] -> LogicT [RProgram]
        -- blah (x:xs) = do
        --     dfsstuff <- dfs ...
        --     return dfsstuff : blah xs

        -- do basically this: WE HOPE 
        -- dfsstuff0 <- dfs ... arg0 :: RProgram
        -- dfsstuff1 <- dfs ... arg1 :: RProgram
        -- dfsstuff2 <- dfs ... arg2 :: RProgram
        -- argsFilled = [dfsstuff0, dfsstuff1, dfsstuff2]
        argsFilled <- mapM (dfs env messageChan (depth - 1)) args :: LogicT (StateT Comps IO) [RProgram]


        -- fill in arguments of func as RPrograms - e.g. func a d, func a e, func a f, func b d, func b e, func b f
        let program = Program { content = PApp id argsFilled, typeOf = refineTop env schema }
        return program 
        -- do x <- [1,2,3]; y <- [4,5,6]; return (x + y)
                    -- X is first arg
                    -- Y is second arg




---------


        -- when (id == "GHC.List.length") $ do
        --   liftIO $ printf "trying to turn (%s :: %s) into programs that look like (%s ...args...) \n" id (show schema) id
        --   liftIO $ printf "\t* args: %s\n" (show args)
        --   liftIO $ printf "\t* programsPerArg: %s\n" (take 80 $ show programsPerArg)
        --   liftIO $ printf "\t* solutionsPerArg: %s\n" (take 80 $ show solutionsPerArg)
        --   liftIO $ printf "\t* finalResultList: %s\n" (take 80 $ show finalResultList)


--
-- gets list of components/functions that unify with a given type
-- 

    -- e.g. if you are looking for programs that fill a hole ?? :: [a]
    -- and you have a candidate, b -> b
    -- unify a and b (set a ==> b) and return this substitution explicitly
    -- this results in a function of type a -> a
    -- unifyGoal :: Map Id SType -> (Id, RSchema) -> StateT CheckerState IO (Maybe (Map Id SType, SType))
    -- unifyGoal    sub             (id, schema) = do
    --   modify $ set typeAssignment sub
    --   freshVars <- freshType (env ^. boundTypeVars) schema
    --   let retType = shape (lastType freshVars) :: SType
    --   solveTypeConstraint env retType goalType :: StateT CheckerState IO ()

    --   st <- get
    --   if st ^. isChecked
    --     then do
    --       let sub'        = st ^. typeAssignment
    --       let schema'     = stypeSubstitute sub' (shape freshVars)
    --       return $ Just (sub', schema')
    --     else return Nothing

-- q: we wanna save typeAssignment after unifying w first argument, for use in second argument
--    how do we do this?
-- a: when you unify with first argument, you pass in initial sub, you get a new sub
--    when you unify with second argument, you pass in a sub, and get a new sub
--    etc.



getUnifiedFunctions :: Environment -> Chan Message -> (Id, RSchema) -> SType
                    -> StateT CheckerState (LogicT (StateT Comps IO)) (Id, SType)
--  getUnifiedFunctions env messageChan components goalType mzero
getUnifiedFunctions env messageChan (id, schema) goalType = do
    
    freshVars <- freshType (env ^. boundTypeVars) schema

    let t1 = shape (lastType freshVars) :: SType
    let t2 = goalType :: SType

    -- modify $ set isChecked True
    -- modify $ set typeAssignment Map.empty

    solveTypeConstraint env t1 t2 :: StateT CheckerState (LogicT (StateT Comps IO)) ()
    st' <- get
    
    let sub =  st' ^. typeAssignment
    let checkResult = st' ^. isChecked

    let schema' = stypeSubstitute sub (shape freshVars)

    -- if it unifies, add that particular unified compoenent to state's list of components
    if (checkResult)
      then return (id, schema')
      else mzero

------

-- getUnifiedFunctions :: Environment -> Chan Message -> [(Id, RSchema)] -> SType
--                     -> StateT CheckerState (LogicT (StateT Comps IO)) (Id, SType)
--                     -> StateT CheckerState (LogicT (StateT Comps IO)) (Id, SType)
-- --  getUnifiedFunctions env messageChan components goalType mzero
-- getUnifiedFunctions _   _           []                     _        unifiedFuncs = unifiedFuncs
-- getUnifiedFunctions env messageChan ( v@(id, schema) : ys) goalType unifiedFuncs = do
    
--     -- liftIO $ print $ "here hehe"
--     freshVars <- freshType (env ^. boundTypeVars) schema

--     let t1 = shape (lastType freshVars) :: SType
--     let t2 = goalType :: SType

--     modify $ set isChecked True
--     -- modify $ set typeAssignment Map.empty

--     solveTypeConstraint env t1 t2 :: StateT CheckerState (LogicT (StateT Comps IO)) ()
--     st' <- get
    
--     let sub =  st' ^. typeAssignment
--     let checkResult = st' ^. isChecked

--     let schema' = stypeSubstitute sub (shape freshVars)

--     -- if it unifies, add that particular unified compoenent to state's list of components
--     if (checkResult)
--       then getUnifiedFunctions env messageChan ys goalType $ unifiedFuncs `mplus` (return (id, schema'))
--       else getUnifiedFunctions env messageChan ys goalType unifiedFuncs