{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TopDown.Synthesize(synthesize, envToGoal) where

import Database.Environment
import Database.Util
import qualified HooglePlus.Abstraction as Abstraction
import PetriNet.PNSolver
import HooglePlus.TypeChecker
import HooglePlus.GHCChecker 
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
import Types.Filtering
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

     -- before synthesis, first check that user has provided valid examples
    let exWithOutputs = filter ((/=) "??" . output) examples
    checkResult <- checkExamples envWithHo goalType exWithOutputs messageChan
    --  checkSolution env goal examples code
    
    let augmentedExamples = examples -- nubOrdOn inputs $ examples ++ preseedExamples
    
    case checkResult of
      
      Right errs -> error (unlines ("examples does not type check" : errs))
      
      Left _ -> do

        -- used for figuring out which programs to filter (those without all arguments)
        let numArgs = length (Map.elems (envWithHo ^. arguments))
        
        let depth = 7
        start <- getCPUTime

        printf "running dfsTop on %s with depth %d\n" (show $ shape destinationType) depth

        solutions <- evalTopDownBackTrack messageChan $ dfs envWithHo messageChan depth (shape destinationType) :: IO [RProgram]

        -- print the first solution that has all the arguments
        mapM print $ take 1 $ filter (filterParams numArgs) solutions

        -- printf "done running dfsTop on %s\n" (show $ shape destinationType)

        end <- getCPUTime

        let diff = fromIntegral (end - start) / (10^12)
        printf "Computation time: %0.3f sec\n" (diff :: Double)




---------------
        -- TODO should we use `examples` or something else? also, `goalType` correct? 
        -- let myCheck :: RProgram -> FilterTest m (Maybe AssociativeExamples)
        --     myCheck program = check envWithHo searchParams examples program goalType messageChan

        -- -- how to use FilterTest in BackTrack (from check in src/PetriNet/PNSolver.hs)
        -- (checkResult, fState') <- withTime TypeCheckTime $ 
        --     liftIO $ runStateT (check env params examples code' goal msgChan) fState

        -- -- this is how you evaluate BackTrack, in PNSolver
        -- searchResults <- withTime FormerTime $ observeManyT cnt $
        --     enumeratePath env goal examples usefulTrans

        -- this is how to use PNSolver TODO ask zheng


        -- type PNSolver m = StateT SolverState m
        -- type BackTrack m = LogicT (PNSolver m)

        -- check :: MonadIO m 
        --       => Environment -- symbol environment
        --       -> SearchParams -- search parameters: to control what to be checked
        --       -> [Example] -- examples for post-filtering
        --       -> RProgram -- program to be checked
        --       -> RSchema -- goal type to be checked against
        --       -> Chan Message -- message channel for logging
        --       -> FilterTest m (Maybe AssociativeExamples) -- return Nothing is check fails, otherwise return a list of updated examples
        -- check env searchParams examples program goalType solverChan =
        --     runGhcChecks searchParams env (lastType $ toMonotype goalType) examples program

        -- how to use FilterTest (from check in HooglePlus/GHCChecker.hs)
        -- (checkResult, fState') <- withTime TypeCheckTime $ 
        --     liftIO $ runStateT (check env params examples code' goal msgChan) fState

        -- how to use BackTrack (from findProgram in PetriNet/PNSolver.hs)
        -- searchResults <- withTime FormerTime $ observeManyT cnt $
        --     enumeratePath env goal examples usefulTrans

------------
        writeChan messageChan (MesgClose CSNormal)
        return ()

    where
      -- determines if the result has all the appropriate arguments given the number of args
      -- TODO add "check" function here
      filterParams :: Int -> RProgram -> Bool
      filterParams 0       _ = error "filterParams error: shouldn't have 0 args!" -- TODO maybe should be true here? 
      filterParams 1       x = "arg0" `isInfixOf` (show x)
      filterParams numArgs x = isInfixOf ("arg" ++ (show (numArgs - 1))) (show x) && filterParams (numArgs - 1) x


type CompsSolver m = StateT Comps (StateT CheckerState m)
evalCompsSolver messageChan m = m `evalStateT` emptyComps `evalStateT` (emptyChecker { _checkerChan = messageChan })

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
-- does DFS stuff
--
-- Environment -> Int -> SType -> [RProgram]
dfs :: Environment -> Chan Message -> Int -> SType -> TopDownBackTrack IO RProgram
dfs env messageChan depth goalType = do
  
  -- collect all the component types (which we might use to fill the holes)
  let components = Map.toList (env ^. symbols)

  -- find all functions that unify with goal type
  -- unifiedFuncs <- getUnifiedFunctions' env messageChan components goalType :: CompsSolver IO [(Id, SType)]

  -- Int -> Int
  -- unifiedFuncs should first return arg0
  -- checks if that's ground and returns that
  -- if it gets to something else, like `add`
    -- then 


  -- unifiedFunc@(id, schema) <- getUnifiedFunctions' env messageChan components goalType mzero :: TopDownBackTrack IO (Id, SType)
  unifiedFunc@(id, schema) <- getUnifiedFunctions' env messageChan (choices components) goalType :: TopDownBackTrack IO (Id, SType)
  -- guard (isGround schema)
  guard (isInfixOf "length" id)
  
  liftIO $ print $ unifiedFunc
  -- liftIO $ print $ "here"



    -- print $ unifiedFunc
    -- guard ?????????
    -- return undefined

    -- guard (check if unfiedFunc)


      {--
            functionA :: Int -> [Int] -> [Int]
            functionA (x : xs)

            functionB :: Int -> [Int]
            = x : functionB xs 
      --}


{--
    takes in single type (return type)          e.g. Int
    finds all functions that unify with it      e.g. [f: Int, g: Int -> Int, h: Int -> Bool -> Int]
      for each of these functions F:
        if not ground:                          e.g. [Int -> Int, Int -> Bool -> Int]
          get arguments of F                    e.g. [[Int], [Int, Bool]]
          for each argument:
            call dfs on the argument to get the programs that create that argument
                                                e.g. Int becomes [f,g,h], Bool becomes []
          now you have a list of [RProgram] for each argument
          take our function F (e.g. h: Int -> Bool -> Int) and fill in the arguments using the [RProgram] we get
          to get a new list of [RProgram]       e.g. if F is g, then return [g f]
          we somehow take these programs for each argument and combine them into a bunch of larger programs
    then we return [RProgram] that build up the goalType
--}


    {--
        * get unified functions for original goal type
        * if any of those are ground
              return the first one that's ground
        * if neither are ground 
              look at the first unified function and recurse on its chiren 

                query: Bool -> Int
                                        goalType
              guard: whatever program we're looking at that's coming out of getUnifiedFunctions,
                     we want to check if it's ground          
          level 1:    find the ground program at this level, else           (incomplete: f (?? :: Bool) )     g ??? ??? ???? ???? ????        arg0
          level 2:    find the ground program at this level, else       
          level 3:    find the ground program at this level


    --}




    -- guard ()

  return undefined 


-- naiveFactorize' :: Int -> Logic (Int, Int)
-- naiveFactorize' n =
--     x <- nat
--     y <- nat
--     guard (2^x * y == n)
--     return (x, y)



  -- for each of these functions, find solutions
  -- functionSolutions <- mapM turnFunctionIntoSolutions unifiedFuncs :: TopDownBackTrack IO [RProgram] -- [solutions for func1, solutions for func2]
  -- let allFunctionSolutions = concat functionSolutions :: [RProgram]
  -- return allFunctionSolutions
  
  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True

    -- converts [a] to a Logic a
    choices :: MonadPlus m => [a] -> m a
    choices = msum . map return

  --   -- given a function that returns the type we want (goalType)
  --   -- find all the possible ways to call that function and return them as RProgram
  --   turnFunctionIntoSolutions :: (Id, SType) -> TopDownBackTrack IO RProgram
  --   turnFunctionIntoSolutions (id, schema)
  --     | isGround schema = return Program { content = PSymbol id, typeOf = refineTop env schema }
  --     | depth == 0 = return []  -- stop if depth is 0
  --     | otherwise = do

  --       -- collect all the argument types (the holes ?? we need to fill)
  --       let args = allArgTypes schema :: [SType]

  --       -- recursively call DFS on the arguments' types to get the functions that return those types
  --       let recurse = dfs env messageChan (depth-1) :: SType -> TopDownBackTrack IO RProgram
  --       solutionsPerArg <- mapM recurse args :: TopDownBackTrack IO [RProgram] -- [[a,b,c], [d,e,f]]

  --       -- get cartesian product of all the arguments
  --       let programsPerArg = sequence solutionsPerArg :: [[RProgram]] -- [[a,d], [a,e], [a,f], [b,d], [b,e], [b,f]]

  --       -- fill in arguments of func as RPrograms - [func a d, func a e, func a f, func b d, func b e, func b f]
  --       let formatFn :: [RProgram] -> RProgram
  --           formatFn args = Program { content = PApp id args, typeOf = refineTop env schema }
        
  --       let finalResultList = map formatFn programsPerArg

  --       return finalResultList








-- type PNSolver m = StateT SolverState m
-- type BackTrack m = LogicT (PNSolver m)
type TopDownBackTrack m = LogicT (StateT CheckerState m)
-- evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m [a]
-- evalTopDownBackTrack messageChan action = observeAllT action `evalStateT` (emptyChecker { _checkerChan = messageChan })
evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m [a]
evalTopDownBackTrack messageChan action = fmap (\x -> [x]) $ observeT action `evalStateT` (emptyChecker { _checkerChan = messageChan })

-- type CompsSolver m = StateT Comps (StateT CheckerState m)


--
-- gets list of components/functions that unify with a given type
-- 
-- getUnifiedFunctions :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> CompsSolver IO [(Id, SType)]
getUnifiedFunctions' :: Environment -> Chan Message -> TopDownBackTrack IO (Id, RSchema) -> SType -> TopDownBackTrack IO (Id, SType)

-- MAYBE HOW TO DO THIS ????????????????????
-- existingList `mplus` (return singleElement) 
-- getUnifiedFunctions' _ _ [] _ = mzero

getUnifiedFunctions' env messageChan func goalType = do
    (id, schema) <- func
    (freshVars, st') <- lift $ do

      freshVars <- freshType (env ^. boundTypeVars) schema
      liftIO $ putStrLn $ "Consumed a value: " ++ id ++ ", with goalType: " ++ (show goalType)

      let t1 = shape (lastType freshVars) :: SType
      let t2 = goalType :: SType

      modify $ set isChecked True
      modify $ set typeAssignment Map.empty

      solveTypeConstraint env t1 t2 :: StateT CheckerState IO ()
      st' <- get
      
      return (freshVars, st') :: StateT CheckerState IO (RType, CheckerState)

    let sub =  st' ^. typeAssignment
    let checkResult = st' ^. isChecked

    let schema' = stypeSubstitute sub (shape freshVars)

    -- if it unifies, add that particular unified compoenent to state's list of components
    -- getUnifiedFunctions' env messageChan ys goalType `mplus`
    --   if (checkResult) 
    --     then (return (id, schema'))
    --     else mzero
    guard checkResult
    return (id, schema')





-- getUnifiedFunctions' :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> TopDownBackTrack IO (Id, SType) -> TopDownBackTrack IO (Id, SType)

-- -- MAYBE HOW TO DO THIS ????????????????????
-- -- existingList `mplus` (return singleElement) 
-- getUnifiedFunctions' _ _ [] _ unifiedFuncs = unifiedFuncs

-- getUnifiedFunctions' env messageChan ( v@(id, schema) : ys) goalType unifiedFuncs = do
--     (freshVars, st') <- lift $ do

--       freshVars <- freshType (env ^. boundTypeVars) schema
--       liftIO $ putStrLn $ "Consumed a value: " ++ id ++ ", with goalType: " ++ (show goalType)

--       let t1 = shape (lastType freshVars) :: SType
--       let t2 = goalType :: SType

--       modify $ set isChecked True
--       modify $ set typeAssignment Map.empty

--       solveTypeConstraint env t1 t2 :: StateT CheckerState IO ()
--       st' <- get
      
--       return (freshVars, st') :: StateT CheckerState IO (RType, CheckerState)

--     let sub =  st' ^. typeAssignment
--     let checkResult = st' ^. isChecked

--     let schema' = stypeSubstitute sub (shape freshVars)

--     -- if it unifies, add that particular unified compoenent to state's list of components
--     getUnifiedFunctions' env messageChan ys goalType $
--       if (checkResult) 
--         then unifiedFuncs `mplus` (return (id, schema'))
--         else unifiedFuncs

