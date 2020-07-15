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

: SearchParams -> Goal -> [Example] -> Chan Message -> IO ()
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

    writeChan messageChan (MesgClose CSNormal)
    return ()

type CompsSolver m = StateT CheckerState (LogicT (StateT Comps m))

evalCompsSolverList :: Monad m => Chan Message -> [CompsSolver m a] -> m a
evalCompsSolverList messageChan m = do
  (`evalStateT` emptyComps) $ observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m

-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO ()
iterativeDeepening env messageChan searchParams examples goal = evalCompsSolverList messageChan (map helper [0..]) >> return ()
  where
    -- -- used for figuring out which programs to filter out (those without all arguments)
    -- numArgs :: Int
    -- numArgs = length $ filterOutTypeClass $ Map.elems $ env ^. arguments

    -- -- filters out type classes (@@type_class@@) so that numArgs can be correct when used
    -- -- in filterParams
    -- filterOutTypeClass :: [RSchema] -> [RSchema]
    -- filterOutTypeClass xs = filter (not . \x -> "@@" `isInfixOf` (show x)) xs

    -- filters out type classes (@@type_class@@) so that numArgs can be correct when used
    -- in filterParams
    filterOutTypeClass :: [Id] -> [Id]
    filterOutTypeClass xs = filter (not . \x -> "tc" `isPrefixOf` (show x)) xs

  
    -- calls dfs at a certain depth and checks to see if there is a solution
    helper :: Int -> CompsSolver IO RProgram
    helper depth = do
      
      -- liftIO $ printf "num args: %d\n" numArgs
      -- liftIO $ printf "args: %s\n" (show $ Map.elems (env ^. arguments))
      liftIO $ printf "running dfs on %s at depth %d\n" (show goal) depth
      -- liftIO $ print $ Map.keys $ env ^. arguments
      let goalType = shape $ lastType (toMonotype goal) :: SType
      solution <- dfs env messageChan depth goalType :: CompsSolver IO RProgram
      
      isChecked <- liftIO $ check' solution
      guard isChecked -- gets the first valid program

      return solution
    
    -- wrapper for `check` function
    check' :: RProgram -> IO Bool
    check' program = do
      -- printf "omg we are checking this program: %s\n" (show program)
      -- liftIO $ printf "about to filter program: %s\n" $ show program
      let blah = filterParams program
      if blah
        
        then do
          liftIO $ printf "\t\tthis has all the args: %s\n" $ show program

          checkResult <- evalStateT (check env searchParams examples program goal messageChan) emptyFilterState
          case checkResult of
            Nothing  -> return False
            Just exs -> do
              out <- toOutput env program exs
              printResult $ encodeWithPrefix out
              return True
        else do
          liftIO $ printf "\t\tthis doesn't have all the args: %s\n" $ show program
          return False

-- running dfs on (Int -> (Int -> Int)) at depth 0
-- running dfs on (Int -> (Int -> Int)) at depth 1
--                 this has all the args: Data.Bool.bool arg0 arg1 Data.Bool.False
--                 this has all the args: Data.Bool.bool arg0 arg1 Data.Bool.True
--                 this has all the args: Data.Bool.bool arg0 arg1 Data.Bool.otherwise
--                 this has all the args: Data.Bool.bool arg1 arg0 Data.Bool.False
--                 this has all the args: Data.Bool.bool arg1 arg0 Data.Bool.True
--                 this has all the args: Data.Bool.bool arg1 arg0 Data.Bool.otherwise
--                 this has all the args: Data.Function.const arg0 arg1
--                 this has all the args: Data.Function.const arg1 arg0
-- running dfs on (Int -> (Int -> Int)) at depth 2
--                 this has all the args: ([] !! arg0) !! ([] !! arg1)
-- RESULTS:{"outCandidates":[{"outExamples":[],"solution":"\\arg0 arg1 -> ([] !! arg0) !! ([] !! arg1)"}],"outDocs":[{"functionSig":"[a] -> Int -> a","functionName":"(!!)","functionDesc":"List index (subscript) operator, starting from 0. It is an instance of\nthe more general genericIndex, which takes an index of any\nintegral type.\n"},{"functionSig":"IntMap a","functionName":"Nil","functionDesc":""},{"functionSig":"Int","functionName":"arg0","functionDesc":""},{"functionSig":"Int","functionName":"arg1","functionDesc":""}],"outError":""}
-- Computation time: 1.908 sec



-- stack run -- hplus --json='{"query": "arg1: Bool -> arg0: Int -> Int", "inExamples": []}'
-- running dfs on (Bool -> (Int -> Int)) at depth 0
-- running dfs on (Bool -> (Int -> Int)) at depth 1
--                 this has all the args: Data.Bool.bool arg0 arg0 arg1
-- RESULTS:{"outCandidates":[{"outExamples":[],"solution":"\\arg0 arg1 -> Data.Bool.bool arg0 arg0 arg1"}],"outDocs":[{"functionSig":"a -> a -> Bool -> a","functionName":"bool","functionDesc":"Case analysis for the Bool type. bool x y p\nevaluates to x when p is False, and evaluates\nto y when p is True.\n\nThis is equivalent to if p then y else x; that is, one can\nthink of it as an if-then-else construct with its arguments reordered.\n\nExamples\n\nBasic usage:\n\n\n>>> bool \"foo\" \"bar\" True\n\"bar\"\n\n>>> bool \"foo\" \"bar\" False\n\"foo\"\n\n\nConfirm that bool x y p and if p then y else\nx are equivalent:\n\n\n>>> let p = True; x = \"bar\"; y = \"foo\"\n\n>>> bool x y p == if p then y else x\nTrue\n\n>>> let p = False\n\n>>> bool x y p == if p then y else x\nTrue\n\n"},{"functionSig":"Int","functionName":"arg0","functionDesc":""},{"functionSig":"Bool","functionName":"arg1","functionDesc":""}],"outError":""}
-- "solution":"\\arg0 arg1 -> Data.Bool.bool arg1 arg1 arg0"

-- running dfs on (Bool -> (Int -> Int)) at depth 0
-- running dfs on (Bool -> (Int -> Int)) at depth 1
--                 this has all the args: Data.Bool.bool b b Data.Bool.False
--                 this has all the args: Data.Bool.bool b b Data.Bool.True
--                 this has all the args: Data.Bool.bool b b Data.Bool.otherwise
--                 this has all the args: Data.Bool.bool b b a
-- "solution":"\\a b -> Data.Bool.bool b b a"







    -- determines if the result has all the appropriate arguments
    filterParams :: RProgram -> Bool
    -- filterParams program = True
    filterParams program = all (`isInfixOf` (show program)) $ Map.keys $ env ^. arguments
    -- filterParams 0       _ = error "filterParams error: shouldn't have 0 args!" -- TODO maybe should be true here? 
    -- filterParams 1       x = "arg0" `isInfixOf` (show x)
    -- filterParams numArgs x = isInfixOf ("arg" ++ (show (numArgs - 1))) (show x) && filterParams (numArgs - 1) x

-- arg2: (a -> b) -> arg1: (a -> c) -> arg0: a -> (b, c)
-- \arg0 arg1 arg2 -> ((arg0 arg2) , (arg1 arg2))

--
-- does DFS stuff
--
dfs :: Environment -> Chan Message -> Int -> SType -> CompsSolver IO RProgram
dfs env messageChan depth goalType = do
  
  -- collect all the component types (which we might use to fill the holes)
  component <- choices $ Map.toList (env ^. symbols)

  -- stream of components that unify with goal type
  (id, schema) <- getUnifiedComponents env messageChan component goalType :: CompsSolver IO (Id, SType)
  
  -- stream of solutions to the (id, schema) returned from getUnifiedComponents
  case () of -- hack to use guards
  
    _ | isGround schema -> return Program { content = PSymbol id, typeOf = refineTop env schema }
      | depth == 0 -> mzero  -- stop if depth is 0
      | otherwise -> do

        -- collect all the argument types (the holes ?? we need to fill)
        let args = allArgTypes schema :: [SType]

        -- do basically this:
        -- dfsstuff0 <- dfs ... arg0 :: RProgram
        -- dfsstuff1 <- dfs ... arg1 :: RProgram
        -- dfsstuff2 <- dfs ... arg2 :: RProgram
        -- argsFilled = [dfsstuff0, dfsstuff1, dfsstuff2]
        argsFilled <- mapM (dfs env messageChan (depth - 1)) args :: CompsSolver IO [RProgram]
        return Program { content = PApp id argsFilled, typeOf = refineTop env schema } 
        
  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True
    
    -- converts [a] to a Logic a
    choices :: MonadPlus m => [a] -> m a
    choices = msum . map return

--
-- Given a component (id, schema) like ("length", <a>. [a] -> Int)
--
getUnifiedComponents :: Environment -> Chan Message -> (Id, RSchema) -> SType -> CompsSolver IO (Id, SType)
getUnifiedComponents env messageChan (id, schema) goalType = do
    
    freshVars <- freshType (env ^. boundTypeVars) schema

    let t1 = shape (lastType freshVars) :: SType
    let t2 = goalType :: SType

    solveTypeConstraint env t1 t2 :: CompsSolver IO ()
    st' <- get
    
    let sub = st' ^. typeAssignment
    let checkResult = st' ^. isChecked

    let schema' = stypeSubstitute sub (shape freshVars)

    -- if it unifies, add that particular unified compoenent to state's list of components
    if (checkResult)
      then return (id, schema')
      else mzero
