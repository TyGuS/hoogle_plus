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
      -- let args = rawEnv ^. arguments
      -- let hoArgs = Map.filter (isFunctionType . toMonotype) args
      -- let hoFuns = map (\(k, v) -> (k ++ hoPostfix, withSchema toFunType v)) (Map.toList hoArgs)
      -- return $ rawEnv { 
      --     _symbols = rawSyms `Map.union` Map.fromList hoFuns, 
      --     _hoCandidates = hoCands ++ map fst hoFuns
      --     }
    --------------------------

      let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
      return $ rawEnv {
          _symbols = Map.withoutKeys syms $ Set.fromList hoCands, 
          _hoCandidates = []
          }

    -- call dfs with iterativeDeepening
    iterativeDeepening envWithHo messageChan searchParams examples goalType

    writeChan messageChan (MesgClose CSNormal)
    return ()


type TopDownSolver m = StateT CheckerState (LogicT m)

evalTopDownSolverList :: Monad m => Chan Message -> [TopDownSolver m a] -> m a
evalTopDownSolverList messageChan m = do
  observeT $ msum $ map (`evalStateT` emptyChecker {_checkerChan = messageChan}) m

-- 
-- try to get solutions by calling dfs on depth 0, 1, 2, 3, ... until we get an answer
--
iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO ()
iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan (map helper [1..]) >> return ()
-- iterativeDeepening env messageChan searchParams examples goal = evalTopDownSolverList messageChan (map helper [5..]) >> return ()
  where
    -- filters out type classes (@@type_class@@) so that numArgs can be correct when used
    -- in filterParams
    filterOutTypeClass :: [Id] -> [Id]
    filterOutTypeClass xs = filter (not . \x -> "tc" `isPrefixOf` (show x)) xs

  
    -- calls dfs at a certain depth and checks to see if there is a solution
    helper :: Int -> TopDownSolver IO RProgram
    helper quota = do
      -- liftIO $ printf "running dfs on %s at size %d\n" (show goal) quota

      let goalType = shape $ lastType (toMonotype goal) :: SType
      solution <- dfs env messageChan quota goalType :: TopDownSolver IO RProgram
      
      -- liftIO $ printf "solution: %s\n" (show solution)
      isChecked <- liftIO $ check' solution
      guard isChecked -- gets the first valid program

      return solution
    
    -- wrapper for `check` function
    check' :: RProgram -> IO Bool
    check' program = do
      -- printf "omg we are checking this program: %s\n" (show program)
      let blah = filterParams program
      if blah
        
        then do
          liftIO $ printf "program: %s\n" $ show program
          
          checkResult <- evalStateT (check env searchParams examples program goal messageChan) emptyFilterState
          case checkResult of
            Nothing  -> return False
            Just exs -> do
              out <- toOutput env program exs
              printResult $ encodeWithPrefix out
              return True
        else do
          -- liftIO $ printf "\t\tthis doesn't have all the args: %s\n" $ show program
          return False

    -- determines if the result has all the appropriate arguments
    filterParams :: RProgram -> Bool
    filterParams program = all (`isInfixOf` (show program)) $ Map.keys $ env ^. arguments

--
-- does DFS stuff, with max size of program given as a quota
--
dfs :: Environment -> Chan Message -> Int -> SType -> TopDownSolver IO RProgram
dfs env messageChan quota goalType
  | quota <= 0 = mzero
  | otherwise  = do
    -- collect all the component types (which we might use to fill the holes)
    component <- choices $ Map.toList (env ^. symbols)

    -- guard (not $ "Data.Function.&" `isInfixOf` fst component)
    -- -- \xs x -> Data.List.foldr ($) x xs
    -- guard (not $ "Data.Function.$" `isInfixOf` fst component)
    -- guard (not $ "Data.Function.." `isInfixOf` fst component)
    -- guard (not $ "GHC.List.!!" `isInfixOf` fst component)
    -- guard ("Data.Function.$" `isInfixOf` fst component 
    --     || "Data.Tuple.fst" `isInfixOf` fst component 
    --     || "Data.Tuple.snd" `isInfixOf` fst component 
    --     || "arg" `isInfixOf` fst component 
    --     || "Pair" `isInfixOf` fst component)

    -- if goalType is an arrow type a->b, turn it into a Fun a b
    -- let goalType' = goalType
    -- let goalType' =
    --       if (isFunctionType goalType)
    --         then shape $ toFunType $ refineTop env goalType
    --               -- TODO or we can call DFS to synthesize \x -> ...
    --               -- see the cartProduct test
    --         else goalType

    -- dfsResults <- dfs ...
    
    -- stream of components that unify with goal type
    (id, schema) <- getUnifiedComponents component :: TopDownSolver IO (Id, SType)
    
    -- newList <- dfsResults1, getUnifiedComponents1, dfsResults2, getUnifiedComponents2
    
    -- remove _n'ho' from higher order things
    -- ("(Data.Eq./=)_0'ho'",<a> . (@@hplusTC@@Eq (a) -> (a -> Fun (a) (Bool))))
    -- ("(Data.Eq./=)_1'ho'",<a> . (@@hplusTC@@Eq (a) -> Fun (a) ((Fun (a) (Bool)))))
    -- ("(Data.Eq./=)_2'ho'",<a> . Fun ((@@hplusTC@@Eq (a))) ((Fun (a) ((Fun (a) (Bool))))))
    --  TODO fix this later
    let id' = if "'ho'" `isSuffixOf` id
          then 
            let noHO = reverse $ drop 4 $ reverse id in-- hack to remove 'ho'
            -- function_1 !! 0
            if '_' == (reverse noHO !! 1)
              then reverse $ drop 2 $ reverse noHO
              else noHO
          else id

    -- stream of solutions to the (id, schema) returned from getUnifiedComponents
    if isGround schema
      then return Program { content = PSymbol id', typeOf = refineTop env schema }
      else do
        -- collect all the argument types (the holes ?? we need to fill)
        let args = allArgTypes schema :: [SType]

        -- do basically this:
        -- dfsstuff0 <- dfs ... arg0 (quota - 1) :: RProgram
        -- dfsstuff1 <- dfs ... arg1 (quota - 1 - sizeOf dfsstuff0) :: RProgram
        -- dfsstuff2 <- dfs ... arg2 (quota - 1 - sizeOf dfsstuff0 - sizeOf dfsstuff1) :: RProgram
        -- argsFilled = [dfsstuff0, dfsstuff1, dfsstuff2]
        let func :: (Int, [RProgram]) -> SType -> TopDownSolver IO (Int, [RProgram])
            func (quota', programs) arg = do
              program <- dfs env messageChan quota' arg
              return (quota' - sizeOf program, programs ++ [program])

        (_, argsFilled) <- foldM func (quota - 1, []) args :: TopDownSolver IO (Int, [RProgram])

        return Program { content = PApp id' argsFilled, typeOf = refineTop env schema } 
      
  where
    -- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
    isGround :: SType -> Bool
    isGround (FunctionT id arg0 arg1) = False
    isGround _ = True
    
    -- converts [a] to a Logic a
    choices :: MonadPlus m => [a] -> m a
    choices = msum . map return

    -- gets the size of a program, used for checking quota
    sizeOf :: RProgram -> Int
    sizeOf = length . words . show

    -- Given a component (id, schema) like ("length", <a>. [a] -> Int)
    getUnifiedComponents :: (Id, RSchema) -> TopDownSolver IO (Id, SType)
    getUnifiedComponents (id, schema) = do
      
      freshVars <- freshType (env ^. boundTypeVars) schema

      let t1 = shape (lastType freshVars) :: SType
      let t2 = goalType :: SType

      solveTypeConstraint env t1 t2 :: TopDownSolver IO ()
      st' <- get
      
      let sub = st' ^. typeAssignment
      let checkResult = st' ^. isChecked
      -- when (show goalType == "a") $ do
      -- -- when (show goalType == "Fun (a) (b)") $ do
      -- -- when (isInfixOf "Pair" id && ) $ do
      -- -- when ((isInfixOf "Pair" id) || (isInfixOf "arg" id && (show goalType) == "b")) $ do
      -- -- when True $ do
      -- (liftIO $ printf "unifying (%s) with (%s, %s), " (show goalType) (show id) (show freshVars))
      -- (liftIO $ printf "isChecked: %s, sub: %s\n" (show checkResult) (show sub))

      -- if it unifies, add that particular unified compoenent to state's list of components
      guard checkResult
      return (id, stypeSubstitute sub (shape freshVars))
