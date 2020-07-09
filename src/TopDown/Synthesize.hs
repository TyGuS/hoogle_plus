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
        
        -- let depth = 7
        start <- getCPUTime

        printf "running bfs on %s\n" (show $ shape destinationType)
      
        let startType = [Program { content = PHole, typeOf = refineTop envWithHo (shape destinationType) } ]
        
        -- bfs :: Environment -> Chan Message -> Int -> [RProgram] -> StateT CheckerState IO RProgram
        
        sol <- bfs envWithHo messageChan numArgs startType `evalStateT` emptyChecker { _checkerChan = messageChan } :: IO RProgram
        -- solution <- evalTopDownBackTrack messageChan $ do
        --   sol <- dfs envWithHo messageChan depth (shape destinationType) :: TopDownBackTrack IO RProgram
          
        --   guard (filterParams numArgs sol)
        --   -- guard (isInfixOf "arg1" (show sol))
          
        --   return sol
        
        -- print the first solution that has all the arguments
        -- mapM print $ take 1 $ filter (filterParams numArgs) solutions

        -- printf "done running dfsTop on %s\n" (show $ shape destinationType)
        printf "\n\n\nSOLUTION: %s\n\n\n" (show sol)
        end <- getCPUTime

        let diff = fromIntegral (end - start) / (10^12)
        printf "Computation time: %0.3f sec\n" (diff :: Double)

        writeChan messageChan (MesgClose CSNormal)
        return ()
        -- https://hackage.haskell.org/package/logict-0.7.0.2/docs/Control-Monad-Logic.html
        -- https://stackoverflow.com/questions/17239511/prologs-returns-in-haskell
        -- runLogic :: Logic a -> (a -> r -> r) -> r -> r



check' :: MonadIO (t m) => Environment -> SearchParams -> [Example] -> RProgram -> RSchema -> Chan Message -> t m ()
check' env params examples program goal msgChan = do
  checkResult <- liftIO $ evalStateT (check env params examples program goal msgChan) emptyFilterState
  case checkResult of
    Just something -> do   
      let exs = fromJust checkResult
      out <- liftIO $ toOutput env program exs
      liftIO $ printResult $ encodeWithPrefix out
    _ -> error "todo" -- recurse

-- [?? :: Goaltype]
-- take a list of programs with holes and return the first ground valid program
bfs :: Environment -> Chan Message -> Int -> [RProgram] -> StateT CheckerState IO RProgram
bfs env messageChan _ [] = error "cannot find solution"
bfs env messageChan numArgs (p:ps) = do
  testPrograms <- testFillFirstHole env messageChan :: StateT CheckerState IO [RProgram]
  liftIO $ mapM print testPrograms
  return undefined

------------
-- bfs env messageChan numArgs (p:ps)
--   | hasHole p = do
--         newHoledPrograms <- fillFirstHole env messageChan p :: StateT CheckerState IO [RProgram]
--         liftIO $ printf "Non-ground program found: %s. adding %d newHoledPrograms\n" (show p) (length newHoledPrograms)
--         bfs env messageChan numArgs (ps ++ newHoledPrograms)
--   | otherwise = do
--         liftIO $ printf "Ground program found: %s\n" (show p)
--         if (filterParams numArgs p) 
--           then return p
--           else bfs env messageChan numArgs ps
-------------



-- If it returns Just something, then check passed (passed examples)

        
        -- TODO check stuff

      -- fillHoles :: Environment -> Chan Message -> RProgram -> StateT CheckerState IO [RProgram]


  

-- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
isGround :: SType -> Bool
isGround (FunctionT id arg0 arg1) = False
isGround _ = True

-- hasHole :: RProgram -> Bool
-- hasHole (Program p _) = case p of
--   PApp fun arg -> or (map hasHole arg)
--   PHole -> True
--   _ -> False

-- converts [a] to a Logic a
choices :: MonadPlus m => [a] -> m a
choices = msum . map return

-- length ?? -> [length (tail ??), length (Nil), length (?? ++ ??)]
getUnifiedFunctions'' :: Environment -> Chan Message -> SType -> [RProgram]
getUnifiedFunctions'' env messageChan goalType = do
  undefined

getUnifiedFunctions''' :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> StateT CheckerState IO [RProgram]
getUnifiedFunctions''' env messageChan []                  goalType = return []
getUnifiedFunctions''' env messageChan ((id, schema) : ys) goalType = do
    -- (id, schema) <- func
    freshVars <- freshType (env ^. boundTypeVars) schema
    
    let t1 = shape (lastType freshVars) :: SType
    let t2 = goalType :: SType

    liftIO $ printf "NewThing: \n\tfreshVars: %s\n\tid: %s\n\tschema: %s\n\tt1: %s\n\tt2: %s\n" 
      (show freshVars) id (show schema) (show t1) (show t2)

-- NewThing: 
--         id: GHC.List.length
--         schema: <a> . ([a] -> Int)
--         t1: Int
--         t2: Int
--         goalType: Int

-- NewThing: 
--         id: GHC.List.sum
--         schema: <a> . (@@hplusTC@@Num (a) -> ([a] -> a))
--         t1: tau68
--         t2: Int
--         goalType: Int


    -- reset previous calls of solveTypeConstraint
    modify $ set isChecked True
    modify $ set typeAssignment Map.empty

    solveTypeConstraint env t1 t2 :: StateT CheckerState IO ()
    st' <- get
    
    -- (freshVars, st') <- do

    --   return (freshVars, st') :: StateT CheckerState IO (RType, CheckerState)

    let sub =  st' ^. typeAssignment
    let checkResult = st' ^. isChecked

    -- if it unifies, add that particular unified compoenent to state's list of components
    if (checkResult) 
      then do 
        let schema' = stypeSubstitute sub (shape freshVars) :: SType

        let args = allArgTypes schema' :: [SType]
        
        let typeToHole :: SType -> RProgram
            typeToHole t = Program { content = PHole, typeOf = refineTop env t } 
        
        let typedHoles = map typeToHole args :: [RProgram]
        let resultingProgram = Program { content = PApp id typedHoles, typeOf = refineTop env schema' } :: RProgram

        liftIO $ printf "unified with: %s\n" (show resultingProgram)
        fmap (resultingProgram :) (getUnifiedFunctions''' env messageChan ys goalType)
      
      else getUnifiedFunctions''' env messageChan ys goalType

{-
do
  chan <- newChan
  do
    solveTypeConstraint emptyEnv (ScalarT (IntT) ()) (ScalarT (IntT) ())
    st <- get
    return $ st ^. isChecked
  `evalStateT` emptyChecker { _checkerChan = chan }

False

newChan >>= \chan -> 
  solveTypeConstraint emptyEnv (ScalarT (IntT) ()) (ScalarT (IntT) ()) >> get >>= \st -> return $ st ^. isChecked
  `evalStateT` emptyChecker { _checkerChan = chan }
-}


-- fill one level of holes for the given RProgram with holes
fillHoles :: Environment -> Chan Message -> RProgram -> StateT CheckerState IO [RProgram]
fillHoles env messageChan (Program (PApp f args) typ) = do
    argUnifications <- mapM (fillHoles env messageChan) args :: StateT CheckerState IO [[RProgram]]
    let cartesianArgs = sequence argUnifications :: [[RProgram]]
    let filledPrograms = [Program { content = PApp f filledArgs, typeOf = typ } | filledArgs <- cartesianArgs]
    return filledPrograms
fillHoles env messageChan (Program PHole typ)         = getUnifiedFunctions''' env messageChan (Map.toList (env ^. symbols)) (shape typ)
fillHoles _   _           program                     = return [program]

-------------

-- data TypeSkeleton r =
--   ScalarT (BaseType r) r |
--   FunctionT Id (TypeSkeleton r) (TypeSkeleton r) |
  
-- data BaseType r = BoolT | IntT | DatatypeT Id [TypeSkeleton r] [r] | TypeVarT Substitution Id


-- -- | Program skeletons parametrized by information stored symbols, conditionals, and by node types
-- data BareProgram t =
--   PSymbol Id |                                -- ^ Symbol (variable or constant)
--   PApp Id [Program t] |              -- ^ Function application
--   PHole |                                     -- ^ Hole (program to fill in)

-- -- | Programs annotated with types
-- data Program t = Program {
--   content :: BareProgram t,
--   typeOf :: t
-- } deriving (Functor, Generic)

-- -- | Refinement-typed programs
-- type RProgram = Program RType

-- -- | Type skeletons (parametrized by refinements)
-- data TypeSkeleton r =
--   ScalarT (BaseType r) r |
--   FunctionT Id (TypeSkeleton r) (TypeSkeleton r) |
--   AnyT |
--   BotT 
--   deriving (Eq, Ord, Generic)

-- -- | Refined types
-- type RType = TypeSkeleton Formula

-------------

testFillFirstHole :: Environment -> Chan Message -> StateT CheckerState IO [RProgram]
testFillFirstHole env messageChan = do

  let intT = ScalarT (IntT) ftrue :: RType
  let funcT a b = FunctionT "f" a b :: RType
  let arg0 = (Program (PSymbol "arg0") intT) :: RProgram
  let intHole = Program PHole intT :: RProgram
  -- f arg0 ?? :: Int
  -- where f :: Int -> Int -> Int
  let p = Program (PApp "f" [arg0, intHole]) (intT) :: RProgram
  fillFirstHole env messageChan p :: StateT CheckerState IO [RProgram]

  -- should give us things like
  -- f arg0 arg0
  -- f arg0 (length ??)
  
  -- fillFirstHole undefined undefined

-- RESULTS
-- f arg0 ((?? :: [Int]) !! (?? :: Int))
-- f arg0 (Data.Bool.bool (?? :: Int) (?? :: Int) (?? :: Bool))
-- f arg0 (Data.Either.fromLeft (?? :: Int) (?? :: Either (Int) (tau23)))
-- f arg0 (Data.Either.fromRight (?? :: Int) (?? :: Either (tau26) (Int)))
-- f arg0 (Data.Function.const (?? :: Int) (?? :: tau37))
-- f arg0 (Data.Function.id (?? :: Int))
-- f arg0 (Data.Maybe.fromJust (?? :: Maybe (Int)))
-- f arg0 (Data.Maybe.fromMaybe (?? :: Int) (?? :: Maybe (Int)))
-- f arg0 (GHC.List.head (?? :: [Int]))
-- f arg0 (GHC.List.last (?? :: [Int]))
-- f arg0 (GHC.List.maximum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int]))
-- f arg0 (GHC.List.minimum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int]))
-- f arg0 (GHC.List.product (?? :: @@hplusTC@@Num (Int)) (?? :: [Int]))
-- f arg0 (GHC.List.sum (?? :: @@hplusTC@@Num (Int)) (?? :: [Int]))
----------------



fillFirstHole :: Environment -> Chan Message -> RProgram -> StateT CheckerState IO [RProgram]
fillFirstHole env messageChan p = increaseDepth 1

  where
    increaseDepth :: Int -> StateT CheckerState IO [RProgram]
    increaseDepth depth = do
      -- liftIO $ printf "fillFirstHole trying depth %d\n" depth
      result <- helper depth p :: StateT CheckerState IO (Maybe [RProgram])
      case result of
        Nothing -> increaseDepth (depth+1)
        Just programs -> return programs

    helper :: Int -> RProgram                     -> StateT CheckerState IO (Maybe [RProgram])
    helper    0      _                            = return Nothing
    helper    1      (Program (PApp _ _   ) _  )  = return Nothing
    helper    _      (Program PHole         typ)  = Just <$> getUnifiedFunctions''' env messageChan (Map.toList (env ^. symbols)) (shape typ)
    helper    depth  (Program (PApp f args) typ)  = do
        case span (\arg -> content arg /= PHole) args of
          (l, (Program PHole _):r) -> do
            unifiedFuncs <- getUnifiedFunctions''' env messageChan (Map.toList (env ^. symbols)) (shape typ) :: StateT CheckerState IO [RProgram]
            return $ Just $ [Program (PApp f (l ++ [func] ++ r)) typ | func <- unifiedFuncs] -- fill first hole with unifiedFunctions
          _            -> do -- need to recurse by applying fillFirstHole on each arg, but stop once we get a Just
            possiblyFilled <- mapM (helper (depth-1)) args :: StateT CheckerState IO [Maybe [RProgram]]
            return $ msum $ possiblyFilled :: StateT CheckerState IO (Maybe [RProgram])
                          -- msum :: [Maybe [RProgram]] -> [RProgram] -- get the first Just
    helper    _      _                            = error "helper: shouldn't be here"

-- TODO add "check" function here
filterParams :: Int -> RProgram -> Bool
filterParams 0       _ = error "filterParams error: shouldn't have 0 args!" -- TODO maybe should be true here? 
filterParams 1       x = "arg0" `isInfixOf` (show x)
filterParams numArgs x = isInfixOf ("arg" ++ (show (numArgs - 1))) (show x) && filterParams (numArgs - 1) x