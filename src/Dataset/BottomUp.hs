module Dataset.BottomUp
  (
    Generator(..)
  , runGenerateT
  , generate
  , generateApp
  , assignArgs
  ) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.List
import Data.Maybe (maybe)

import Control.Monad.Extra
import Data.Tuple.Extra
import Pipes
import qualified Pipes.Prelude as P

import Database.Dataset
import Types.Type
import Types.Program hiding (canonicalize)
import Types.Fresh
import Types.TypeChecker
import Types.Pretty
import Types.Common
import Dataset.Configuration
import Dataset.HyperGraph (HyperGraph, Edge(..))
import qualified Dataset.HyperGraph as HG

import Debug.Trace

data Generator = Generator {
  nameCounter :: Map Id Int,
  graph :: HyperGraph
}

instance Monad m => Fresh Generator m where
  nextCounter prefix = do
    counters <- gets nameCounter
    let counter = Map.findWithDefault 0 prefix counters
    modify $ \s -> s { nameCounter = Map.insert prefix (counter + 1) counters }
    return counter

type GenerateT m = ListT (StateT Generator m)

runGenerateT :: Monad m => Generator -> GenerateT m a -> m HyperGraph
runGenerateT genState g = graph <$> execStateT (runListT g) genState

-- Note that this algorithm cannot apply something to `head []`, should we support this?
generateApp :: Monad m => HyperGraph -> GenerateT m (Edge, TypeSkeleton)
generateApp g = do
  -- trace ("generateApp with a graph of size " ++ show (Map.size g)) $ return ()
  (t, es) <- Select $ each $ filter (isFunctionType . fst) (Map.toList g)
  Select (each es) >>= go t
  where
    go :: Monad m => TypeSkeleton -> Edge -> GenerateT m (Edge, TypeSkeleton)
    go typ e = do
      typ' <- lift $ freshType [] typ
      let FunctionT _ tArg tRet = typ'
      (tArg', subst) <- unifiedTerms g tArg
      if typeDepth tArg' > 4 then
        mempty
       else do
        let tRet' = canonicalize $ typeSubstitute subst tRet
        let e' = Edge (edgeSymbol e) (edgeChildren e ++ [tArg'])
        -- trace ("insert " ++ plainShow tRet' ++ " for " ++ plainShow e') $ return ()
        return (e', tRet')

    unifiedTerms :: Monad m => HyperGraph -> TypeSkeleton -> GenerateT m (TypeSkeleton, TypeSubstitution)
    unifiedTerms g t = do
      (typ, _) <- Select $ each (Map.toList g)
      if typeSize typ > 4 then
        mempty
       else
        case getUnifier [] [UnifiesWith typ t] of
          Nothing -> mempty
          Just subst -> return (typ, subst)

generate :: Monad m => [(Text, SchemaSkeleton)] -> Int -> m HyperGraph
generate components n = do
    let inits = map (uncurry constEdge) components ++ intLits ++ charLits
    let initGraph = foldr (uncurry HG.insert) Map.empty inits
    graph <$> execStateT (go n) (Generator Map.empty initGraph)
  where
    go :: Monad m => Int -> StateT Generator m ()
    go n | n <= 0 = return ()
         | otherwise = do g <- gets graph
                          es <- P.toListM (every $ generateApp g)
                          modify $ \s -> s { graph = foldr (uncurry HG.insert) g es }
                          go (n-1)

    constEdge :: Text -> SchemaSkeleton -> (Edge, TypeSkeleton)
    constEdge x t = (Edge x [], canonicalize $ toMonotype t)

assignArgs :: Monad m => Configuration -> TProgram -> GenerateT m TProgram
assignArgs config program = mkLambda <$> go program
  where
    go :: Monad m => TProgram -> GenerateT m (TProgram, [Id])
    go prog@(Program p t) = case p of
      PSymbol _ -> do
        arg <- lift $ freshId [] "arg"
        msum $ map return [(prog, []), (Program (PSymbol arg) t, [arg])]
      PApp f args -> (do
          arg <- lift $ freshId [] "arg"
          return (Program (PSymbol arg) t, [arg])
        ) `mplus` (do
          argsList <- sequence $ map go args
          -- trace ("argsList: " ++ show argsList ++ " for " ++ show f) $ return ()
          let (args', binds) = (map fst argsList, concatMap snd argsList)
          let withFname = (Program (PApp f args') t, binds)
          return withFname `mplus` mkApp t args' binds
        )

      PFun x body -> do
        (e, xs) <- go body -- note that x may become an argument, should we prevent that?
        arg <- lift $ freshId [] "arg"
        msum (map return [(prog, []), (Program (PSymbol arg) t, [arg])])
          `mplus` return (Program (PFun x e) t, xs)

      _ -> error "unsupported expression in assignArgs"

    mkApp :: Monad m => TypeSkeleton -> [TProgram] -> [Id] -> GenerateT m (TProgram, [Id])
    mkApp tRet args binds
      | 1 + length binds > maxArgs config = mempty
      | otherwise = do
        arg <- lift $ freshId [] "arg"
        args' <- Select $ each $ init $ tails args
        return (Program (PApp arg args') tRet, arg:binds)

    mkLambda :: (TProgram, [Id]) -> TProgram
    mkLambda (p, args) = let args' = filter (`Set.member` (symbolsOf p)) args
                          in foldr (\arg body -> untyped (PFun arg body)) p args'

--------------------------------------------------------------------------------
----------                     Literal Expressions                    ----------
--------------------------------------------------------------------------------
-- strLits :: [TProgram]
-- strLits = map ((`ann` listType charType) . varp) ["str"] -- ["a", "A", "Aa", "bB"]

intLits :: [(Edge, TypeSkeleton)]
intLits = map (\s -> (Edge s [], intType)) ["0"] -- [-1, 0, 1]

charLits :: [(Edge, TypeSkeleton)]
charLits = map (\s -> (Edge s [], charType)) ["'a'"] -- ['a', ' ', 'B', '+']