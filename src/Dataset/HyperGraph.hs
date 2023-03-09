module Dataset.HyperGraph (
  -- * hypergraph definitions and operations
    Node(..)
  , Edge(..)
  , HyperGraph
  , insert

  -- * sample programs from a given hypergraph
  , SampleConfig(..)
  , sample
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import GHC.Generics
import System.Random

import Control.Monad.Extra
import Pipes
import Text.PrettyPrint.ANSI.Leijen (string)

import Types.Common
import Types.Fresh
import Types.Program
import Types.Type
import Types.Pretty

import Debug.Trace

type Node = TypeSkeleton

data Edge = Edge {
  edgeSymbol :: Id,
  edgeChildren :: [Node]
} deriving (Eq, Ord, Generic)

instance Pretty Edge where
  pretty e = string (show e)

instance Show Edge where
  show (Edge s cs) = "Edge (" ++ show s ++ ", " ++ show (map plainShow cs) ++ ")"

type HyperGraph = Map Node (Set Edge)

data SampleConfig m = SampleConfig {
  sampleDepth :: MonadIO m => m Int,
  sampleEdge :: MonadIO m => Set Edge -> m Edge,
  sampleArg :: MonadIO m => Node -> StateT Int m Bool
}

type SampleT m = StateT Int m

insert :: Edge -> TypeSkeleton -> HyperGraph -> HyperGraph
insert e t graph = Map.insertWith Set.union t (Set.singleton e) graph

sample :: MonadIO m => HyperGraph -> SampleConfig m -> m TProgram
sample graph config = do
  depth <- sampleDepth config
  body <- evalStateT (sampleProgram graph config depth TopT) 0
  let args = Set.toList $ Set.filter (Text.isPrefixOf "arg") (symbolsOf body)
  return (mkLambda body args)
  where
    mkLambda :: TProgram -> [Id] -> TProgram
    mkLambda = foldr (\arg p -> untyped $ PFun arg p)

sampleProgram :: MonadIO m => HyperGraph -> SampleConfig m -> Int -> Node -> SampleT m TProgram
sampleProgram graph config depth node =
  ifM (sampleArg config node)
      (varp <$> freshId [] "arg")
      (sampleApp graph config depth node)

sampleApp :: MonadIO m => HyperGraph -> SampleConfig m -> Int -> Node -> SampleT m TProgram
sampleApp graph config depth node = do
  let es = case node of
            TopT -> Set.unions (Map.elems graph)
            _ -> Map.findWithDefault Set.empty node graph
  let (nulls, edges) = Set.partition ((== 0) . length . edgeChildren) es
  let candidates = if depth == 0 then nulls else edges
  if Set.null candidates then do
    varp <$> freshId [] "arg"
   else do
    e <- lift $ sampleEdge config candidates
    -- trace ("sample edge " ++ show e ++ " for type " ++ plainShow node) $ return ()
    -- ensure at least one of the children has depth of (depth - 1)
    i <- randomRIO (0, length (edgeChildren e) - 1)
    depthPartitions <- depthes (depth - 1) i (length $ edgeChildren e)
    args <- zipWithM (sampleProgram graph config) depthPartitions (edgeChildren e)
    if depth == 0 then do
      r <- randomRIO (0, 1)
      if isConstant (edgeSymbol e) && r > (0.25 :: Double) then
        varp <$> freshId [] "arg"
      else
        return (varp $ edgeSymbol e)
    else
      return (untyped $ PApp (edgeSymbol e) args)
  where
    isConstant :: Text.Text -> Bool
    isConstant s = (isUpper $ Text.head $ last (Text.split (== '.') s)) || s == "0" || s == "'a'"

    depthes :: MonadIO m => Int -> Int -> Int -> m [Int]
    depthes _ _ 0 = return []
    depthes upper i len = do
      d <- if i == len - 1 then return upper else randomRIO (0, upper)
      (d:) <$> depthes upper i (len - 1)