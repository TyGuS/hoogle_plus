{-# LANGUAGE FlexibleContexts #-}

module Synquid.Graph where

import Synquid.Type hiding (set)
import Synquid.Util
import Synquid.Logic
import Synquid.Program
import Synquid.Succinct
import Synquid.Pretty
import Database.Util
import PetriNet.AbstractType

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Serialize (Serialize)
import Data.Aeson
import qualified Data.Serialize as S
import Control.Monad.State
import Control.Applicative hiding (empty)
import Control.Lens
import Debug.Trace
import qualified Data.Text as Text
import qualified Data.Vector as Vector

type NameCount = Map Id Int

-- | Replace all bound type variables with fresh free variables
instantiate :: (Monad s) => RSchema -> StateT NameCount s RType
instantiate sch = instantiate' Map.empty sch
  where
    instantiate' subst (ForallT (a,_) sch) = do
      a' <- freshId "A"
      instantiate' (Map.insert a (vart a' (BoolLit True)) subst) sch       
    instantiate' subst (Monotype t) = return $ typeSubstitute subst $ t
    freshId pre = do
      nameCnt <- get
      let cnt = Map.findWithDefault 0 pre nameCnt
      put $ Map.insert pre (cnt+1) nameCnt
      return $ pre ++ show cnt

addGraphSymbol :: Monad s => Id -> RSchema -> Environment -> StateT NameCount s Environment
addGraphSymbol name sch env = do
  newt <- instantiate sch
  let succinctTy = toSuccinctType newt
  traceShow (plain $ text "Adding" <+> text name <+> text "::" <+> pretty succinctTy <+> text "for" <+> pretty newt) $ return ()
  -- traceShow (plain $ text "polysymbols" <+> pretty (HashMap.size $ HashMap.filter isSuccinctAll $ env ^. succinctSymbols)) $ return ()
  case newt of
    (LetT id tDef tBody) -> do
      env' <- addGraphSymbol id (Monotype tDef) env
      addGraphSymbol name (Monotype tBody) env'
    _ -> return $ addEdgeForSymbol name succinctTy env

addEdgeForSymbol :: Id -> SuccinctType -> Environment -> Environment
addEdgeForSymbol name succinctTy env = let
  envWithSelf = addEdge name succinctTy $ (succinctSymbols %~ HashMap.insert name succinctTy) env
  in iteration env envWithSelf
  where
    allConcreteNodes env = Set.filter isSuccinctConcrete $ allSuccinctNodes env
    polySymbols env = HashMap.filter isSuccinctAll (env ^. succinctSymbols)
    newArrivedNodes oldEnv newEnv = allConcreteNodes newEnv `Set.difference` allConcreteNodes oldEnv
    iteration oldEnv newEnv = 
      if Set.null $ newArrivedNodes oldEnv newEnv
        then newEnv
        else iteration newEnv $ HashMap.foldrWithKey (addPolyEdge $ newArrivedNodes oldEnv newEnv) newEnv $ polySymbols newEnv

addPolyEdge :: Set SuccinctType -> Id -> SuccinctType -> Environment -> Environment
addPolyEdge targets name (SuccinctAll idSet ty) env 
  | isAllBound env idSet = addEdge name ty env -- if all the type vars are bound in the env, treat it as none-all type
  | otherwise = Set.foldr (\sty accEnv ->
    let (unified, substitutions) = unifySuccinct (lastSuccinctType ty) sty (accEnv ^. boundTypeVars)
        substedTys = Set.fromList $ map (flip succinctTypeSubstitute ty) substitutions
    in if unified then Set.foldr (\t e -> addEdge name (generalizeFV e t) e) accEnv substedTys else accEnv
    ) env targets

-- | helper function: check whether all type variables in @idSet@ are all bound in the @env@
isAllBound :: Environment -> Set Id -> Bool
isAllBound env idSet = Set.foldr ((&&) . isBound env) True idSet

-- | helper function: replace all the unbound type variables in @t@ as SuccinctAny
generalizeFV :: Environment -> SuccinctType -> SuccinctType
generalizeFV env t = 
  let tyVars = extractSuccinctTyVars t `Set.difference` Set.fromList (env ^. boundTypeVars)
  in if Set.size tyVars > 0 
    then succinctTypeSubstitute (Set.foldr (\tv -> Map.insert tv SuccinctAny) Map.empty tyVars) t 
    else t

addEdge :: Id -> SuccinctType -> Environment -> Environment
addEdge name (SuccinctFunction paramCnt argSet retTy) env = 
  let argTy = if paramCnt == 1 then Set.findMin argSet else SuccinctComposite argSet
      addedRevEnv = (succinctGraphRev %~ HashMap.insertWith mergeMapOfSet argTy (HashMap.singleton retTy (Set.singleton $ SuccinctEdge name paramCnt 0.01))) env
      addedRetEnv = (succinctGraph %~ HashMap.insertWith mergeMapOfSet retTy (HashMap.singleton argTy (Set.singleton $ SuccinctEdge name paramCnt 0.01))) addedRevEnv
  in if paramCnt == 1
    then addedRetEnv
    else Set.foldr (\elem acc -> 
      let revEnv = (succinctGraphRev %~ HashMap.insertWith mergeMapOfSet elem (HashMap.singleton argTy (Set.singleton $ SuccinctEdge ("||"++(show argTy)++"||"++(show elem)++"||") 0 0.01))) acc
      in (succinctGraph %~ HashMap.insertWith mergeMapOfSet argTy (HashMap.singleton elem (Set.singleton $ SuccinctEdge ("||"++(show argTy)++"||"++(show elem)++"||") 0 0.01))) revEnv
      ) addedRetEnv argSet
addEdge name typ@(SuccinctAll idSet ty) env = 
  (if isAllBound env (extractSuccinctTyVars (lastSuccinctType ty))
    then addEdge name (generalizeFV env ty)
    else id) $ addPolyEdge (Set.filter isSuccinctConcrete (allSuccinctNodes env)) name typ env
addEdge name typ env = 
  let inhabitedEnvRev = (succinctGraphRev %~ HashMap.insertWith mergeMapOfSet (SuccinctInhabited typ) (HashMap.singleton typ (Set.singleton $ SuccinctEdge name 0 0.01))) env
      inhabitedEnv = (succinctGraph %~ HashMap.insertWith mergeMapOfSet typ (HashMap.singleton (SuccinctInhabited typ) (Set.singleton $ SuccinctEdge name 0 0.01))) inhabitedEnvRev
  in inhabitedEnv

allSuccinctNodes :: Environment -> Set SuccinctType
allSuccinctNodes env = Set.fromList $ (HashMap.keys (env ^. succinctGraph)) ++ (HashMap.foldr (\m acc -> acc ++ (HashMap.keys m)) [] (env ^. succinctGraph))

isReachable :: Environment -> SuccinctType -> Bool
isReachable env typ = isReachableHelper (env ^. succinctGraph) Set.empty typ
  where
    isReachableHelper g visited typ' = case typ' of
      SuccinctInhabited _ -> True
      SuccinctAny -> True
      SuccinctComposite tys -> Set.foldr (\t acc -> acc && isReachableHelper g (Set.insert typ' visited) t) True tys
      _ -> HashMap.foldrWithKey (\i _ acc -> acc || isReachableHelper g (Set.insert typ' visited) i) False (if Set.member typ' visited then HashMap.empty else HashMap.lookupDefault HashMap.empty typ' g)

getReachableNodes :: SuccinctGraph -> [SuccinctType] -> Set SuccinctType
getReachableNodes graph starters = 
  getReachableNodesHelper graph Set.empty [] starters
  where
    isCompositeReachable reachableSet typ = case typ of
      SuccinctComposite tySet -> Set.foldr (\b acc -> acc && (Set.member b reachableSet)) True tySet
      _ -> True
    getReachableNodesWithoutComposite g visited toVisit = case toVisit of
      [] -> visited
      curr:xs -> if Set.member curr visited
        then getReachableNodesWithoutComposite g visited xs
        else let newVisited = Set.insert curr visited 
          in getReachableNodesWithoutComposite g newVisited (xs ++ (filter (isCompositeReachable newVisited) (HashMap.keys $ HashMap.lookupDefault HashMap.empty curr g)))
    getReachableNodesHelper g visited waitingList toVisit = case toVisit of
      [] -> visited `Set.union` (getReachableNodesWithoutComposite g visited (filter (isCompositeReachable visited) waitingList))
      curr:xs -> if Set.member curr visited 
        then getReachableNodesHelper g visited waitingList xs
        else case curr of
          SuccinctComposite _ -> getReachableNodesHelper g visited (waitingList++[curr]) xs
          _ -> getReachableNodesHelper g (Set.insert curr visited) waitingList (xs ++ (HashMap.keys (HashMap.lookupDefault HashMap.empty curr g)))

reachableGraphFromNode :: Environment -> SuccinctType -> Set SuccinctType
reachableGraphFromNode env goalTy = reachableGraphFromNodeHelper (env ^. succinctGraph) Set.empty startTys
  where
    startTys = (SuccinctScalar BoolT):(Set.toList $ Set.filter (\t -> succinctAnyEq goalTy t) (allSuccinctNodes env))
    isCompositeReachable reachableSet typ = case typ of
      SuccinctComposite tySet -> Set.foldr (\b acc -> acc && (Set.member b reachableSet)) True tySet
      _ -> True
    reachableGraphFromNodeHelper g visited toVisit = case toVisit of
      [] -> visited
      curr:xs -> if Set.member curr visited
        then reachableGraphFromNodeHelper g visited xs
        else reachableGraphFromNodeHelper g (Set.insert curr visited) (xs ++ (HashMap.keys (HashMap.lookupDefault HashMap.empty curr g)))

rmUnreachableComposite :: Environment -> Set SuccinctType -> Set SuccinctType
rmUnreachableComposite env reachableSet = Set.foldr (\t acc -> if isCompositeReachable t then acc else Set.delete t acc) reachableSet (compositeNodes)
  where
    isCompositeNode ty = case ty of
      SuccinctComposite _ -> True
      _ -> False
    compositeNodes = Set.filter isCompositeNode reachableSet
    isCompositeReachable t = let SuccinctComposite tySet = t in 
      Set.foldr (\b acc -> acc && (Set.member b reachableSet)) True tySet

findDstNodesInGraph :: Environment -> SuccinctType -> HashMap SuccinctType (Set SuccinctEdge)
findDstNodesInGraph env typ = case typ of
  SuccinctLet _ _ ty -> findDstNodesInGraph env ty
  SuccinctAll _ ty -> findDstNodesInGraph env ty
  _ -> let
    filter_fun k v = (succinctAnyEq typ k) && (not (isSuccinctComposite k))
    candidateMap = HashMap.filterWithKey filter_fun (env ^. graphFromGoal)
    in HashMap.foldr (\m acc -> HashMap.foldrWithKey (\kty set accM -> HashMap.insertWith Set.union kty set accM) acc m) HashMap.empty candidateMap

pruneGraphByReachability g reachableSet = HashMap.foldrWithKey (\k v acc -> if Set.member k reachableSet then HashMap.insert k (HashMap.filterWithKey (\k' s -> Set.member k' reachableSet) v) acc else acc) HashMap.empty g

type SGraph = HashMap SuccinctType (HashMap SuccinctType (Set SuccinctEdge))
-- type NGraph = HashMap Node (HashMap Node (Set SuccinctEdge))
type SuccinctGraph = HashMap SuccinctType (HashMap SuccinctType (Set SuccinctEdge))

-- toNGraph g = HashMap.foldrWithKey (\k v acc -> HashMap.insert (toNode k) (HashMap.foldrWithKey (\k' v' acc' -> HashMap.insert (toNode k') v' acc') acc v)) HashMap.empty g

data Node = Node {
  typ :: SuccinctType, -- type of the current node
  path :: [Set SuccinctEdge] -- path from the src node, used when we compute the shortest path from some node
} deriving(Eq, Ord)

shortestPathFromTo :: Environment -> SuccinctType -> SuccinctType -> [Set SuccinctEdge]
shortestPathFromTo env src dst = shortestPathHelper (env ^. graphFromGoal) Set.empty [Node src []]
  where
    -- we mark the current path on the graph by the node, how to get here from `src` node
    -- a [src,c] -> b [src,c,a]
    shortestPathHelper g visited toVisit = case toVisit of
      [] -> [] -- error $ "There is no path from " ++ show src ++ " to " ++ show dst
      curr:vs | typ curr == dst -> path curr
              | Set.member (typ curr) visited -> shortestPathHelper g visited vs
              | otherwise -> shortestPathHelper g (Set.insert (typ curr) visited) (vs ++ nodesWithPath curr g)
    nodesWithPath node graph = map (uncurry (makeNode $ path node)) 
                               $ HashMap.toList 
                               $ HashMap.filter (\s -> Set.size s /= 1 || Set.notMember "__goal__" (Set.map getEdgeId s)) 
                               $ HashMap.lookupDefault HashMap.empty (typ node) graph
    makeNode currPath t id = Node t (currPath 
                                 ++ (let s = Set.filter ((/=) "__goal__" . getEdgeId) id 
                                     in if Set.null s then [] else [s]))


instance (Eq k, Hashable k, Serialize k, Serialize v) => Serialize (HashMap k v) where
  put hm = S.put (HashMap.toList hm)
  get = HashMap.fromList <$> S.getListOf S.get

instance Serialize SuccinctType
instance Serialize Formula
instance Serialize Sort
instance Serialize UnOp
instance Serialize BinOp
instance Serialize SuccinctContext
instance Serialize SuccinctEdge
instance Serialize Environment
instance Serialize PredSig
instance Serialize DatatypeDef
instance Serialize MeasureCase
instance Serialize MeasureDef
instance Serialize Metadata
instance Serialize t => Serialize (Case t)
instance Serialize t => Serialize (BareProgram t)
instance Serialize t => Serialize (Program t)
instance Serialize r => Serialize (TypeSkeleton r)
instance Serialize r => Serialize (BaseType r)
instance Serialize r => Serialize (SchemaSkeleton r)
instance Serialize AbstractSkeleton

edges isPruned env = HashMap.foldrWithKey (\k v acc -> (map (\(k',v') -> (k,v',k')) (HashMap.toList v)) ++ acc) [] (if isPruned then env ^. graphFromGoal else env ^. succinctGraph)

nodes isPruned env = Set.fromList $ (HashMap.keys (if isPruned then env ^. graphFromGoal else env ^. succinctGraph)) ++ (HashMap.foldr (\m acc -> acc ++ (HashMap.keys m)) [] (if isPruned then env ^. graphFromGoal else env ^. succinctGraph))

showGraphViz isPruned env =
  "digraph name{\n" ++
  "layout=dot;\n" ++
  "splines=true;\n" ++ 
  "margin=\"0.5,0.5\";\n" ++
  "fontsize=16;\n" ++
  "dpi=250;\n"++
  "concentrate=True;\n" ++
  "rankdir=BT;\n" ++
  "ratio=fill;\n" ++
  "size=\"25,25\";\n" ++
  "node  [style=\"rounded,filled,bold\", shape=box, width=2, fontsize=20];\n"++
  "edge [fontsize=20]\n"++
  (concatMap showNode $ nodes isPruned env) ++
  (concatMap showEdge $ edges isPruned env) ++
  "}\n"
  where showEdge (from, t, to) = "\"" ++ (show from) ++ "\"" ++ " -> " ++ "\"" ++(show to) ++"\"" ++
                                 " [label = \"" ++ (Set.foldr (\e str -> str++","++(show e)) "" t) ++ "\"];\n"
        showNode v = "\"" ++(show v) ++ "\"" ++"\n"