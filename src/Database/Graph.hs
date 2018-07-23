module Database.Graph where

import System.Environment
import Data.List.Extra
import Data.Maybe
import Data.Either
import Control.Monad.State
import Language.Haskell.Exts
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Debug.Trace

import Synquid.Succinct
import Synquid.Pretty
import Synquid.Type
import Synquid.SolverMonad
import Synquid.Program hiding (TypeDecl)
import Synquid.Util
import Synquid.Logic
import Database.Convert
import Database.Generate
import Database.Util
import Database.Download

renameSigs :: String -> [Entry] -> [Entry]
renameSigs _ [] = []
renameSigs currModule (decl:decls) = case decl of
    EModule mdl -> decl:(renameSigs mdl decls)
    EPackage _ -> decl:(renameSigs currModule decls)
    EDecl (TypeSig _ names ty) -> (EDecl (TypeSig () (map (prependName currModule) names) ty)):(renameSigs currModule decls)
    _ -> decl:(renameSigs currModule decls)

addSynonym :: [Entry] -> [Entry]
addSynonym [] = []
addSynonym (decl:decls) = case decl of
    EDecl (TypeDecl _ (DHead _ name) typ) -> let typ' = TyFun () (TyCon () (UnQual () name)) typ
        in (EDecl (TypeSig () [Ident () ((nameStr name)++"To"++(consStr typ))] typ')):(addSynonym decls)
    _ -> decl:(addSynonym decls)

readDeclations :: PkgName -> Maybe Version -> IO [Entry]
readDeclations pkg version = do
    vpkg <- do 
        case version of
            Nothing -> return pkg
            Just v -> ifM (checkVersion pkg v) (return $ pkg ++ "-" ++ v) (return pkg)
    s   <- readFile $ downloadDir ++ vpkg ++ ".txt"
    let code = concat . rights . (map parseLine) $ splitOn "\n" s
    return $ renameSigs "" $ addSynonym code


printDeclaration :: Environment -> Entry -> IO ()
printDeclaration env decl = case decl of
    EDecl (TypeSig _ names ty) -> do
        let typ = evalState (toSynquidSchema ty) 0
        putStrLn $ (nameStr (names !! 0)) ++ " :: " ++ (show typ)
        let styp = toSuccinctType $ evalState (toSynquidRType env ty) 0
        putStrLn $ "===> " ++ (show styp)
    EDecl decl -> print decl
    EPackage pkg -> putStrLn pkg
    EModule mdl -> putStrLn mdl

printDeclarations :: PkgName -> Maybe Version -> Environment -> IO ()
printDeclarations pkg version env = do
    decls <- readDeclations pkg version
    mapM_ (printDeclaration env) decls

typeSignatureOf :: Environment -> Entry -> Maybe (Id, RSchema)
typeSignatureOf env decl = case decl of
    EDecl (TypeSig _ names ty) -> do
        let typ = toSynquidRSchema env $ evalState (toSynquidSchema ty) 0
        Just (nameStr (names !! 0), typ)
    _ -> Nothing

emptyDtDef = DatatypeDef [] [] [] [] Nothing

type EnvState = Map Id Int

freshId :: Monad m => Id -> StateT EnvState m Id
freshId prefix = do
    idMap <- get
    let id = Map.findWithDefault 0 prefix idMap
    put $ Map.insert prefix (id + 1) idMap
    return $ prefix ++ (show id)

-- | Replace all bound type variables with fresh free variables
instantiateSymbol :: Monad m => RSchema -> StateT EnvState m RType
instantiateSymbol sch = instantiate' Map.empty sch
  where
    instantiate' subst (ForallT a sch) = do
        a' <- freshId "A"
        instantiate' (Map.insert a (vart a' (BoolLit True)) subst) sch   
    instantiate' subst (Monotype t) = return $ typeSubstitute subst t

addSuccinctSymbol :: Monad m => Id -> RSchema -> Environment -> StateT EnvState m Environment
addSuccinctSymbol name t env = do
    newt <- instantiateSymbol t
    -- let newt = t
    let succinctTy = getSuccinctTy newt
    trace (show (name)) return ()
    trace (show (t)) return ()
    case newt of 
        (LetT id tDef tBody) -> do
          env' <- addSuccinctSymbol id (Monotype tDef) env
          addSuccinctSymbol name (Monotype tBody) env'
        _ -> return $ addEdgeForSymbol name succinctTy env
    where
        getSuccinctTy tt = case toSuccinctType tt of
          SuccinctAll vars ty -> SuccinctAll vars (refineSuccinctDatatype name ty env)
          ty -> refineSuccinctDatatype name ty env


addEdgeForSymbol :: Id -> SuccinctType -> Environment -> Environment
addEdgeForSymbol name succinctTy env = let
    envWithSelf = addEdge name succinctTy $ (succinctSymbols %~ HashMap.insert name succinctTy) env
    iteratedEnv = iteration env envWithSelf
    goalTy = lastSuccinctType (HashMap.lookupDefault SuccinctAny "__goal__" (iteratedEnv ^. succinctSymbols))
    diffTys = Set.filter isSuccinctConcrete $ ((allSuccinctNodes iteratedEnv)) `Set.difference` ((allSuccinctNodes env))
    -- finalEnv = iteratedEnv
    finalEnv = foldr (\sty accEnv -> addFromEdges sty $ addToEdges sty accEnv) iteratedEnv diffTys
    subgraphNodes = if goalTy == SuccinctAny then allSuccinctNodes finalEnv else reachableGraphFromGoal finalEnv
    reachableSet = (getReachableNodes iteratedEnv) `Set.intersection` subgraphNodes
    prunedEnv = finalEnv { _graphFromGoal = pruneGraphByReachability (finalEnv ^. succinctGraph) reachableSet }
    in prunedEnv
    where
    iteration oldEnv newEnv = let
      diffTys = Set.filter isSuccinctConcrete $ ((allSuccinctNodes newEnv)) `Set.difference` ((allSuccinctNodes oldEnv))
      in if Set.size diffTys == 0
        then newEnv
        else let
          env' = HashMap.foldrWithKey (\name ty accEnv -> addPolyEdge name ty accEnv (Set.filter isSuccinctConcrete (allSuccinctNodes newEnv))) newEnv (HashMap.filter isSuccinctAll (newEnv ^. succinctSymbols))
          -- goal = lastSuccinctType (HashMap.lookupDefault SuccinctAny "__goal__" (env' ^. succinctSymbols))
          -- subgraphNodes = if goal == SuccinctAny then allSuccinctNodes env' else reachableGraphFromGoal env'
          in iteration newEnv env'
    measureNames = case succinctTy of
      SuccinctDatatype (id,_) _ _ _ _ -> Map.keysSet $ allMeasuresOf id env
      _ -> Set.empty
    addToEdges typ oldEnv = let
      filter_fun k v = (isStrongerThan k typ) && (not (isSuccinctComposite k)) && k /= typ
      candidates = HashMap.keys $ HashMap.filterWithKey filter_fun (oldEnv ^. succinctGraph)
      in foldr (\t accEnv -> let 
        revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union t (Set.singleton typ)) accEnv
        in (succinctGraph %~ HashMap.insertWith mergeMapOfSet typ (HashMap.singleton t (Set.singleton (SuccinctEdge {_symbolId = "", _params = 0, _weight = HashMap.empty})))) revEnv
        ) oldEnv candidates
    addFromEdges typ oldEnv = let
      filter_fun k v = (isStrongerThan typ k) && (not (isSuccinctComposite k)) && k /= typ
      candidates = HashMap.keys $ HashMap.filterWithKey filter_fun (oldEnv ^. succinctGraph)
      in foldr (\t accEnv -> let 
        revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union typ (Set.singleton t)) accEnv
        in (succinctGraph %~ HashMap.insertWith mergeMapOfSet t (HashMap.singleton typ (Set.singleton (SuccinctEdge {_symbolId = "", _params = 0, _weight = HashMap.empty})))) revEnv
        ) oldEnv candidates

-- | add constructor and measure infos for a datatype
refineSuccinctDatatype :: Id -> SuccinctType -> Environment -> SuccinctType
refineSuccinctDatatype name sty env = case sty of
    SuccinctDatatype outerId ids tys cons measures -> let
        consMap = Set.foldr (\(id,_) accMap -> foldr (\c acc -> Map.insert c id acc) accMap (case (Map.lookup id (env ^. datatypes)) of
            Just dt -> if length (dt ^. constructors) > 1 then dt ^. constructors else []
            Nothing -> [])) Map.empty ids
        in if Map.member name consMap
            then SuccinctDatatype outerId ids tys (Map.singleton (fromJust (Map.lookup name consMap)) name) measures
            else SuccinctDatatype outerId ids tys cons measures
    SuccinctFunction paramCnt params ret -> SuccinctFunction paramCnt params (refineSuccinctDatatype name ret env)
    ty' -> ty'

addPolyEdge :: Id -> SuccinctType -> Environment -> Set SuccinctType -> Environment
addPolyEdge name (SuccinctAll idSet ty) env targets = 
  -- if all the type vars are bound in the env, treat it as none-all type
    if isAllBound 
    then addEdge name ty env 
    else case ty of 
      SuccinctFunction paramCnt pty rty -> let 
        fold_fun sty accEnv = let
          (unified, substitutions) = unifySuccinct rty sty (accEnv ^. boundTypeVars)
          pty' = Set.fromList $ map (\substitution -> Set.map (succinctTypeSubstitute substitution) pty) substitutions -- list of possible ptys
          in if unified 
            then Set.foldr (\ptySet acc -> let
              tyVars = foldr (\t set  -> set `Set.union` ((extractSuccinctTyVars t) `Set.difference` Set.fromList (accEnv ^. boundTypeVars))) Set.empty ptySet
              in if Set.size tyVars > 0
              then let 
                subst = Set.foldr (\tv macc -> Map.insert tv SuccinctAny macc) Map.empty tyVars
                ptySet' = Set.map (succinctTypeSubstitute subst) ptySet
                in addEdge name (SuccinctFunction paramCnt ptySet' sty) acc
              else addEdge name (SuccinctFunction paramCnt ptySet sty) acc
            ) accEnv pty'
            else accEnv
        in Set.foldr fold_fun env targets
      _                        -> let 
        fold_fun sty accEnv = let 
          (unified, substitutions) = unifySuccinct ty sty (accEnv ^. boundTypeVars)
          tys = Set.fromList $ map (\substitution -> succinctTypeSubstitute substitution ty) substitutions
          in if unified 
            then Set.foldr (\ty' acc -> let
              tyVars = (extractSuccinctTyVars ty') `Set.difference` Set.fromList (accEnv ^. boundTypeVars)
              in if Set.size tyVars > 0
                then let
                  subst = Set.foldr (\tv macc -> Map.insert tv SuccinctAny macc) Map.empty tyVars
                  substedTy = succinctTypeSubstitute subst ty'
                  revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union (SuccinctInhabited substedTy) (Set.singleton sty)) acc
                  in (succinctGraph %~ HashMap.insertWith mergeMapOfSet sty (HashMap.singleton (SuccinctInhabited substedTy) (Set.singleton (SuccinctEdge {_symbolId = name, _params = 0, _weight = HashMap.empty})))) revEnv
                else let
                  revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union (SuccinctInhabited ty') (Set.singleton sty)) acc
                  in (succinctGraph %~ HashMap.insertWith mergeMapOfSet sty (HashMap.singleton (SuccinctInhabited ty') (Set.singleton (SuccinctEdge {_symbolId = name, _params = 0, _weight = HashMap.empty})))) revEnv
            ) accEnv tys 
            else accEnv
        in Set.foldr fold_fun env targets
  where
    isAllBound = Set.foldr (\id acc -> (isBound env id) && acc) True idSet

addEdge :: Id -> SuccinctType -> Environment -> Environment
addEdge name (SuccinctFunction paramCnt argSet retTy) env = 
  let
    argTy = if Set.size argSet == 1 then Set.findMin argSet else SuccinctComposite argSet
    addedRevEnv = (succinctGraphRev %~ HashMap.insertWith Set.union argTy (Set.singleton retTy)) env
    addedRetEnv = (succinctGraph %~ HashMap.insertWith mergeMapOfSet retTy (HashMap.singleton argTy (Set.singleton (SuccinctEdge {_symbolId = name, _params = paramCnt, _weight = HashMap.empty})))) addedRevEnv
  in if Set.size argSet == 1
    then addedRetEnv
    else Set.foldr (\elem acc -> let revEnv = (succinctGraphRev %~ HashMap.insertWith Set.union elem (Set.singleton argTy)) acc
      in (succinctGraph %~ HashMap.insertWith mergeMapOfSet argTy (HashMap.singleton elem (Set.singleton (SuccinctEdge {_symbolId = "", _params = 0, _weight = HashMap.empty})))) revEnv) addedRetEnv argSet
addEdge name typ@(SuccinctAll idSet ty) env = 
  let 
    polyEnv = addPolyEdge name typ env $ Set.filter isSuccinctConcrete (allSuccinctNodes env)
  in case ty of
    SuccinctFunction paramCnt pty rty -> if Set.null ((extractSuccinctTyVars rty) `Set.difference` Set.fromList (env ^. boundTypeVars))
      then let
        tyVars = foldr (\t set -> set `Set.union` ((extractSuccinctTyVars t) `Set.difference` Set.fromList (env ^. boundTypeVars))) Set.empty pty
        subst = Set.foldr (\tv macc -> Map.insert tv SuccinctAny macc) Map.empty tyVars
        substedTys = Set.map (succinctTypeSubstitute subst) pty
        in addEdge name (SuccinctFunction paramCnt substedTys rty) env
      else polyEnv
    _ -> polyEnv
addEdge name typ env = 
  let
    inhabitedEnvRev = (succinctGraphRev %~ HashMap.insertWith Set.union (SuccinctInhabited typ) (Set.singleton typ)) env
    inhabitedEnv = (succinctGraph %~ HashMap.insertWith mergeMapOfSet typ (HashMap.singleton (SuccinctInhabited typ) (Set.singleton (SuccinctEdge {_symbolId = name, _params = 0, _weight = HashMap.empty})))) inhabitedEnvRev
    in inhabitedEnv

getReachableNodes :: Environment -> Set SuccinctType
getReachableNodes env = 
  getReachableNodesHelper (env ^. succinctGraphRev) Set.empty [] $ Set.toList $ Set.filter (\typ -> isSuccinctInhabited typ || isSuccinctFunction typ || typ == (SuccinctScalar BoolT) || hasSuccinctAny typ) (allSuccinctNodes env)
  where
    isCompositeReachable reachableSet typ = case typ of
      SuccinctComposite tySet -> Set.foldr (\b acc -> acc && (Set.member b reachableSet)) True tySet
      _ -> True
    getReachableNodesWithoutComposite g visited toVisit = case toVisit of
      [] -> visited
      curr:xs -> if Set.member curr visited
        then getReachableNodesWithoutComposite g visited xs
        else let newVisited = Set.insert curr visited 
          in getReachableNodesWithoutComposite g newVisited (xs ++ (Set.toList (Set.filter (isCompositeReachable newVisited) (HashMap.lookupDefault Set.empty curr g))))
    getReachableNodesHelper g visited waitingList toVisit = case toVisit of
      [] -> visited `Set.union` (getReachableNodesWithoutComposite g visited (filter (isCompositeReachable visited) waitingList))
      curr:xs -> if Set.member curr visited 
        then getReachableNodesHelper g visited waitingList xs
        else case curr of
          SuccinctComposite _ -> getReachableNodesHelper g visited (waitingList++[curr]) xs
          _ -> getReachableNodesHelper g (Set.insert curr visited) waitingList (xs ++ (Set.toList (HashMap.lookupDefault Set.empty curr g)))

reachableGraphFromGoal :: Environment -> Set SuccinctType
reachableGraphFromGoal env = reachableGraphFromGoalHelper (env ^. succinctGraph) Set.empty startTys
  where
    goalTy = outOfSuccinctAll $ lastSuccinctType (HashMap.lookupDefault SuccinctAny "__goal__" (env ^. succinctSymbols))
    startTys = (SuccinctScalar BoolT):(Set.toList $ Set.filter (\t -> succinctAnyEq goalTy t) (allSuccinctNodes env))
    isCompositeReachable reachableSet typ = case typ of
      SuccinctComposite tySet -> Set.foldr (\b acc -> acc && (Set.member b reachableSet)) True tySet
      _ -> True
    reachableGraphFromGoalHelper g visited toVisit = case toVisit of
      [] -> visited
      curr:xs -> if Set.member curr visited
        then reachableGraphFromGoalHelper g visited xs
        else reachableGraphFromGoalHelper g (Set.insert curr visited) (xs ++ (HashMap.keys (HashMap.lookupDefault HashMap.empty curr g)))

rmUnreachableComposite :: Environment -> Set SuccinctType -> Set SuccinctType
rmUnreachableComposite env reachableSet = Set.foldr (\t acc -> if isCompositeReachable t then acc else Set.delete t acc) reachableSet (compositeNodes)
  where
    isCompositeNode ty = case ty of
      SuccinctComposite _ -> True
      _ -> False
    compositeNodes = Set.filter isCompositeNode reachableSet
    isCompositeReachable t = let SuccinctComposite tySet = t in 
      Set.foldr (\b acc -> acc && (Set.member b reachableSet)) True tySet

pruneGraphByReachability g reachableSet = HashMap.foldrWithKey (\k v acc -> if Set.member k reachableSet then HashMap.insert k (HashMap.filterWithKey (\k' s -> Set.member k' reachableSet) v) acc else acc) HashMap.empty g

allSuccinctNodes :: Environment -> Set SuccinctType
allSuccinctNodes env = Set.fromList $ (HashMap.keys (env ^. succinctGraph)) ++ (HashMap.foldr (\m acc -> acc ++ (HashMap.keys m)) [] (env ^. succinctGraph))

edges env = HashMap.foldrWithKey (\k v acc -> (map (\(k',v') -> (k,v',k')) (HashMap.toList v)) ++ acc) [] (env ^. graphFromGoal)

nodes env = allSuccinctNodes env

showGraphViz env =
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
  (concatMap showNode $ nodes env) ++
  (concatMap showEdge $ edges env) ++
  "}\n"
  where showEdge (from, t, to) = "\"" ++ (show from) ++ "\"" ++ " -> " ++ "\"" ++(show to) ++"\"" ++
                                 " [label = \"" ++ (Set.foldr (\(SuccinctEdge s params _) str -> str++","++s) "" t) ++ "\"];\n"
        showNode v = "\"" ++(show v) ++ "\"" ++"\n"

packageEnv :: MonadIO m => PkgName -> StateT EnvState m Environment
packageEnv pkg = do
    decls <- liftIO $ readDeclations pkg Nothing
    let dts = Set.unions $ map getDeclTy decls
    let env = foldr (uncurry addDatatype) emptyEnv (map withEmptyDt $ Set.toList dts)
    let sigs = map fromJust . filter isJust . map (typeSignatureOf env) $ decls
    let env' = foldr (uncurry addPolyVariable) env sigs
    foldM (\accEnv (id, typ) -> addSuccinctSymbol id typ accEnv) env' sigs
  where
    withEmptyDt id = (id, emptyDtDef)
    getDeclTy decl = case decl of
        EDecl (TypeSig _ names ty) -> datatypeOf ty
        _ -> Set.empty

packageTypes :: PkgName -> Set RType
packageTypes = undefined