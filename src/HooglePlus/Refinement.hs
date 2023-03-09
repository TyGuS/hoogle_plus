module HooglePlus.Refinement where

import Control.Lens (over, set, view)
import Control.Monad.Logic
import Control.Monad.State (StateT, gets, modify)
import qualified Data.HashMap.Strict as HashMap
import Data.List ((\\), nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set

import Types.Common
import Types.Environment
import Types.Fresh
import Types.Log
import Types.Pretty
import Types.Program
import Types.Solver
import Types.Substitution
import Types.Type
import Types.TypeChecker
import Utility.Utils

-- | add a new type into our cover and ensure all of them have proper lower bound
updateCover :: [Id] -> TypeSkeleton -> AbstractCover -> AbstractCover
updateCover tvs t cover =
  let (_, cover') = updateCover' tvs cover [] t rootNode in cover'

updateCover' :: [Id] -> AbstractCover -> [TypeSkeleton] -> TypeSkeleton -> TypeSkeleton -> ([TypeSkeleton], AbstractCover)
updateCover' bound cover intscts t paren | equalAbstract bound t paren =
  (intscts, cover)
updateCover' bound cover intscts t paren | isSubtypeOf bound t paren =
  let children = Map.findWithDefault Set.empty paren cover
      child_fun c (ints, acc) = updateCover' bound acc ints t c
      (scts, updatedCover) = Set.foldr child_fun (intscts, cover) children
      lower c = isSubtypeOf bound t c || isSubtypeOf bound c t
      inSubtree = any lower (Set.toList children)
      baseCover = if inSubtree
        then updatedCover
        else Map.insertWith Set.union paren (Set.singleton t) updatedCover
      int_fun s (ints, acc) = updateCover' bound acc ints s rootNode
  in  foldr int_fun ([], baseCover) scts
updateCover' bound cover intscts t paren | isSubtypeOf bound paren t =
  let
    parents = Map.keys $ Map.filter (Set.member paren) cover
    rmParen s =
      let s' = Set.delete paren s in if Set.null s' then Nothing else Just s'
    replaceWith p acc =
      if isSubtypeOf bound t p && not (equalAbstract bound t p)
        then Map.insertWith Set.union
                            p
                            (Set.singleton t)
                            (Map.update rmParen p acc)
        else acc
    cover' = Map.insertWith Set.union
                            t
                            (Set.singleton paren)
                            (foldr replaceWith cover parents)
  in
    (intscts, cover')
updateCover' bound cover intscts t paren =
  let intsctMb = abstractIntersect bound t paren
  in  if isJust intsctMb
        then (fromJust intsctMb : intscts, cover)
        else (intscts, cover)

propagate :: SolverMonad m => NameMapping -> Environment -> TProgram -> TypeSkeleton -> PNSolver m ()
-- | base case, when we reach the leaf of the AST
propagate nameMapping env p@(Program (PSymbol sym) t) upstream = do
  writeLog 2 "propagate"
    $   text "propagate"
    <+> pretty upstream
    <+> text "into"
    <+> pretty p
  cover <- gets $ view (refineState . abstractionCover)
  let bvs = getBoundTypeVars env
  unless
    (existAbstract bvs cover upstream)
    (do
      writeLog 2 "propagate" $ text "adding abstract type" <+> pretty upstream <+> "into the cover"
      let newCover = updateCover bvs upstream cover
      modify $ set (refineState . abstractionCover) newCover
      let newTyps = typesInCover newCover \\ typesInCover cover
      writeLog 3 "propagate" $ text "computing splitted types"
      modify
        $ over (refineState . splitTypes) (Set.union $ Set.fromList newTyps)
    )
-- | starter case, when we start from a bottom type
-- find the most general abstraction that unifies with the concrete types
-- of the arguments, but not unify with the function args of its signature
propagate nameMapping env p@(Program (PApp f args) _) upstream = do
  unless (isBot upstream) (propagate nameMapping env (Program (PSymbol "__x") TopT) upstream)
  writeLog 2 "propagate"
    $   text "propagate"
    <+> pretty upstream
    <+> text "into"
    <+> pretty p
  -- we need to do the name mapping here
  -- let f' = lookupWithError "nameMapping" (removeLast '_' f) nameMapping
  t <- findSymbol nameMapping env f
  let closedArgs = map typeOf args
  let argConcs   = map toAbstractType closedArgs
  let absFun     = toAbstractType (toMonotype t)
  writeLog 2 "propagate" $ "generalizing" <+> pretty argConcs <+> text "to match" <+> pretty absFun
  abstractArgs <- observeT $ mostGeneral argConcs absFun
  mapM_ (uncurry $ propagate nameMapping env) (zip args abstractArgs)
 where
  mostGeneral :: SolverMonad m => [TypeSkeleton] -> TypeSkeleton -> LogicT (PNSolver m) [TypeSkeleton]
  mostGeneral cArgs t = do
    let bvs = getBoundTypeVars env
    absArgs <- mapM (generalize bvs) cArgs
    lift
      $   writeLog 3 "propagate"
      $   text "get generalized types"
      <+> pretty absArgs
      <+> text "from"
      <+> pretty cArgs
    cover <- gets $ view (refineState . abstractionCover)
    res   <- lift $ abstractApply bvs cover t absArgs
    lift
      $   writeLog 3 "propagate"
      $   text "apply"
      <+> pretty absArgs
      <+> text "to"
      <+> pretty t
      <+> text "gets"
      <+> pretty res
    guard (isSubtypeOf bvs res upstream)
    return $ map toAbstractFun absArgs

-- | case for lambda functions
propagate nameMapping env (Program (PFun x body) (FunctionT _ tArg tRet)) (FunctionT _ atArg atRet)
  = propagate nameMapping (addComponent x (Monotype tArg) env) body atRet
propagate nameMapping env (Program (PFun x body) t) (FunctionT _ atArg atRet) = do
  id <- freshId (getBoundTypeVars env) "A"
  let tArg = TypeVarT id
  propagate nameMapping (addComponent x (Monotype tArg) env) body atRet
propagate _ _ prog t = return ()

-- | generalize a closed concrete type into an abstract one
generalize :: (Loggable (LogicT (StateT s m)), Fresh s m) => [Id] -> TypeSkeleton -> LogicT (StateT s m) TypeSkeleton
generalize bound t@(TypeVarT id)
  | id `notElem` bound = return t
  | otherwise = do
    v <- lift $ freshId bound "T"
    return (TypeVarT v) `mplus` return t
-- for datatype, we define the generalization order as follows:
-- (1) v
-- (2) datatype with all fresh type variables
-- (3) datatype with incrementally generalized inner types
generalize bound t@(DatatypeT id args) = do
  v <- lift $ freshId bound "T"
  return (TypeVarT v) `mplus` freshVars `mplus` subsetTyps -- interleave
 where
    -- this search may explode when we have a large number of datatype parameters
  patternOfLen n
    | n == 0 = mzero
    | n == 1 = return [n]
    | n > 1 = do
      let nextNumber l = 1 + maximum l
      let candidates l = nextNumber l : nub l
      prevPat <- patternOfLen (n - 1)
      msum $ map (\c -> return (c : prevPat)) (candidates prevPat)
    | otherwise = error "patternOfLen: invalid length"

  freshVars = do
    let n = length args
    pat <- patternOfLen n
    let argNames = map (appendIndex "T") pat
    let args'    = map TypeVarT argNames
    absTy <- lift $ freshType bound (DatatypeT id args')
    guard (isSubtypeOf bound t absTy)
    writeLog 3 "generalize"
      $   text "generalize"
      <+> pretty t
      <+> text "into"
      <+> pretty absTy
    return absTy

  subsets []           = return []
  subsets (arg : args) = do
    args' <- subsets args
    arg'  <- generalize bound arg
    return (arg' : args')

  subsetTyps = do
    args' <- subsets args
    return (DatatypeT id args')

generalize bound (FunctionT x tArg tRes) = do
  tArg' <- generalize bound tArg
  tRes' <- generalize bound tRes
  return (FunctionT x tArg' tRes')
generalize _ _ = error "generalize: cannot generalize TopT or BotT"
