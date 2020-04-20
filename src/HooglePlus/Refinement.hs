{-# LANGUAGE FlexibleContexts #-}
module HooglePlus.Refinement where

import Database.Convert
import Database.Util
import PetriNet.AbstractType
import PetriNet.Util
import Synquid.Pretty
import Synquid.Program
import Synquid.Type
import Synquid.Util
import Types.Abstract
import Types.Common
import Types.Environment
import Types.Program
import Types.Solver
import Types.Type
import Types.CheckMonad

import Control.Lens
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Text.Printf

-- | add a new type into our cover and ensure all of them have proper lower bound
updateCover :: [Id] -> AbstractSkeleton -> AbstractCover -> AbstractCover
updateCover tvs t cover = let (_, cover') = updateCover' tvs cover [] t rootNode in cover'

updateCover' :: [Id] -> AbstractCover -> [AbstractSkeleton] -> AbstractSkeleton -> AbstractSkeleton -> ([AbstractSkeleton], AbstractCover)
updateCover' bound cover intscts t paren | equalAbstract bound t paren = (intscts, cover)
updateCover' bound cover intscts t paren | isSubtypeOf bound t paren =
    let children = HashMap.lookupDefault Set.empty paren cover
        child_fun c (ints, acc) = updateCover' bound acc ints t c
        (scts, updatedCover) = Set.foldr child_fun (intscts, cover) children
        lower c = isSubtypeOf bound t c || isSubtypeOf bound c t
        inSubtree = any lower (Set.toList children)
        baseCover = if inSubtree
                      then updatedCover
                      else HashMap.insertWith Set.union paren (Set.singleton t) updatedCover
        int_fun s (ints, acc) = updateCover' bound acc ints s rootNode
     in foldr int_fun ([], baseCover) scts
updateCover' bound cover intscts t paren | isSubtypeOf bound paren t =
    let parents = HashMap.keys $ HashMap.filter (Set.member paren) cover
        rmParen = HashMap.map (Set.delete paren) cover
        addCurr p = HashMap.insertWith Set.union p $ Set.singleton t
        addedCurr = foldr addCurr rmParen parents
        cover' = HashMap.insertWith Set.union t (Set.singleton paren) addedCurr
     in (intscts, cover')
updateCover' bound cover intscts t paren =
    let intsctMb = abstractIntersect bound t paren
     in if isJust intsctMb then (fromJust intsctMb : intscts, cover)
                           else (intscts, cover)

propagate :: MonadIO m => Environment -> RProgram -> AbstractSkeleton -> PNSolver m ()
-- | base case, when we reach the leaf of the AST
propagate env p@(Program (PSymbol sym) t) upstream = do
    writeLog 2 "propagate" $ text "propagate" <+> pretty upstream <+> text "into" <+> pretty p
    cover <- gets $ view (refineState . abstractionCover)
    let bound = env ^. boundTypeVars
    unless (existAbstract bound cover upstream)
           (do
                let newCover = updateCover bound upstream cover
                modify $ set (refineState . abstractionCover) newCover
                let newTyps = allTypesOf newCover \\ allTypesOf cover
                modify $ over (refineState . splitTypes) (Set.union $ Set.fromList newTyps)
           )
-- | starter case, when we start from a bottom type
-- find the most general abstraction that unifies with the concrete types
-- of the arguments, but not unify with the function args of its signature
propagate env p@(Program (PApp f args) _) upstream = do
    unless (isBot upstream) (propagate env (Program (PSymbol "x") AnyT) upstream)
    writeLog 2 "propagate" $ text "propagate" <+> pretty upstream <+> text "into" <+> pretty p
    t <- findSymbol env (removeLast '_' f)
    let closedArgs = map (shape . typeOf) args
    let argConcs = map toAbstractType closedArgs
    let absFun = toAbstractType $ shape t
    abstractArgs <- observeT $ mostGeneral argConcs absFun
    mapM_ (uncurry $ propagate env) (zip args abstractArgs)
  where
    mostGeneral cArgs t = do
        let bound = env ^. boundTypeVars
        absArgs <- mapM (generalize bound) cArgs
        lift $ writeLog 3 "propagate" $ text "get generalized types" <+> pretty absArgs <+> text "from" <+> pretty cArgs
        res <- lift $ applySemantic bound t absArgs
        lift $ writeLog 3 "propagate" $ text "apply" <+> pretty absArgs <+> text "to" <+> pretty t <+> text "gets" <+> pretty res
        guard (isSubtypeOf bound res upstream)
        return $ map compactAbstractType absArgs
-- | case for lambda functions
propagate env (Program (PFun x body) (FunctionT _ tArg tRet))
              (AFunctionT atArg atRet) =
    propagate (addVariable x (addTrue tArg) env) body atRet
propagate env (Program (PFun x body) t) (AFunctionT atArg atRet) = do
    id <- freshId "A"
    let tArg = addTrue (ScalarT (TypeVarT Map.empty id) ())
    propagate (addVariable x (addTrue tArg) env) body atRet
propagate _ prog t = return ()

-- | generalize a closed concrete type into an abstract one
generalize :: (CheckMonad (t m), MonadIO (t m), MonadIO m) 
           => [Id] 
           -> AbstractSkeleton 
           -> LogicT (t m) AbstractSkeleton
generalize bound t@(AScalar (ATypeVarT id))
  | id `notElem` bound = return t
  | otherwise = do
    v <- lift $ freshId "T"
    return (AScalar (ATypeVarT v)) `mplus` return t
-- for datatype, we define the generalization order as follows:
-- (1) v
-- (2) datatype with all fresh type variables
-- (3) datatype with incrementally generalized inner types
generalize bound t@(AScalar (ADatatypeT id args)) = do
    v <- lift $ freshId "T"
    return (AScalar (ATypeVarT v)) `mplus` freshVars `mplus` subsetTyps -- interleave
  where
    -- this search may explode when we have a large number of datatype parameters
    patternOfLen n
      | n == 0 = mzero
      | n == 1 = return [n]
      | n >  1 = do
          let nextNumber l = 1 + maximum l
          let candidates l = nextNumber l : nub l
          prevPat <- patternOfLen (n - 1)
          msum $ map (\c -> return (c:prevPat)) (candidates prevPat)

    freshVars = do
        let n = length args
        pat <- patternOfLen n
        let argNames = map (\i -> "T" ++ show i) pat
        let args' = map (AScalar . ATypeVarT) argNames
        absTy <- lift $ freshAbstract bound (AScalar (ADatatypeT id args'))
        guard (isSubtypeOf bound t absTy)
        lift $ writeLog 3 "generalize" $ text "generalize" <+> pretty t <+> text "into" <+> pretty absTy
        return absTy

    subsets [] = return []
    subsets (arg:args) = do
        args' <- subsets args
        arg' <- generalize bound arg
        return (arg':args')

    subsetTyps = do
        args' <- subsets args
        return (AScalar (ADatatypeT id args'))

generalize bound (AFunctionT tArg tRes) = do
    tArg' <- generalize bound tArg
    tRes' <- generalize bound tRes
    return (AFunctionT tArg' tRes')
