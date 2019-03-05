module Abstract.Cover where

import Abstract.Types
import HooglePlus.Types
import HooglePlus.Utils
import Synquid.Type
import Synquid.Program
import Synquid.Util
import Synquid.Pretty


import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Data.List
import Data.List.Extra
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Char as Char
import Control.Lens
import Control.Monad.State
import Text.Printf


firstLvAbs :: Environment -> [RSchema] -> AbstractionTree
firstLvAbs env schs =
    Set.foldl' (updateSemantic env) (ALeaf (AExclusion Set.empty)) dts
  where
    typs = map (shape . toMonotype) schs
    dts = Set.unions (map (allAbstractDts (env ^. boundTypeVars)) typs)

-- Produce the most specific abstraction possible from the given types.
specificAbstractionFromTypes :: Environment -> [RSchema] -> AbstractionTree
specificAbstractionFromTypes env schemas = let
    abstrSkels = map (toAbstractType . shape . toMonotype) schemas
    baseTree = ALeaf (AExclusion Set.empty)
    in
        foldr (flip $ updateSemantic env) baseTree abstrSkels

updateSemantic :: Environment -> AbstractionTree -> AbstractSkeleton -> AbstractionTree
updateSemantic env semantic typ@(ATypeVarT id) | isBound env id =
    case semantic of
    ALeaf t
        | t == typ -> semantic
        | otherwise -> ANode t (ALeaf typ) (ALeaf (typeDifference t typ))
    ANode t lt rt
        | t == typ -> semantic
        | isSubtypeOf typ t && isSubtypeOf typ (valueType lt) -> ANode t (updateSemantic env lt typ) rt
        | isSubtypeOf typ t -> ANode t lt (updateSemantic env rt typ)
        | otherwise -> error (printf "%s is not a subtype of %s, not a subtype of %s, not a subtype of %s, we should not reach here" (show typ) (show t) (show (valueType lt)) (show (valueType rt)))
updateSemantic _ semantic (ATypeVarT id) | otherwise = semantic
updateSemantic _ semantic@(ALeaf t) typ@(ADatatypeT {}) | typ == t = semantic
updateSemantic env semantic@(ALeaf t@(ADatatypeT id tArgs)) typ@(ADatatypeT id' tArgs') =
    case (tArgs, tArgs') of
    ([], []) -> semantic
    _ -> firstDiff semantic [] tArgs tArgs'
    where
    wrapTree preArgs postArgs (ALeaf t) = ALeaf (ADatatypeT id (preArgs ++ [t] ++ postArgs))
    wrapTree preArgs postArgs (ANode t lt rt) =
        ANode (ADatatypeT id (preArgs ++ [t] ++ postArgs))
                (wrapTree preArgs postArgs lt)
                (wrapTree preArgs postArgs rt)

    rootType (ALeaf t) = t
    rootType (ANode t _ _) = t

    replaceTree (ALeaf t) tree | rootType tree == t = tree
    replaceTree (ANode t lt rt) tree | rootType tree == t = error "should not replace a non-leaf type"
    replaceTree (ANode t lt rt) tree | isSubtypeOf (rootType tree) (valueType lt) = ANode t (replaceTree lt tree) rt
    replaceTree (ANode t lt rt) tree | isSubtypeOf (rootType tree) (valueType rt) = ANode t lt (replaceTree rt tree)
    replaceTree t t' = error $ "unhandled case with " ++ show t ++ " and " ++ show t'

    firstDiff s _ [] [] = s
    firstDiff s pre (arg:args) (arg':args')
        | arg == arg' = firstDiff s (pre ++ [arg]) args args'
        | otherwise   = let tmp = updateSemantic env (ALeaf arg) arg'
                            s' = replaceTree s (wrapTree pre args tmp)
                        in firstDiff s' (pre ++ [arg']) args args'
updateSemantic env semantic@(ALeaf t) typ@(ADatatypeT id tArgs) | t /= typ =
    updateSemantic env semantic' typ
    where
    emptyArgs = map fillAny tArgs
    typ' = ADatatypeT id emptyArgs
    semantic' = ANode t (ALeaf typ') (ALeaf (typeDifference t typ'))
updateSemantic env semantic@(ANode t lt rt) typ
    | t == typ = semantic
    | isSubtypeOf typ t && isSubtypeOf typ (valueType lt) = ANode t (updateSemantic env lt typ) rt
    | isSubtypeOf typ t = ANode t lt (updateSemantic env rt typ)
    | otherwise = error (printf "%s is not a subtype of %s, not a subtype of %s, not a subtype of %s, we should not reach here" (show typ) (show t) (show (valueType lt)) (show (valueType rt)))
updateSemantic env semantic (AFunctionT tArg tRet) = semantic''
    where
    semantic' = updateSemantic env semantic tArg
    semantic'' = updateSemantic env semantic' tRet
updateSemantic env semantic@(ALeaf (AExclusion s)) (AExclusion s') =
    foldl' (updateSemantic env) semantic (absVars ++ absDts)
    where
    buildDt dt = case Map.lookup dt (env ^. datatypes) of
                    Nothing -> error $ "cannot find datatype " ++ dt
                    Just dtDef -> ADatatypeT dt (map fillAny (dtDef ^. typeParams))
    (vars, dts) = partition (Char.isLower . head) (Set.toList (Set.difference s' s))  
    absVars = map ATypeVarT vars
    absDts = map buildDt dts
updateSemantic env semantic (AExclusion s) =
    -- add all the complementary datatypes or type variables into the semantic
    foldl' (updateSemantic env) semantic (absVars ++ absDts)
    where
    buildDt dt = case Map.lookup dt (env ^. datatypes) of
                    Nothing -> error $ "cannot find datatype " ++ dt
                    Just dtDef -> ADatatypeT dt (map fillAny (dtDef ^. typeParams))
    (vars, dts) = partition (Char.isLower . head) (Set.toList s)  
    absVars = map ATypeVarT vars
    absDts = map buildDt dts
        

-- distinguish one type from a given general one
type SplitMsg = (AbstractSkeleton, AbstractSkeleton)

distinguish :: MonadIO m => Environment -> SType -> SType -> PNSolver m (Maybe SplitMsg)
distinguish env (FunctionT _ tArg tRes) (FunctionT _ tArg' tRes') = do
    diff <- distinguish env tArg tArg'
    case diff of
      Nothing  -> distinguish env tRes tRes'
      res -> return res
distinguish env AnyT _ = return Nothing
distinguish env _ AnyT = return Nothing
distinguish env t1 t2 = do
    tass <- view typeAssignment <$> get
    semantic <- view abstractionTree <$> get
    writeLog 2 $ text "type assignments" <+> text (show tass)
    let t1' = var2any env (stypeSubstitute tass t1)
    let t2' = var2any env (stypeSubstitute tass t2)
    let ats1 = cutoff env semantic (toAbstractType t1')
    let ats2 = cutoff env semantic (toAbstractType t2')
    writeLog 3 $ text "trying to distinguish" <+> pretty t1' <+> text "==>" <+> pretty ats1 <+> text "and" <+> pretty t2' <+> text "==>" <+> pretty ats2
    -- only try to get split information when the two types have
    -- same abstract representations in the current abstraction level
    let diff = ats1 `intersect` ats2
    let pTyp = head diff
    let aTyp1 = toAbstractType t1'
    let aTyp2 = toAbstractType t2'
    if null diff || t1' == t2'
       then return Nothing
       else case distinguish' (env ^. boundTypeVars) pTyp aTyp1 aTyp2 of
              Nothing -> return Nothing
              Just t -> return (Just (pTyp, t))

distinguishFrom :: AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton
distinguishFrom (AExclusion s) t@(ATypeVarT id) | id `Set.notMember` s = t
distinguishFrom (AExclusion s) t@(ADatatypeT id _) | id `Set.notMember` s = t
distinguishFrom (ADatatypeT id tArgs) (ADatatypeT id' tArgs') | id == id' =
    ADatatypeT id (firstDiff tArgs tArgs')
  where
    firstDiff [] [] = []
    firstDiff (arg:args) (arg':args')
        | arg == arg' = arg:(firstDiff args args')
        | otherwise = (distinguishFrom arg arg'):args
distinguishFrom t1 t2 = error ("Cannot distinguish " ++ show t2 ++ " from " ++ show t1)

distinguish' :: [Id] -> AbstractSkeleton -> AbstractSkeleton -> AbstractSkeleton -> Maybe AbstractSkeleton
distinguish' _ _ t1 t2 | t1 == t2 = Nothing
distinguish' _ _ t1@(ADatatypeT id1 tArgs1) t2@(ADatatypeT id2 tArgs2) | id1 /= id2 =
    Just (ADatatypeT id1 (map fillAny tArgs1))
distinguish' tvs (AExclusion {}) t1@(ADatatypeT id1 tArgs1) t2@(ADatatypeT id2 tArgs2) | id1 == id2 =
    distinguish' tvs (ADatatypeT id1 (map fillAny tArgs1)) t1 t2
distinguish' tvs (ADatatypeT pid pArgs) t1@(ADatatypeT id1 tArgs1) t2@(ADatatypeT id2 tArgs2) | id1 == id2 =
    case firstDifference pArgs tArgs1 tArgs2 of
      [] -> Nothing
      diffs -> Just (ADatatypeT id1 diffs)
  where
    firstDifference _ [] [] = []
    firstDifference (parg:pargs) (arg:args) (arg':args') =
        case distinguish' tvs parg arg arg' of
            Nothing -> case firstDifference pargs args args' of
                         [] -> []
                         diffs -> parg:diffs
            Just t  -> if t /= parg then t:pargs 
                                    else case firstDifference pargs args args' of
                                           [] -> []
                                           diffs -> parg : diffs
distinguish' _ _ (AExclusion s) (ADatatypeT id args) | id `Set.notMember` s = Just (ADatatypeT id (map fillAny args))
distinguish' _ _ (AExclusion s) (ADatatypeT id _) = Nothing
distinguish' tvs p t1@(ADatatypeT id args) t2@(AExclusion s) = distinguish' tvs p t2 t1
distinguish' _ _ (ATypeVarT id1) (ADatatypeT id2 args) = Just (ATypeVarT id1) -- Just (ADatatypeT id2 (map fillAny args))
distinguish' tvs p t1@(ADatatypeT {}) t2@(ATypeVarT {}) = distinguish' tvs p t2 t1
distinguish' tvs _ (AExclusion s) (ATypeVarT id) | id `elem` tvs && id `Set.notMember` s = Just (ATypeVarT id)
distinguish' _ _ (AExclusion {}) (ATypeVarT id) = Nothing
distinguish' tvs p t1@(ATypeVarT {}) t2@(AExclusion {}) = distinguish' tvs p t2 t1
distinguish' tvs _ (ATypeVarT id) (ATypeVarT _) | id `elem` tvs = Just (ATypeVarT id)
distinguish' tvs _ (ATypeVarT _) (ATypeVarT id) | id `elem` tvs = Just (ATypeVarT id)
distinguish' tvs _ (ATypeVarT _) (ATypeVarT _) = Nothing
distinguish' _ _ t1 t2 = error (printf "unhandled case for distinguish %s and %s" (show t1) (show t2))
