module HooglePlus.Refinement where

import Types.Abstract
import Types.Common
import HooglePlus.Abstraction

import qualified Data.Set as Set
import Text.Printf

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
