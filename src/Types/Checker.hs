module Types.Checker where

data CheckerState r = CheckerState {
    typeAssignments :: Map Id (TypeSkeleton r),
    nameMapping :: Map Id Id,
    isChecked :: Bool,
    nameCounter :: Map Id Int
}

type TypeChecker r = StateT (CheckerState r) m