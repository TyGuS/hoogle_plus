module Database.Util where

import Types.Program
import Types.Type
import Synquid.Logic
import Synquid.Error (Pos(Pos))

import Text.Parsec.Pos (initialPos)
import qualified Data.Set as Set
import qualified Data.Map as Map


(>.<) :: Ord a => [a] -> [a] -> [a]
xs >.< ys = let ys' = Set.fromList ys in filter (flip Set.member ys') xs

(>.>) :: Ord a => [a] -> [a] -> [a]
xs >.> ys = let ys' = Set.fromList ys in filter (flip Set.notMember ys') xs


-- Default Library
defaultFuncs = [ Pos (initialPos "fst") $ FuncDecl "fst" (Monotype (FunctionT "p" (ScalarT (DatatypeT "Pair" [ScalarT (TypeVarT Map.empty "a") ftrue, ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue) (ScalarT (TypeVarT Map.empty "a") ftrue)))
                , Pos (initialPos "snd") $ FuncDecl "snd" (Monotype (FunctionT "p" (ScalarT (DatatypeT "Pair" [ScalarT (TypeVarT Map.empty "a") ftrue, ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue) (ScalarT (TypeVarT Map.empty "b") ftrue)))
                , Pos (initialPos "tcBuiltIn") $ FuncDecl "test" (Monotype (FunctionT "p" (ScalarT (DatatypeT "Int" [] []) ftrue) (ScalarT (DatatypeT "__hplusTC__Show" [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue)))
                ]
-- TODO: Should be Int -> ShowD Int!

defaultDts = [defaultList, defaultPair, defaultUnit, defaultInt, defaultBool]

defaultInt = Pos (initialPos "Int") $ DataDecl "Int" [] [] []

defaultBool = Pos (initialPos "Bool") $ DataDecl "Bool" [] [] []

defaultList = Pos (initialPos "List") $ DataDecl "List" ["a"] [] [
    ConstructorSig "Nil"  $
      ScalarT (DatatypeT "List" [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue
  , ConstructorSig "Cons" $
      FunctionT "x" (ScalarT (TypeVarT Map.empty "a") ftrue)
      (FunctionT "xs"
        (ScalarT (DatatypeT "List" [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue)
      (ScalarT (DatatypeT "List" [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue))
  ]

defaultPair = Pos (initialPos "Pair") $ DataDecl "Pair" ["a", "b"] [] [
    ConstructorSig "Pair" $ FunctionT "x" (ScalarT (TypeVarT Map.empty "a") ftrue) (FunctionT "y" (ScalarT (TypeVarT Map.empty "b") ftrue) (ScalarT (DatatypeT "Pair" [ScalarT (TypeVarT Map.empty "a") ftrue, ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue))
  ]

defaultUnit = Pos (initialPos "Unit") $ DataDecl "Unit" [] [] []

