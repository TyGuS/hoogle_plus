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
                ]

defaultDts = [defaultList, defaultPair, defaultUnit, defaultInt, defaultBool, defaultChar, defaultString]

defaultTypeclasses = [defaultShowClass, defaultShowFunc, intShow]

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

defaultChar = Pos (initialPos "Char") $ DataDecl "Char" [] [] []

defaultString = Pos (initialPos "String") $ TypeDecl "String" []
    (ScalarT (DatatypeT "List" [ScalarT (DatatypeT "Char" [] []) ftrue] []) ftrue)

defaultPair = Pos (initialPos "Pair") $ DataDecl "Pair" ["a", "b"] [] [
    ConstructorSig "Pair" $ FunctionT "x" (ScalarT (TypeVarT Map.empty "a") ftrue) (FunctionT "y" (ScalarT (TypeVarT Map.empty "b") ftrue) (ScalarT (DatatypeT "Pair" [ScalarT (TypeVarT Map.empty "a") ftrue, ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue))
  ]

defaultUnit = Pos (initialPos "Unit") $ DataDecl "Unit" [] [] []

defaultShowClass = Pos (initialPos "ShowD") $ DataDecl "ShowD" ["a"] [] [
    -- ShowDict ::  (a -> String) -> ShowD a
    ConstructorSig "ShowDict" $ FunctionT "y"
      (FunctionT "x" (ScalarT (TypeVarT Map.empty "a") ftrue) (ScalarT (DatatypeT "String" [] []) ftrue))
      (ScalarT (DatatypeT "ShowD" [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue)
    ]

defaultShowFunc = Pos (initialPos "show") $ FuncDecl "show$TC"
    (Monotype (FunctionT "dict" (ScalarT (DatatypeT "ShowD" [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue)
              (FunctionT "thing" (ScalarT (TypeVarT Map.empty "a") ftrue)
              (ScalarT (DatatypeT "String" [] []) ftrue))))

intShow = Pos (initialPos "showInt") $ FuncDecl "show<Int>"
    (Monotype (ScalarT (DatatypeT "ShowD" [ScalarT (DatatypeT "Int" [] []) ftrue] []) ftrue))
