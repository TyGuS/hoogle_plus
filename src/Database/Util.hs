module Database.Util where

import Types.Program
import Types.Type
import Synquid.Logic
import Synquid.Error (Pos(Pos))

import Text.Parsec.Pos (initialPos)
import qualified Data.Set as Set
import qualified Data.Map as Map

tyclassPrefix = "@@hplusTC@@"
tyclassInstancePrefix = "@@hplusTCInstance@@"
tyclassArgBase = "tcarg"

(>.<) :: Ord a => [a] -> [a] -> [a]
xs >.< ys = let ys' = Set.fromList ys in filter (flip Set.member ys') xs

(>.>) :: Ord a => [a] -> [a] -> [a]
xs >.> ys = let ys' = Set.fromList ys in filter (flip Set.notMember ys') xs

defaultLibrary = concat [
  defaultFuncs,
  defaultDts,
  defaultTypeclassInstances
  ]

-- Default Library
defaultFuncs = [ Pos (initialPos "fst") $ FuncDecl "fst" (Monotype (FunctionT "p" (ScalarT (DatatypeT "Pair" [ScalarT (TypeVarT Map.empty "a") ftrue, ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue) (ScalarT (TypeVarT Map.empty "a") ftrue)))
                , Pos (initialPos "snd") $ FuncDecl "snd" (Monotype (FunctionT "p" (ScalarT (DatatypeT "Pair" [ScalarT (TypeVarT Map.empty "a") ftrue, ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue) (ScalarT (TypeVarT Map.empty "b") ftrue)))

                ]

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

defaultTypeclassInstances = [
  Pos (initialPos "tcBuiltin") $
    FuncDecl (tyclassInstancePrefix ++ "01ShowInt") $ Monotype $
      -- Monotype (FunctionT "p" (ScalarT (DatatypeT "Int" [] []) ftrue) $
      ScalarT (DatatypeT (tyclassPrefix ++ "Show") [mkTyVar "a"] []) ftrue
  -- , Pos (initialPos "tcBuiltIn") $ FuncDecl (tyclassInstancePrefix ++ "99Show") (
  --       Monotype (FunctionT "p" (ScalarT (DatatypeT "Int" [] []) ftrue)
  --                   (ScalarT (DatatypeT (tyclassPrefix++"Show") [
  --                     ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue)))
  -- , Pos (initialPos "tcBuiltIn") $ FuncDecl (tyclassInstancePrefix ++ "100Show")(Monotype (FunctionT "p" (ScalarT (TypeVarT Map.empty "a") ftrue) (ScalarT (DatatypeT (tyclassPrefix++"Show") [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue)))
  ]

mkTyVar str = ScalarT (TypeVarT (Map.empty) str) ftrue
