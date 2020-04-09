module Database.Util where

import Types.Program
import Types.Type
import Synquid.Logic
import Synquid.Error (Pos(Pos))
import Synquid.Type

import Text.Printf
import Text.Parsec.Pos (initialPos)
import qualified Data.Set as Set
import qualified Data.Map as Map

tyclassPrefix = "@@hplusTC@@"
tyclassInstancePrefix = "@@hplusTCInstance@@"
tyclassArgBase = "tcarg"
hoPostfix = "'ho'"

(>.<) :: Ord a => [a] -> [a] -> [a]
xs >.< ys = let ys' = Set.fromList ys in filter (flip Set.member ys') xs

(>.>) :: Ord a => [a] -> [a] -> [a]
xs >.> ys = let ys' = Set.fromList ys in filter (flip Set.notMember ys') xs


defaultLibrary = concat [
  defaultFuncs,
  defaultFun,
  defaultDts,
  defaultTypeclassInstances
  ]

{-
defaultFuncs = [
    Pos (initialPos "fst") $ FuncDecl "fst" (Monotype $
      (FunctionT "p" (ScalarT (
        DatatypeT "Pair" [
          ScalarT (TypeVarT Map.empty "a") ftrue,
          ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue) $
        (ScalarT (TypeVarT Map.empty "a") ftrue)))
  , Pos (initialPos "snd") $ FuncDecl "snd" (Monotype $
      (FunctionT "p" (ScalarT (
        DatatypeT "Pair" [
          ScalarT (TypeVarT Map.empty "a") ftrue,
          ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue) $
        (ScalarT (TypeVarT Map.empty "b") ftrue)))
  ]
-}
defaultFuncs = []

defaultDts = [
  defaultList, defaultPair, defaultUnit,
  defaultInt, defaultBool, defaultChar,
  defaultFloat, defaultDouble]

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
    {-
    ConstructorSig "Pair" $ FunctionT "x" (ScalarT (TypeVarT Map.empty "a") ftrue) $
      (FunctionT "y" (ScalarT (TypeVarT Map.empty "b") ftrue) $
        (ScalarT (DatatypeT "Pair" [
          ScalarT (TypeVarT Map.empty "a") ftrue,
          ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue))
    -}
    ]


defaultFun = [Pos (initialPos "Fun") $ DataDecl "Fun" ["a", "b"] [] []]

-- This is only a subset of those predefinted in Haskell:
-- Full report: https://www.haskell.org/onlinereport/basic.html
-- In short: We also might want:
-- Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Floating,
-- Integral, RealFloat, Fractional, RealFrac
defaultTypeclassInstances =
    [ mkInstance "Show" intType
    , mkInstance "Show" boolType
    , mkInstance "Show" charType
    , mkInstance "Show" intType
    , mkInstance "Show" floatType
    , mkInstance "Show" doubleType
    , mkInstance "Show" unitType
    , mkInstance "Eq" intType
    , mkInstance "Eq" boolType
    , mkInstance "Eq" charType
    , mkInstance "Eq" intType
    , mkInstance "Eq" floatType
    , mkInstance "Eq" doubleType
    , mkInstance "Eq" unitType
    , mkInstance "Ord" intType
    , mkInstance "Ord" boolType
    , mkInstance "Ord" charType
    , mkInstance "Ord" intType
    , mkInstance "Ord" floatType
    , mkInstance "Ord" doubleType
    , mkInstance "Num" intType
    , mkInstance "Num" floatType
    , mkInstance "Num" doubleType
    , mkInstance "Read" intType
    ]


mkInstance :: String -> RType -> Declaration
mkInstance tyclassName instanceType = let
    instanceName = scalarName instanceType
    in Pos (initialPos "tcBuiltin") $
        FuncDecl (printf "%s0%s%s" tyclassInstancePrefix tyclassName instanceName) $ Monotype $
          ScalarT (DatatypeT (tyclassPrefix ++ tyclassName) [instanceType] []) ftrue

listInstance :: String -> Declaration
listInstance tyclassName = let
    instanceType = mkTyVar "a"
    listInstance = ScalarT (DatatypeT "List" [instanceType] []) ftrue
    listInstanceName = longScalarName listInstance
    instanceName = longScalarName instanceType
    in Pos (initialPos "tcBuiltin") $
        FuncDecl (printf "%s0%s%s" tyclassInstancePrefix tyclassName listInstanceName) $ Monotype $
          FunctionT "tc" instanceType $
            ScalarT (DatatypeT (tyclassPrefix ++ tyclassName) [listInstance] []) ftrue

mkTyVar str = ScalarT (TypeVarT (Map.empty) str) ftrue

intType = ScalarT (DatatypeT "Int" [] []) ftrue
boolType = ScalarT (DatatypeT "Bool" [] []) ftrue
charType = ScalarT (DatatypeT "Char" [] []) ftrue

floatType = ScalarT (DatatypeT "Float" [] []) ftrue
doubleType = ScalarT (DatatypeT "Double" [] []) ftrue
unitType = ScalarT (DatatypeT "Unit" [] []) ftrue

defaultInt = Pos (initialPos "Int") $ DataDecl "Int" [] [] []
defaultBool = Pos (initialPos "Bool") $ DataDecl "Bool" [] [] []
defaultChar = Pos (initialPos "Char") $ DataDecl "Char" [] [] []
defaultFloat = Pos (initialPos "Float") $ DataDecl "Float" [] [] []
defaultDouble = Pos (initialPos "Double") $ DataDecl "Double" [] [] []
defaultUnit = Pos (initialPos "Unit") $ DataDecl "Unit" [] [] []
