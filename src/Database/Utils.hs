module Database.Utils where

import Types.Program
import Types.Type
import Synquid.Error (Pos(Pos))
import Synquid.Type

import Text.Printf
import Text.Parsec.Pos (initialPos)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

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
  defaultDts,
  defaultTypeclassInstances
  ]

pairTy = TyAppT (TyAppT (DatatypeT "Pair") (TypeVarT "a")) (TypeVarT "b")
listTy = TyAppT (DatatypeT "List") (TypeVarT "a")

defaultFuncs = [
    Pos (initialPos "fst") $ FuncDecl "fst" (
        Monotype $ (FunctionT "p" pairTy (TypeVarT "a")))
  , Pos (initialPos "snd") $ FuncDecl "snd" (
        Monotype $ (FunctionT "p" pairTy (TypeVarT "b")))
  ]

-- defaultFuncs = []

defaultDts = [
  defaultList, defaultPair, defaultUnit,
  defaultInt, defaultBool, defaultChar,
  defaultFloat, defaultDouble]

defaultList = Pos (initialPos "List") $ DataDecl "List" ["a"] [
    ConstructorSig "Nil" listTy
  , ConstructorSig "Cons" $ 
        FunctionT "x" (TypeVarT "a") (FunctionT "xs" listTy listTy)
  ]

defaultPair = Pos (initialPos "Pair") $ DataDecl "Pair" ["a", "b"] [
    ConstructorSig "Pair" $ 
        FunctionT "x" (TypeVarT "a") $
        FunctionT "y" (TypeVarT "b") pairTy
  ]


-- defaultFun = Pos (initialPos "Fun") $ DataDecl "Fun" ["a", "b"] [] []

-- defaultTyApp = Pos (initialPos "TyApp") $ DataDecl "TyApp" ["a", "b"] [] []

-- This is only a subset of those predefinted in Haskell:
-- Full report: https://www.haskell.org/onlinereport/basic.html
-- In short: We also might want:
-- Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Floating,
-- Integral, RealFloat, Fractional, RealFrac
defaultTypeclassInstances =
    [
    {-  mkInstance "Show" intType
    , mkInstance "Show" boolType
    , mkInstance "Show" charType
    , mkInstance "Show" intType
    , mkInstance "Show" floatType
    , mkInstance "Show" doubleType
    , mkInstance "Show" unitType
    , -}
      mkInstance "Eq" intType
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
    , mkSupertype "Ord" "Eq"
    , mkSupertype "Num" "Ord"
    -- , mkSupertype "Num" "Eq"
    ]


-- mkInstance creates something like
-- @@hplusTCInstance@@0EqInt :: @@hplusTC@@Eq Int
mkInstance :: String -> TypeSkeleton -> Declaration
mkInstance tyclassName instanceType = let
    instanceName = scalarName instanceType
    in Pos (initialPos "tcBuiltin") $
        FuncDecl (printf "%s0%s%s" tyclassInstancePrefix tyclassName instanceName) $ 
            Monotype $ TyAppT (DatatypeT (tyclassPrefix ++ tyclassName)) instanceType


-- create the function that converts from a subtype to a supertype
-- (e.g., Ord a => Eq a)
mkSupertype :: String -> String -> Declaration
mkSupertype subtype supertype = let
  instanceType = mkTyVar "a"
  in Pos (initialPos "tcBuiltin") $
    FuncDecl (printf "%s0%s%s" tyclassInstancePrefix subtype supertype) $
      Monotype (FunctionT "tc"
        (TyAppT (DatatypeT (tyclassPrefix ++ subtype)) instanceType)
        (TyAppT (DatatypeT (tyclassPrefix ++ supertype)) instanceType))

listInstance :: String -> Declaration
listInstance tyclassName = let
    instanceType = mkTyVar "a"
    listInstance = TyAppT (DatatypeT "List") instanceType
    listInstanceName = longScalarName listInstance
    instanceName = longScalarName instanceType
    in Pos (initialPos "tcBuiltin") $
        FuncDecl (printf "%s0%s%s" tyclassInstancePrefix tyclassName listInstanceName) $ 
            Monotype $
                FunctionT "tc" instanceType $
                    TyAppT (DatatypeT (tyclassPrefix ++ tyclassName)) listInstance

mkTyVar a = TypeVarT a

intType = DatatypeT "Int"
boolType = DatatypeT "Bool"
charType = DatatypeT "Char"

floatType = DatatypeT "Float"
doubleType = DatatypeT "Double"
unitType = DatatypeT "Unit"

maybeType = DatatypeT "Maybe"

defaultInt = Pos (initialPos "Int") $ DataDecl "Int" [] []
defaultBool = Pos (initialPos "Bool") $ DataDecl "Bool" [] []
defaultChar = Pos (initialPos "Char") $ DataDecl "Char" [] []
defaultFloat = Pos (initialPos "Float") $ DataDecl "Float" [] []
defaultDouble = Pos (initialPos "Double") $ DataDecl "Double" [] []
defaultUnit = Pos (initialPos "Unit") $ DataDecl "Unit" [] []
