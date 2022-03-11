module Database.Prelude where

import Text.Printf
import Text.Parsec.Pos ( initialPos )
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text

import Types.Common
import Types.Program
import Types.Type
import Synquid.Error ( Pos(Pos) )

--------------------------------------------------------------------------------
---------------------- Default Functions and Datatypes -------------------------
--------------------------------------------------------------------------------

defaultLibrary :: [Declaration]
defaultLibrary = concat [ defaultFuncs
                        , defaultDatatypes
                        , defaultTypeclassInstances
                        ]

------- Default functions 

defaultFuncs :: [Declaration]
defaultFuncs = [ Pos (initialPos "fst") $ FuncDecl "fst" $ Monotype (FunctionT "p" (DatatypeT "Pair" [(TypeVarT "a"), (TypeVarT "b")]) (TypeVarT "a"))
               , Pos (initialPos "snd") $ FuncDecl "snd" $ Monotype (FunctionT "p" (DatatypeT "Pair" [(TypeVarT "a"), (TypeVarT "b")]) (TypeVarT "b"))
               ]

------- Default datatypes 

defaultDatatypes :: [Declaration]
defaultDatatypes = [ defaultList
                   , defaultPair
                   , defaultUnit
                   , defaultInt
                   , defaultBool
                   , defaultChar
                   , defaultFloat
                   , defaultDouble
                   , defaultFun    -- special datatype for higher orders
                   ]

defaultList :: Declaration
defaultList = Pos (initialPos "List") $ DataDecl "List" ["a"] 
  [ ConstructorSig "Nil" (DatatypeT "List" [TypeVarT "a"])
  , ConstructorSig "Cons" $ FunctionT "x" (TypeVarT "a") (FunctionT "xs" (DatatypeT "List" [TypeVarT "a"]) (DatatypeT "List" [TypeVarT "a"]))
  ]

defaultPair :: Declaration
defaultPair = Pos (initialPos "Pair") $ DataDecl "Pair" ["a", "b"] [
    ConstructorSig "Pair" $ FunctionT "x" (TypeVarT "a") (FunctionT "y" (TypeVarT "b") (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"]))
  ]

defaultFun :: Declaration
defaultFun = Pos (initialPos "Fun") $ DataDecl "Fun" ["a", "b"] []

defaultInt :: Declaration
defaultInt = Pos (initialPos "Int") $ DataDecl "Int" [] []

defaultBool :: Declaration
defaultBool = Pos (initialPos "Bool") $ DataDecl "Bool" [] []

defaultChar :: Declaration
defaultChar = Pos (initialPos "Char") $ DataDecl "Char" [] []

defaultFloat :: Declaration
defaultFloat = Pos (initialPos "Float") $ DataDecl "Float" [] []

defaultDouble :: Declaration
defaultDouble = Pos (initialPos "Double") $ DataDecl "Double" [] []

defaultUnit :: Declaration
defaultUnit = Pos (initialPos "Unit") $ DataDecl "Unit" [] []


------- Default typeclass instances
-- This is only a subset of those predefinted in Haskell:
-- Full report: https://www.haskell.org/onlinereport/basic.html
-- In short: We also might want:
-- Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Floating,
-- Integral, RealFloat, Fractional, RealFrac
defaultTypeclassInstances :: [Declaration]
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
    ]


mkInstance :: Id -> TypeSkeleton -> Declaration
mkInstance tyclassName instanceType = let instanceName = typeName instanceType
                                      in Pos (initialPos "tcBuiltin") $
                                          FuncDecl (Text.pack $ printf "%s0%s%s" tyclassInstancePrefix tyclassName instanceName) $ Monotype $
                                            (DatatypeT (tyclassPrefix `Text.append` tyclassName) [instanceType])

listInstance :: Id -> Declaration
listInstance tyclassName = let instanceType = TypeVarT "a"
                               listInstance = DatatypeT "List" [instanceType]
                               listInstanceName = longScalarName listInstance
                               instanceName = longScalarName instanceType
                           in Pos (initialPos "tcBuiltin") $
                                FuncDecl (Text.pack $ printf "%s0%s%s" tyclassInstancePrefix tyclassName listInstanceName) $ Monotype $
                                  FunctionT "tc" instanceType (DatatypeT (tyclassPrefix `Text.append` tyclassName) [listInstance])


------- Util type definitions

intType :: TypeSkeleton
intType = nullDatatype "Int"

boolType :: TypeSkeleton
boolType = nullDatatype "Bool"

charType :: TypeSkeleton
charType = nullDatatype "Char"

floatType :: TypeSkeleton
floatType = nullDatatype "Float"

doubleType :: TypeSkeleton
doubleType = nullDatatype "Double"

unitType :: TypeSkeleton
unitType = nullDatatype "Unit"