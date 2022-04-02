module Database.Prelude
  ( defaultLibrary
  ) where

import qualified Data.Text                     as Text
import           Text.Parsec.Pos                ( initialPos )
import           Text.Printf                    ( printf )

import           Compiler.Error                 ( Pos(Pos) )
import           Types.Common
import           Types.Program
import           Types.Type

--------------------------------------------------------------------------------
---------------------- Default Functions and Datatypes -------------------------
--------------------------------------------------------------------------------

defaultLibrary :: [Declaration]
defaultLibrary =
  concat [defaultFuncs, defaultDatatypes, defaultTypeclassInstances]

------- Default functions 

defaultFuncs :: [Declaration]
defaultFuncs =
  [ FuncDecl "fst" $ Monotype
    (FunctionT "p"
               (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
               (TypeVarT "a")
    )
  , FuncDecl "snd" $ Monotype
    (FunctionT "p"
               (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
               (TypeVarT "b")
    )
  ]

------- Default datatypes 

defaultDatatypes :: [Declaration]
defaultDatatypes =
  [ defaultList
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
defaultList = DataDecl
  "List"
  ["a"]
  [ ConstructorSig "Nil" (DatatypeT "List" [TypeVarT "a"])
  , ConstructorSig "Cons" $ FunctionT
    "x"
    (TypeVarT "a")
    (FunctionT "xs"
               (DatatypeT "List" [TypeVarT "a"])
               (DatatypeT "List" [TypeVarT "a"])
    )
  ]

defaultPair :: Declaration
defaultPair = DataDecl
  "Pair"
  ["a", "b"]
  [ ConstructorSig "Pair" $ FunctionT
      "x"
      (TypeVarT "a")
      (FunctionT "y"
                 (TypeVarT "b")
                 (DatatypeT "Pair" [TypeVarT "a", TypeVarT "b"])
      )
  ]

defaultFun :: Declaration
defaultFun = DataDecl "Fun" ["a", "b"] []

defaultInt :: Declaration
defaultInt = DataDecl "Int" [] []

defaultBool :: Declaration
defaultBool = DataDecl "Bool" [] []

defaultChar :: Declaration
defaultChar = DataDecl "Char" [] []

defaultFloat :: Declaration
defaultFloat = DataDecl "Float" [] []

defaultDouble :: Declaration
defaultDouble = DataDecl "Double" [] []

defaultUnit :: Declaration
defaultUnit = DataDecl "Unit" [] []


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
  , mkInstance "Eq"   intType
  , mkInstance "Eq"   boolType
  , mkInstance "Eq"   charType
  , mkInstance "Eq"   intType
  , mkInstance "Eq"   floatType
  , mkInstance "Eq"   doubleType
  , mkInstance "Eq"   unitType
  , mkInstance "Ord"  intType
  , mkInstance "Ord"  boolType
  , mkInstance "Ord"  charType
  , mkInstance "Ord"  intType
  , mkInstance "Ord"  floatType
  , mkInstance "Ord"  doubleType
  , mkInstance "Num"  intType
  , mkInstance "Num"  floatType
  , mkInstance "Num"  doubleType
  ]


mkInstance :: Id -> TypeSkeleton -> Declaration
mkInstance tyclassName instanceType =
  let instanceName = typeName instanceType
  in
    FuncDecl
        ( Text.pack
        $ printf "%s0%s%s" tyclassInstancePrefix tyclassName instanceName
        )
      $ Monotype
          (DatatypeT (tyclassPrefix `Text.append` tyclassName) [instanceType])

listInstance :: Id -> Declaration
listInstance tyclassName =
  let instanceType     = TypeVarT "a"
      listInstance     = DatatypeT "List" [instanceType]
      listInstanceName = longScalarName listInstance
      instanceName     = longScalarName instanceType
  in  FuncDecl
          (Text.pack $ printf "%s0%s%s"
                              tyclassInstancePrefix
                              tyclassName
                              listInstanceName
          )
        $ Monotype
        $ FunctionT
            "tc"
            instanceType
            (DatatypeT (tyclassPrefix `Text.append` tyclassName) [listInstance])


------- Util type definitions

floatType :: TypeSkeleton
floatType = nullDatatype "Float"

unitType :: TypeSkeleton
unitType = nullDatatype "Unit"
