{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}
module Types.Program where

import Synquid.Error
import Types.Common
import Types.Type
import Types.Environment

import Control.Lens
import Data.Serialize
import GHC.Generics

-- | Program skeletons parametrized by information stored symbols, conditionals, and by node types
data BareProgram t =
  PSymbol Id |                                -- ^ Symbol (variable or constant)
  PApp Id [Program t] |              -- ^ Function application
  PFun Id (Program t) |                       -- ^ Lambda abstraction
  PHole                                     -- ^ Hole (program to fill in)
  deriving (Eq, Ord, Functor, Generic)

-- | Programs annotated with types
data Program t = Program {
  content :: BareProgram t,
  typeOf :: t
} deriving (Functor, Generic)

instance Eq (Program t) where
  (==) (Program l _) (Program r _) = l == r

instance Ord (Program t) where
  (<=) (Program l _) (Program r _) = l <= r

-- | Untyped programs
type UProgram = Program TypeSkeleton
-- | Simple-typed programs
type TProgram = Program TypeSkeleton

instance Serialize Environment
instance Serialize DatatypeDef
instance Serialize t => Serialize (BareProgram t)
instance Serialize t => Serialize (Program t)
instance Serialize Kind
instance Serialize TypeSkeleton
instance Serialize SchemaSkeleton

-- | Constructor signature: name and type
data ConstructorSig = ConstructorSig Id TypeSkeleton
  deriving (Eq, Ord)

data BareDeclaration =
  TypeDecl TypeSkeleton TypeSkeleton |                                  -- ^ Type name, variables, and definition
  FuncDecl Id SchemaSkeleton |                                     -- ^ Function name and signature
  DataDecl Id [Id] [ConstructorSig] |     -- ^ Datatype name, type parameters, and constructor definitions
  SynthesisGoal Id UProgram                                 -- ^ Name and template for the function to reconstruct
  deriving (Eq, Ord)

type Declaration = Pos BareDeclaration

-- | Synthesis goal
data Goal = Goal {
  gName :: Id,                  -- ^ Function name
  gEnvironment :: Environment,  -- ^ Enclosing environment
  gSpec :: SchemaSkeleton,             -- ^ Specification
  gImpl :: UProgram,            -- ^ Implementation template
  gDepth :: Int,                -- ^ Maximum level of auxiliary goal nesting allowed inside this goal
  gSourcePos :: SourcePos       -- ^ Source Position
} deriving (Eq, Ord)
