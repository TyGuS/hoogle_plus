{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}

module Types.Program where

import Synquid.Error
import Synquid.Logic
import Types.Common
import Types.Environment
import Types.Type

import Control.Lens
import Data.Serialize
import GHC.Generics

-- | One case inside a pattern match expression
data Case t =
    Case
        { constructor :: Id -- ^ Constructor name
        , argNames :: [Id] -- ^ Bindings for constructor arguments
        , expr :: Program t -- ^ Result of the match in this case
        }
    deriving (Eq, Ord, Functor, Generic)

-- | Program skeletons parametrized by information stored symbols, conditionals, and by node types
data BareProgram t
    = PSymbol Id -- ^ Symbol (variable or constant)
    | PApp Id [Program t] -- ^ Function application
    | PFun Id (Program t) -- ^ Lambda abstraction
    | PIf (Program t) (Program t) (Program t) -- ^ Conditional
    | PMatch (Program t) [Case t] -- ^ Pattern match on datatypes
    | PFix [Id] (Program t) -- ^ Fixpoint
    | PLet Id (Program t) (Program t) -- ^ Let binding
    | PHole -- ^ Hole (program to fill in)
    | PErr -- ^ Error
    deriving (Eq, Ord, Functor, Generic)

-- | Programs annotated with types
data Program t =
    Program
        { content :: BareProgram t
        , typeOf :: t
        }
    deriving (Functor, Generic)

instance Eq (Program t) where
    (==) (Program l _) (Program r _) = l == r

instance Ord (Program t) where
    (<=) (Program l _) (Program r _) = l <= r

-- | Untyped programs
type UProgram = Program RType

-- | Refinement-typed programs
type RProgram = Program RType

-- | Simple-typed programs
type TProgram = Program SType

type SProgram = Program (RType, RType)

-- | One case in a measure definition: constructor name, arguments, and body
data MeasureCase =
    MeasureCase Id [Id] Formula
    deriving (Eq, Ord, Generic)

-- | User-defined measure function representation
data MeasureDef =
    MeasureDef
        { _inSort :: Sort
        , _outSort :: Sort
        , _definitions :: [MeasureCase]
        , _postcondition :: Formula
        }
    deriving (Eq, Ord, Generic)

makeLenses ''MeasureDef

instance Serialize Formula

instance Serialize Sort

instance Serialize UnOp

instance Serialize BinOp

instance Serialize Environment

instance Serialize PredSig

instance Serialize DatatypeDef

instance Serialize MeasureCase

instance Serialize MeasureDef

instance Serialize t => Serialize (Case t)

instance Serialize t => Serialize (BareProgram t)

instance Serialize t => Serialize (Program t)

instance Serialize r => Serialize (TypeSkeleton r)

instance Serialize r => Serialize (BaseType r)

instance Serialize r => Serialize (SchemaSkeleton r)

-- | Constructor signature: name and type
data ConstructorSig =
    ConstructorSig Id RType
    deriving (Eq, Ord)

data BareDeclaration
    = TypeDecl Id [Id] RType -- ^ Type name, variables, and definition
    | FuncDecl Id RSchema -- ^ Function name and signature
    | DataDecl Id Id [Id] [(PredSig, Bool)] [ConstructorSig] -- ^ Datatype module, Datatype name, type parameters, predicate parameters, and constructor definitions
    | MeasureDecl Id Sort Sort Formula [MeasureCase] Bool -- ^ Measure name, input sort, output sort, postcondition, definition cases, and whether this is a termination metric
    | PredDecl PredSig -- ^ Module-level predicate
    | QualifierDecl [Formula] -- ^ Qualifiers
    | MutualDecl [Id] -- ^ Mutual recursion group
    | InlineDecl Id [Id] Formula -- ^ Inline predicate
    | SynthesisGoal Id UProgram -- ^ Name and template for the function to reconstruct
    deriving (Eq, Ord)

type Declaration = Pos BareDeclaration

-- | Synthesis goal
data Goal =
    Goal
        { gName :: Id -- ^ Function name
        , gEnvironment :: Environment -- ^ Enclosing environment
        , gSpec :: RSchema -- ^ Specification
        , gImpl :: UProgram -- ^ Implementation template
        , gDepth :: Int -- ^ Maximum level of auxiliary goal nesting allowed inside this goal
        , gSourcePos :: SourcePos -- ^ Source Position
        }
    deriving (Eq, Ord)
