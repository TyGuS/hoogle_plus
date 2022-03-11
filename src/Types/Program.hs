{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}
module Types.Program where

import Control.Lens
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Text ( Text )
import qualified Data.Text as Text
import GHC.Generics

import Synquid.Error
import Types.Common
import Types.Type
import Types.Environment

-- | Program skeletons parametrized by information stored symbols, conditionals, and by node types
data BareProgram t
  = PSymbol Id                                -- ^ Symbol (variable or constant)
  | PApp Id [Program t]                       -- ^ Function application
  | PFun Id (Program t)                       -- ^ Lambda abstraction
  | PHole                                     -- ^ Hole (program to fill in)
  deriving (Eq, Ord, Show, Functor, Generic)

-- | Programs annotated with types
data Program t = Program { content :: BareProgram t
                         , typeOf :: t
                         }
  deriving (Show, Functor, Generic)

instance Eq (Program t) where
  (==) (Program l _) (Program r _) = l == r

instance Ord (Program t) where
  (<=) (Program l _) (Program r _) = l <= r

type TProgram = Program TypeSkeleton    -- Typed program
type RProgram = Program (TypeSkeleton, TypeSkeleton) -- Program with refinement information
type AProgram = Program (TypeSkeleton, TypeSkeleton, TypeSkeleton)

-- | Constructor signature: name and type
data ConstructorSig = ConstructorSig Id TypeSkeleton
  deriving (Eq, Ord, Show)

data BareDeclaration
  = TypeDecl Id [Id] TypeSkeleton          -- ^ Type name, variables, and definition
  | FuncDecl Id SchemaSkeleton             -- ^ Function name and signature
  | DataDecl Id [Id] [ConstructorSig]      -- ^ Datatype name, type parameters, predicate parameters, and constructor definitions
  | SynthesisGoal Id TProgram              -- ^ Name and template for the function to reconstruct
  deriving (Eq, Ord, Show)

type Declaration = Pos BareDeclaration

-- | Synthesis goal
data Goal = Goal { gEnvironment :: Environment  -- ^ Enclosing environment
                 , gSpec :: SchemaSkeleton      -- ^ Specification
                 }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
--------------------------  Prefixes and Postfixes -----------------------------
--------------------------------------------------------------------------------

tyclassPrefix :: Id
tyclassPrefix = "@@hplusTC@@"

tyclassInstancePrefix :: Id
tyclassInstancePrefix = "@@hplusTCInstance@@"

tyclassArgBase :: Id
tyclassArgBase = "tcarg"

hoPostfix :: Id
hoPostfix = "'ho'"

pairProj :: Id
pairProj = "pair_match"

isTyclass :: Id -> Bool
isTyclass f = tyclassArgBase `Text.isInfixOf` f || 
              tyclassInstancePrefix `Text.isPrefixOf` f

--------------------------------------------------------------------------------
-------------------------- Program Operations ----------------------------------
--------------------------------------------------------------------------------

untyped :: BareProgram TypeSkeleton -> TProgram
untyped c = Program c TopT

uHole :: TProgram
uHole = untyped PHole

isHole :: TProgram -> Bool
isHole (Program PHole _) = True
isHole _                 = False

eraseTypes :: TProgram -> TProgram
eraseTypes = fmap (const TopT)

symbolsOf :: Program t -> Set Id
symbolsOf (Program p _) = case p of
  PSymbol name -> Set.singleton name
  PApp fun arg -> fun `Set.insert` (Set.unions $ map symbolsOf arg)
  PFun x body  -> symbolsOf body
  _            -> Set.empty

constructorName (ConstructorSig name _) = name

isSynthesisGoal (Pos _ (SynthesisGoal _ _)) = True
isSynthesisGoal _ = False

{- Misc -}

unqualifiedName :: Id -> Id
unqualifiedName "" = ""
unqualifiedName f = if Text.last name == ')' then '(' `Text.cons` name else name
  where
    name = last (Text.splitOn "." f)

unqualifyFunc :: TProgram -> TProgram
unqualifyFunc (Program (PSymbol f) t)   = Program (PSymbol (unqualifiedName f)) t
unqualifyFunc (Program (PApp f args) t) = Program (PApp (unqualifiedName f) (map unqualifyFunc args)) t
unqualifyFunc (Program (PFun x body) t) = Program (PFun x (unqualifyFunc body)) t
unqualifyFunc p                         = p

untypeclass :: TProgram -> TProgram
untypeclass (Program (PSymbol f) t)   | isTyclass f = Program (PSymbol "") t
untypeclass (Program (PApp f args) t) | isTyclass f = Program (PSymbol "") t
                                      | otherwise   = Program (PApp f (map untypeclass args)) t
untypeclass p                                       = p