module Types.Program
  (
    -- * Program
    Program(..)
  , BareProgram(..)
  , TProgram
  , AProgram
  , RProgram
  , programSize
  , untyped
  , uHole
  , isHole
  , eraseTypes
  , symbolsOf
  , allSymbolsIn
  , constructorName
  , unqualifiedName
  , unqualifyFunc
  , untypeclass

    -- * Declarations
  , Declaration(..)
  , ConstructorSig(..)
  , Goal(..)
  , dummyDecl
  ) where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )

import           Types.Common
import           Types.Environment
import           Types.Type

-- | Program skeletons parametrized by information stored symbols, conditionals, and by node types
data BareProgram t
  = PSymbol Id                                -- ^ Symbol (variable or constant)
  | PApp Id [Program t]                       -- ^ Function application
  | PFun Id (Program t)                       -- ^ Lambda abstraction
  | PHole                                     -- ^ Hole (program to fill in)
  deriving (Eq, Ord, Show, Functor, Generic)

-- | Programs annotated with types
data Program t = Program
  { content :: BareProgram t
  , typeOf  :: t
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

data Declaration
  = TypeDecl Id [Id] TypeSkeleton          -- ^ Type name, variables, and definition
  | FuncDecl Id SchemaSkeleton             -- ^ Function name and signature
  | DataDecl Id [Id] [ConstructorSig]      -- ^ Datatype name, type parameters, predicate parameters, and constructor definitions
  deriving (Eq, Ord, Show)

dummyDecl :: Declaration
dummyDecl = FuncDecl "_dummy" (Monotype TopT)

-- | Synthesis goal
data Goal = Goal
  { gEnvironment :: Environment  -- ^ Enclosing environment
  , gSpec        :: TypeSkeleton -- ^ Specification
  }
  deriving (Eq, Ord, Show)

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
symbolsOf = Set.fromList . allSymbolsIn

allSymbolsIn :: Program t -> [Id]
allSymbolsIn (Program p _) = case p of
  PSymbol name  -> [name]
  PApp fun args -> fun : concatMap allSymbolsIn args
  PFun x   body -> allSymbolsIn body
  _             -> []

constructorName :: ConstructorSig -> Id
constructorName (ConstructorSig name _) = name

programSize :: Program t -> Int
programSize (Program p _) = case p of
  PSymbol _   -> 1
  PApp _ args -> 1 + sum (map programSize args)
  PFun _ body -> 1 + programSize body
  _           -> 0

{- Misc -}

unqualifiedName :: Id -> Id
unqualifiedName "" = ""
unqualifiedName f  = if Text.last name == ')'
  then '(' `Text.cons` name
  else name
  where name = last (Text.splitOn "." f)

unqualifyFunc :: TProgram -> TProgram
unqualifyFunc (Program (PSymbol f) t) = Program (PSymbol (unqualifiedName f)) t
unqualifyFunc (Program (PApp f args) t) =
  Program (PApp (unqualifiedName f) (map unqualifyFunc args)) t
unqualifyFunc (Program (PFun x body) t) =
  Program (PFun x (unqualifyFunc body)) t
unqualifyFunc p = p

untypeclass :: TProgram -> TProgram
untypeclass (Program (PSymbol f) t) | isTyclass f = Program (PSymbol "") t
untypeclass (Program (PApp f args) t)
  | isTyclass f = Program (PSymbol "") t
  | otherwise   = Program (PApp f (map untypeclass args)) t
untypeclass p = p
