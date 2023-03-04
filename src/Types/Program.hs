module Types.Program
  (
    -- * Program
    Program(..)
  , BareProgram(..)
  , TProgram
  , AProgram
  , RProgram
  , ann
  , withContent
  , programSize
  , untyped
  , funcp
  , varp
  , uHole
  , isHole
  , eraseTypes
  , symbolsOf
  , allSymbolsIn
  , constructorName
  , unqualifiedName
  , unqualifyFunc
  , untypeclass
  , numArguments
  , argumentsOf
  , canonicalize
  , unsuffixName

    -- * Declarations
  , Declaration(..)
  , ConstructorSig(..)
  , Example(..)
  , Examples(..)
  , overExamples
  , Goal(..)
  , dummyDecl
  ) where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Serialize                 ( Serialize )
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import           Types.Common
import           Types.Environment
import           Types.Type
import Utility.Utils

-- | Program skeletons parametrized by information stored symbols, conditionals, and by node types
data BareProgram t
  = PSymbol Id                                -- ^ Symbol (variable or constant)
  | PApp Id [Program t]                       -- ^ Function application
  | PFun Id (Program t)                       -- ^ Lambda abstraction
  | PHole                                     -- ^ Hole (program to fill in)
  -- | PFilled Int                               -- ^ Filled hole
  deriving (Eq, Ord, Show, Functor, Generic)

instance ToJSON t => ToJSON (BareProgram t)
instance FromJSON t => FromJSON (BareProgram t)

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

instance ToJSON t => ToJSON (Program t)
instance FromJSON t => FromJSON (Program t)

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

data Example = Example
  { inputs :: [String]
  , output :: String
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Example
instance FromJSON Example
instance Serialize Example

newtype Examples = Examples { unExamples :: [Example] }
  deriving (Eq, Ord, Show, Generic)

instance Semigroup Examples where
  Examples xs <> Examples ys = Examples (xs ++ ys)

overExamples :: ([Example] -> [Example]) -> Examples -> Examples
overExamples f (Examples xs) = Examples (f xs)

-- | Synthesis goal
data Goal = Goal
  { gEnvironment :: Environment  -- ^ Enclosing environment
  , gSpec        :: TypeSkeleton -- ^ Specification
  , gExamples    :: [Example]
  }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-------------------------- Program Operations ----------------------------------
--------------------------------------------------------------------------------

ann :: Program t -> t -> Program t
ann (Program c t1) t2 = Program c t2

withContent :: (t -> t) -> Program t -> Program t
withContent f (Program c t) = Program c (f t)

untyped :: BareProgram TypeSkeleton -> TProgram
untyped c = Program c TopT

funcp :: Id -> TProgram -> TProgram
funcp x = untyped . PFun x

varp :: Id -> TProgram
varp = untyped . PSymbol

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

numArguments :: Program t -> Int
numArguments (Program p _) = case p of
  PFun _ body -> 1 + numArguments body
  _ -> 0

argumentsOf :: Program t -> [Id]
argumentsOf (Program p _) = case p of
  PFun x body -> x : argumentsOf body
  _ -> []

canonicalize :: Program t -> State (Map Id Id) (Program t)
canonicalize (Program p t) = case p of
  PSymbol s | "arg" `Text.isPrefixOf` s -> do st <- get
                                              if s `Map.member` st
                                                then return (Program (PSymbol (st Map.! s)) t)
                                                else do
                                                  let i = Map.size st
                                                  let newArg = Text.pack ("arg" ++ show i)
                                                  modify $ Map.insert s newArg
                                                  return (Program (PSymbol newArg) t)
            | otherwise -> return (Program (PSymbol s) t)
  PApp f args -> do
    st <- get
    let i = Map.size st
    let newArg = Text.pack ("arg" ++ show i)
    when (("arg" `Text.isPrefixOf` f) && f `Map.notMember` st) (modify $ Map.insert f newArg)
    args' <- mapM canonicalize args
    st <- get
    let newFname = if "arg" `Text.isPrefixOf` f then st Map.! f else f
    return (Program (PApp newFname args') t)
  PFun x body -> do
    st <- get
    let i = Map.size st
    let newArg = Text.pack ("arg" ++ show i)
    when (("arg" `Text.isPrefixOf` x) && x `Map.notMember` st) (modify $ Map.insert x newArg)
    st <- get
    let x' = if "arg" `Text.isPrefixOf` x then st Map.! x else x
    body' <- canonicalize body
    return (Program (PFun x' body') t)


{- Misc -}

unsuffixName :: Map Id Id -> Id -> Id
unsuffixName nameMap f = 
  let f' = removeLast '_' f
   in removeLast '_' $ stripSuffix hoPostfix $ fromMaybe f' (Map.lookup f' nameMap)

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
