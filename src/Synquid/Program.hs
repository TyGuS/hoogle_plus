{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}

-- | Executable programs
module Synquid.Program where

import Synquid.Type
import Synquid.Error
import Synquid.Tokens
import Synquid.Util
import Types.Common
import Types.Type
import Types.Abstract
import Types.Program
import Types.Environment

import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Hashable
import Data.Maybe
import Data.Either
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import GHC.Generics hiding (to)

import Control.Monad
import Control.Lens

{- Program terms -}


untyped c = Program c AnyT
uHole = untyped PHole
isHole (Program PHole _) = True
isHole _ = False

eraseTypes :: TProgram -> UProgram
eraseTypes = fmap (const AnyT)

symbolName (Program (PSymbol name) _) = name
symbolList (Program (PSymbol name) _) = [name]
symbolList (Program (PApp f arg) _) = f : concatMap symbolList arg

symbolsOf (Program p _) = case p of
  PSymbol name -> Set.singleton name
  PApp fun arg -> fun `Set.insert` (Set.unions $ map symbolsOf arg)
  PFun x body -> symbolsOf body
  _ -> Set.empty

hasHole (Program p _) = case p of
  PApp fun arg -> or (map hasHole arg)
  PHole -> True
  _ -> False

{- Top-level definitions -}
-- | All symbols in an environment
allSymbols :: Environment -> Map Id SchemaSkeleton
allSymbols env = env ^. symbols

-- | 'lookupSymbol' @name env@ : type of symbol @name@ in @env@, including built-in constants
lookupSymbol :: Id -> Int -> Environment -> Maybe SchemaSkeleton
lookupSymbol name a env = Map.lookup name (allSymbols env)

-- | 'isBound' @tv env@: is type variable @tv@ bound in @env@?
isBound :: Environment -> Id -> Bool
isBound env tv = tv `elem` env ^. boundTypeVars

addArgument :: Id -> TypeSkeleton -> Environment -> Environment
addArgument name t = (arguments %~ Map.insert name (Monotype t))

addVariable :: Id -> TypeSkeleton -> Environment -> Environment
addVariable name t = addPolyVariable name (Monotype t)

addPolyVariable :: Id -> SchemaSkeleton -> Environment -> Environment
addPolyVariable name sch = symbols %~ Map.insert name sch

-- | 'addConstant' @name t env@ : add type binding @name@ :: Monotype @t@ to @env@
addConstant :: Id -> TypeSkeleton -> Environment -> Environment
addConstant name t = addPolyConstant name (Monotype t)

-- | 'addPolyConstant' @name sch env@ : add type binding @name@ :: @sch@ to @env@
addPolyConstant :: Id -> SchemaSkeleton -> Environment -> Environment
addPolyConstant name sch = addPolyVariable name sch . (constants %~ Set.insert name)

addUnresolvedConstant :: Id -> SchemaSkeleton -> Environment -> Environment
addUnresolvedConstant name sch = unresolvedConstants %~ Map.insert name sch

removeVariable :: Id -> Environment -> Environment
removeVariable name env = case Map.lookup name (allSymbols env) of
  Nothing -> env
  Just sch -> over symbols (Map.delete name) . over constants (Set.delete name) $ env

addTypeSynonym :: TypeSkeleton -> TypeSkeleton -> Environment -> Environment
addTypeSynonym syn t = over typeSynonyms (Map.insert syn t)

-- | 'addDatatype' @name env@ : add datatype @name@ to the environment
addDatatype :: Id -> DatatypeDef -> Environment -> Environment
addDatatype name dt = over datatypes (Map.insert name dt)

-- | 'lookupConstructor' @ctor env@ : the name of the datatype for which @ctor@ is registered as a constructor in @env@, if any
lookupConstructor :: Id -> Environment -> Maybe Id
lookupConstructor ctor env = let m = Map.filter (\dt -> ctor `elem` dt ^. constructors) (env ^. datatypes)
    in if Map.null m
        then Nothing
        else Just $ fst $ Map.findMin m

-- | 'addTypeVar' @a@ : Add bound type variable @a@ to the environment
addTypeVar :: Id -> Environment -> Environment
addTypeVar a = over boundTypeVars (a :)

{- Input language declarations -}

constructorName (ConstructorSig name _) = name

isSynthesisGoal (Pos _ (SynthesisGoal _ _)) = True
isSynthesisGoal _ = False

{- Misc -}

unresolvedType env ident = (env ^. unresolvedConstants) Map.! ident
-- unresolvedSpec goal = unresolvedType (gEnvironment goal) (gName goal)
unresolvedSpec goal = gSpec goal