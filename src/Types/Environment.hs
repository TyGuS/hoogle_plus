module Types.Environment
  (
    -- * Environment
    Environment(..)
  , SymbolLib
  , NameMapping
  , emptyEnv
  , loadEnv

    -- * Operations
  , allSymbols
  , lookupSymbol
  , isBound
  , addArgument
  , addComponent
  , modifySymbols
  , removeVariable
  , addTypeVar
  , findSymbol
  , getFirstOrderArgs
  , getHigherOrderArgs
  ) where

import           Control.Monad.State            ( StateT )
import           Data.Function                  ( on )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics                   ( Generic )

import           Database.Dataset
import           Types.Common
import           Types.Fresh
import           Types.Type
import           Utility.Utils


type SymbolLib = Map Id SchemaSkeleton
type NameMapping = Map Id Id

data Environment = Environment
  { getSymbols       :: SymbolLib                   -- ^ Components with their types
  , getArguments     :: [(Id, TypeSkeleton)]    -- ^ Function arguments, required in all the solutions
  , getBoundTypeVars :: [Id]                  -- ^ Bound type variables
  }
  deriving (Show, Generic)

instance Eq Environment where
  (==) e1 e2 = getSymbols e1 == getSymbols e2

instance Ord Environment where
  compare = compare `on` getSymbols

emptyEnv :: Environment
emptyEnv = Environment Map.empty [] []

loadEnv :: Environment
loadEnv =
  foldr (uncurry addComponent) emptyEnv (hplusComponents ++ hplusHigherOrders)

--------------------------------------------------------------------------------
--------------------------  Environment Operations -----------------------------
--------------------------------------------------------------------------------

allSymbols :: Environment -> SymbolLib
allSymbols = getSymbols

-- | 'lookupSymbol' @name env@ : type of symbol @name@ in @env@, including built-in constants
lookupSymbol :: Id -> Environment -> Maybe SchemaSkeleton
lookupSymbol name env = Map.lookup name (allSymbols env)

-- | 'isBound' @tv env@: is type variable @tv@ bound in @env@?
isBound :: Environment -> Id -> Bool
isBound env tv = tv `elem` getBoundTypeVars env

addArgument :: Id -> TypeSkeleton -> Environment -> Environment
addArgument name t (Environment symbols args bvs) =
  Environment symbols (args ++ [(name, t)]) bvs

modifySymbols :: (SymbolLib -> SymbolLib) -> Environment -> Environment
modifySymbols f (Environment symbols args bvs) =
  Environment (f symbols) args bvs

addComponent :: Id -> SchemaSkeleton -> Environment -> Environment
addComponent name sch (Environment symbols args bvs) =
  case name `Map.lookup` symbols of
    Nothing -> Environment (Map.insert name sch symbols) args bvs
    Just _ ->
      error $ "addComponent: symbol " ++ show name ++ " already defined"

removeVariable :: Id -> Environment -> Environment
removeVariable name = modifySymbols (Map.delete name)

-- | 'addTypeVar' @a@ : Add bound type variable @a@ to the environment
addTypeVar :: Id -> Environment -> Environment
addTypeVar a (Environment symbols args bvs) =
  Environment symbols args (a : bvs)

findSymbol
  :: Fresh s m => NameMapping -> Environment -> Id -> StateT s m SchemaSkeleton
findSymbol nameMap env sym = do
  let name  = fromMaybe sym (Map.lookup sym nameMap)
  let bound = getBoundTypeVars env
  case lookupSymbol name env of
    Nothing -> case lookupSymbol (textParens name) env of
      Nothing  -> return $ Monotype TopT
      Just sch -> freshSchema bound sch
    Just sch -> freshSchema bound sch


getFirstOrderArgs :: Environment -> [(Id, TypeSkeleton)]
getFirstOrderArgs = filter (not . isFunctionType . snd) . getArguments

getHigherOrderArgs :: Environment -> [(Id, TypeSkeleton)]
getHigherOrderArgs = filter (isFunctionType . snd) . getArguments