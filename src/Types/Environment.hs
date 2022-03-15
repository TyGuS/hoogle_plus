module Types.Environment where

import           GHC.Generics ( Generic )
import           Data.Function ( on )
import           Data.Map ( Map )
import qualified Data.Map as Map

import Types.Type
import Types.Common


-- | User-defined datatype representation
type Parameter = Id
type ConstructorName = Id
data DatatypeDef = DatatypeDef [Parameter] [ConstructorName]
  deriving ( Eq, Ord, Generic, Show )

getParameters :: DatatypeDef -> [Parameter]
getParameters (DatatypeDef ps _) = ps

getConstructors :: DatatypeDef -> [ConstructorName]
getConstructors (DatatypeDef _ cs) = cs

data TypeSynonym = TypeSynonym Id [Id] TypeSkeleton
  deriving ( Eq, Ord, Generic, Show )

type SymbolLib = Map Id SchemaSkeleton

data Environment = Environment { getSymbols :: SymbolLib                   -- ^ Components with their types
                               , getArguments :: [(Id, SchemaSkeleton)]    -- ^ Function arguments, required in all the solutions
                               , getBoundTypeVars :: [Id]                  -- ^ Bound type variables
                               }
  deriving ( Show, Generic )

instance Eq Environment where
  (==) e1 e2 = getSymbols e1 == getSymbols e2

instance Ord Environment where
  compare = compare `on` getSymbols

emptyEnv :: Environment
emptyEnv = Environment Map.empty [] []

--------------------------------------------------------------------------------
--------------------------  Environment Operations -----------------------------
--------------------------------------------------------------------------------

allSymbols :: Environment -> SymbolLib
allSymbols = getSymbols

-- | 'lookupSymbol' @name env@ : type of symbol @name@ in @env@, including built-in constants
lookupSymbol :: Id -> Int -> Environment -> Maybe SchemaSkeleton
lookupSymbol name a env = Map.lookup name (allSymbols env)

-- | 'isBound' @tv env@: is type variable @tv@ bound in @env@?
isBound :: Environment -> Id -> Bool
isBound env tv = tv `elem` getBoundTypeVars env

addArgument :: Id -> TypeSkeleton -> Environment -> Environment
addArgument name t (Environment symbols args bvs) = Environment symbols (args ++ [(name, Monotype t)]) bvs

modifySymbols :: (SymbolLib -> SymbolLib) -> Environment -> Environment
modifySymbols f (Environment symbols args bvs) = Environment (f symbols) args bvs

addComponent :: Id -> SchemaSkeleton -> Environment -> Environment
addComponent name sch (Environment symbols args bvs) =
  case name `Map.lookup` symbols of
    Nothing -> Environment (Map.insert name sch symbols) args bvs
    Just _  -> error $ "addComponent: symbol " ++ show name ++ " already defined"

removeVariable :: Id -> Environment -> Environment
removeVariable name = modifySymbols (Map.delete name)

-- | 'addTypeVar' @a@ : Add bound type variable @a@ to the environment
addTypeVar :: Id -> Environment -> Environment
addTypeVar a (Environment symbols args bvs) = Environment symbols args (a : bvs)