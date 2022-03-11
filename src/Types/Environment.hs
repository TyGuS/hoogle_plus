module Types.Environment where

import           Control.Lens
import           GHC.Generics ( Generic )
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map


import Types.Type
import Types.Common
import Types.Generate
import Types.IOFormat


-- | User-defined datatype representation
data DatatypeDef = DatatypeDef {
  _typeParams :: [Id],              -- ^ Type parameters
  _predVariances :: [Bool],         -- ^ For each predicate parameter, whether it is contravariant
  _constructors :: [Id]            -- ^ Constructor names
} deriving (Eq, Ord, Generic, Show)

makeLenses ''DatatypeDef


-- | Typing environment
data Environment = Environment {
  _symbols :: Map Id SchemaSkeleton,          -- ^ Variables and constants (with their refinement types), indexed by arity
  _arguments :: [(Id, SchemaSkeleton)],            -- ^ Function arguments, required in all the solutions
  _typeClasses :: Map Id (Set Id),         -- ^ Type class instances
  _boundTypeVars :: [Id],                  -- ^ Bound type variables
  -- | Constant part:
  _constants :: Set Id,                    -- ^ Subset of symbols that are constants
  _datatypes :: Map Id DatatypeDef,        -- ^ Datatype definitions
  _typeSynonyms :: Map Id ([Id], TypeSkeleton),   -- ^ Type synonym definitions
  _included_modules :: Set String,          -- ^ The set of modules any solution would need to import
  _typClassInstances :: [(String, String)],
  _condTypClasses :: [([(String, [Set String])], (String, String))],
  _hoCandidates :: [Id],
  _queryCandidates :: Map SchemaSkeleton [Example]
  } deriving(Show, Generic)

makeLenses ''Environment


instance Eq Environment where
  (==) e1 e2 = (e1 ^. symbols) == (e2 ^. symbols)

instance Ord Environment where
  (<=) e1 e2 = (e1 ^. symbols) <= (e2 ^. symbols)


-- | Empty environment
emptyEnv = Environment {
  _symbols = Map.empty,
  _arguments = [],
  _typeClasses = Map.empty,
  _boundTypeVars = [],
  _constants = Set.empty,
  _datatypes = Map.empty,
  _typeSynonyms = Map.empty,
  _included_modules = Set.empty,
  _typClassInstances = [],
  _condTypClasses = [],
  _hoCandidates = [],
  _queryCandidates = Map.empty 
}

--------------------------------------------------------------------------------
--------------------------  Environment Operations -----------------------------
--------------------------------------------------------------------------------

allSymbols :: Environment -> Map Id SchemaSkeleton
allSymbols env = env ^. symbols

-- | 'lookupSymbol' @name env@ : type of symbol @name@ in @env@, including built-in constants
lookupSymbol :: Id -> Int -> Environment -> Maybe SchemaSkeleton
lookupSymbol name a env = Map.lookup name (allSymbols env)

-- | 'isBound' @tv env@: is type variable @tv@ bound in @env@?
isBound :: Environment -> Id -> Bool
isBound env tv = tv `elem` env ^. boundTypeVars

addArgument :: Id -> TypeSkeleton -> Environment -> Environment
addArgument name t = arguments %~ ((name, Monotype t):)

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

removeVariable :: Id -> Environment -> Environment
removeVariable name env = case Map.lookup name (allSymbols env) of
  Nothing -> env
  Just sch -> over symbols (Map.delete name) . over constants (Set.delete name) $ env

addTypeSynonym :: Id -> [Id] -> TypeSkeleton -> Environment -> Environment
addTypeSynonym name tvs t = over typeSynonyms (Map.insert name (tvs, t))

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

typeSubstituteEnv :: TypeSubstitution -> Environment -> Environment
typeSubstituteEnv tass = over symbols (Map.map (schemaSubstitute tass))