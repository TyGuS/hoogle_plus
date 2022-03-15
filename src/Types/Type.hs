module Types.Type where

import Control.Monad.State.Lazy ( State, evalState, get, put )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Hashable ( Hashable )
import Data.HashMap.Strict ( HashMap )
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Text ( Text )
import qualified Data.Text as Text
import GHC.Generics ( Generic )

import Types.Common
import Utility.Container ( permuteBy )

--------------------------------------------------------------------------------
------------------------------   Concrete Types   ------------------------------
--------------------------------------------------------------------------------

data SchemaSkeleton
  = Monotype TypeSkeleton 
  | ForallT Id SchemaSkeleton -- Type-polymorphic, each type variable may have some class constraints
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable SchemaSkeleton
instance FromJSON SchemaSkeleton
instance ToJSON SchemaSkeleton

{- Type skeletons -}
data TypeSkeleton
  = DatatypeT Id [TypeSkeleton] 
  | TypeVarT Id
  | FunctionT Id TypeSkeleton TypeSkeleton 
  | TopT 
  | BotT 
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable TypeSkeleton
instance FromJSON TypeSkeleton
instance ToJSON TypeSkeleton

-- | Mapping from type variables to types
type TypeSubstitution = Map Id TypeSkeleton

data UnifConstraint = UnifConstraint TypeSkeleton TypeSkeleton
  deriving (Eq, Ord, Show, Read, Generic)

--------------------------------------------------------------------------------
------------------------------   Abstract Types   ------------------------------
--------------------------------------------------------------------------------

-- | Refine an abstract type by separating another from it
type SplitMsg = (TypeSkeleton, TypeSkeleton)

data SplitInfo = SplitInfo { newPlaces :: [TypeSkeleton] -- new types introduced by the split
                           , removedTrans :: [Id]        -- transitions that are invalidated by the split
                           , newTrans :: [Id]            -- transitions that are introduced by the split
                           }
  deriving (Eq, Ord, Show, Generic)

rootNode :: TypeSkeleton
rootNode = TypeVarT varName

type AbstractCover = HashMap TypeSkeleton (Set TypeSkeleton)

--------------------------------------------------------------------------------
------------------------------   Type Operations  ------------------------------
--------------------------------------------------------------------------------

isFunctionType :: TypeSkeleton -> Bool
isFunctionType FunctionT {} = True
isFunctionType _            = False

isHigherOrder :: TypeSkeleton -> Bool
isHigherOrder (FunctionT _ tArg tRet) = isFunctionType tArg || isHigherOrder tRet
isHigherOrder _                       = False

hasAny :: TypeSkeleton -> Bool
hasAny TopT                    = True
hasAny (DatatypeT _ tArgs)     = any hasAny tArgs
hasAny (FunctionT _ tArg tRes) = hasAny tArg || hasAny tRes
hasAny _                       = False

typeName :: TypeSkeleton -> Id
typeName (DatatypeT name _) = name
typeName (TypeVarT name)    = name
typeName t                  = error "scalarName error: cannot be applied to nonscalar type "

allDatatypes :: TypeSkeleton -> Set Id
allDatatypes (FunctionT _ tArg tRet) = allDatatypes tArg `Set.union` allDatatypes tRet
allDatatypes (DatatypeT id tArgs)    = id `Set.insert` foldr (Set.union . allDatatypes) Set.empty tArgs
allDatatypes _                       = Set.empty

arity :: TypeSkeleton -> Int
arity (FunctionT _ _ t) = 1 + arity t
arity _                 = 0

argType :: TypeSkeleton -> TypeSkeleton
argType (FunctionT _ t _) = t
argType _                 = error "argType: not a function type"

resType :: TypeSkeleton -> TypeSkeleton
resType (FunctionT _ _ t) = t
resType _                 = error "resType: not a function type"

lastType :: TypeSkeleton -> TypeSkeleton
lastType (FunctionT _ _ tRes) = lastType tRes
lastType t                    = t

allArgTypes :: TypeSkeleton -> [TypeSkeleton]
allArgTypes (FunctionT x tArg tRes) = tArg : allArgTypes tRes
allArgTypes _                       = []

allArgs :: TypeSkeleton -> [TypeSkeleton]
allArgs (FunctionT _ tArg tRes) = tArg : allArgs tRes
allArgs _                       = []

allBaseTypes :: TypeSkeleton -> [TypeSkeleton]
allBaseTypes t@DatatypeT {}          = [t]
allBaseTypes t@TypeVarT {}           = [t]
allBaseTypes (FunctionT _ tArg tRet) = allBaseTypes tArg ++ allBaseTypes tRet
allBaseTypes _                       = error "allBaseTypes: applied to unsupported types"

toMonotype :: SchemaSkeleton -> TypeSkeleton
toMonotype (Monotype t)  = t
toMonotype (ForallT _ t) = toMonotype t

boundVarsOf :: SchemaSkeleton -> [Id]
boundVarsOf (ForallT a sch) = a : boundVarsOf sch
boundVarsOf _               = []

-- | Building types
nullDatatype :: Id -> TypeSkeleton
nullDatatype name = DatatypeT name []

vart :: Id -> TypeSkeleton
vart = TypeVarT

typeSubstitute :: TypeSubstitution -> TypeSkeleton -> TypeSkeleton
typeSubstitute subst t@(TypeVarT id)         = Map.findWithDefault t id subst
typeSubstitute subst (DatatypeT name tArgs)  = DatatypeT name (map (typeSubstitute subst) tArgs)
typeSubstitute subst (FunctionT x tArg tRes) = FunctionT x (typeSubstitute subst tArg) (typeSubstitute subst tRes)
typeSubstitute subst t                       = t

schemaSubstitute :: TypeSubstitution -> SchemaSkeleton -> SchemaSkeleton
schemaSubstitute tass (Monotype t) = Monotype $ typeSubstitute tass t
schemaSubstitute tass (ForallT a sch) = ForallT a $ schemaSubstitute (Map.delete a tass) sch

-- | 'typeVarsOf' @t@ : all type variables in @t@
typeVarsOf :: TypeSkeleton -> Set Id
typeVarsOf (TypeVarT name)         = Set.singleton name
typeVarsOf (DatatypeT _ tArgs)     = Set.unions (map typeVarsOf tArgs)
typeVarsOf (FunctionT _ tArg tRes) = typeVarsOf tArg `Set.union` typeVarsOf tRes
typeVarsOf _                       = Set.empty

typeDepth :: TypeSkeleton -> Int
typeDepth (DatatypeT _ tys) | not (null tys)  = 1 + maximum (map typeDepth tys)
typeDepth (FunctionT _ tArg tRet)             = max (typeDepth tArg) (typeDepth tRet)
typeDepth t                                   = 0

longScalarName :: TypeSkeleton -> Id
longScalarName (DatatypeT name rs) = name `Text.append` Text.concat (map longScalarName rs)
longScalarName (TypeVarT name)     = name
longScalarName t                   = error "longScalarName error: cannot be applied to nonscalar type "

breakdown :: TypeSkeleton -> [TypeSkeleton]
breakdown t@DatatypeT {}          = [t]
breakdown t@TypeVarT {}           = [t]
breakdown (FunctionT _ tArg tRes) = tArg : breakdown tRes
breakdown t                       = error "breakdown error: should not reach this case" 

argsWithName :: TypeSkeleton -> [(Id, TypeSkeleton)]
argsWithName (FunctionT x tArg tRes) = (x, tArg) : argsWithName tRes
argsWithName _                       = []

typeCmp :: TypeSkeleton -> TypeSkeleton -> Bool
typeCmp t1 t2 = canonicalize t1 == canonicalize t2

data CanonicalState = CanonicalState { varIndex :: Int
                                     , varMap   :: Map Id Id
                                     }

canonicalize :: TypeSkeleton -> TypeSkeleton
canonicalize t = evalState (canonicalize' t) (CanonicalState 0 Map.empty)

canonicalize' :: TypeSkeleton -> State CanonicalState TypeSkeleton
canonicalize' (TypeVarT var)          = do CanonicalState idx m <- get
                                           case Map.lookup var m of
                                             Just var' -> return $ TypeVarT var'
                                             Nothing   -> do let var' = Text.pack $ "t" ++ show idx
                                                             put $ CanonicalState (idx + 1) (Map.insert var var' m)
                                                             return $ TypeVarT var'
canonicalize' (DatatypeT name tArgs)  = DatatypeT name <$> mapM canonicalize' tArgs
canonicalize' (FunctionT x tArg tRes) = do tArg' <- canonicalize' tArg
                                           tRes' <- canonicalize' tRes
                                           return $ FunctionT x tArg' tRes'
canonicalize' t                       = return t

permuteArgs :: [Int] -> SchemaSkeleton -> SchemaSkeleton
permuteArgs ords (ForallT x t) = ForallT x (permuteArgs ords t)
permuteArgs ords (Monotype t)  = let args = argsWithName t
                                     ret  = lastType t
                                 in Monotype $ foldr (uncurry FunctionT) ret (permuteBy ords args)

withSchema :: (TypeSkeleton -> TypeSkeleton) -> SchemaSkeleton -> SchemaSkeleton
withSchema f (ForallT x t) = ForallT x (withSchema f t)
withSchema f (Monotype t)  = Monotype (f t)

hoArgsOf :: TypeSkeleton -> [TypeSkeleton]
hoArgsOf (DatatypeT _ args)      = filter isFunctionType args ++ concatMap hoArgsOf args
hoArgsOf (FunctionT _ tArg tRes) = (if isFunctionType tArg then [tArg] else hoArgsOf tArg) ++ hoArgsOf tRes
hoArgsOf _                       = []

containsType :: TypeSkeleton -> [TypeSkeleton] -> [TypeSkeleton]
containsType t = filter (\tt -> tt == t || t `elem` hoArgsOf tt)
