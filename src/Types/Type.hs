module Types.Type
  (
    -- * Types and constructors
    TypeSkeleton(..)
  , SchemaSkeleton(..)
  , nullDatatype
  , vart
  , listType
  , pairType
  , funcType
  , boolType
  , intType
  , charType
  , doubleType

    -- * Type queries
  , allArgTypes
  , allBaseTypes
  , allDatatypes
  , argsWithName
  , argType
  , arity
  , boundVarsOf
  , containsType
  , hasAny
  , hoArgsOf
  , isBot
  , isFunctionType
  , isHigherOrder
  , isPolymorphic
  , isNullDatatype
  , lastType
  , resType
  , typeDepth
  , typeSize
  , typeName
  , typeVarsOf

    -- * Type classes and constants
  , tyclassPrefix
  , tyclassInstancePrefix
  , tyclassArgBase
  , hoPostfix
  , wildcardPrefix
  , pairProj
  , isTyclass

    -- * Type operations
  , TypeSubstitution
  , UnifConstraint(..)
  , breakdown
  , longScalarName
  , permuteArgs
  , schemaSubstitute
  , toMonotype
  , typeSubstitute
  , withSchema

    -- * Abstract types
  , AbstractCover
  , SplitInfo(..)
  , SplitMsg
  , rootNode
  , toAbstractType
  , toAbstractFun
  , typesInCover
  , coverSize
  , absFunArgs
  , unionSplitInfo
  , unionsSplitInfo
  ) where

import           Control.Monad                  ( liftM2 )
import           Control.Monad.State.Lazy       ( State
                                                , evalState
                                                , get
                                                , put
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Hashable                  ( Hashable )
import           Data.List                      ( (\\) )
import           Data.List.Extra                ( nubOrd )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )

import           Test.QuickCheck                ( Arbitrary(..)
                                                , Gen
                                                , chooseInt
                                                , oneof
                                                , resize
                                                , sized
                                                )

import           Types.Common
import           Utility.Utils                  ( permuteBy )

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

instance Arbitrary TypeSkeleton where
  arbitrary = resize 3 $ sized arbitraryType
   where
    arbitraryType :: Int -> Gen TypeSkeleton
    arbitraryType 0 = oneof [return TopT, return BotT]
    arbitraryType n = oneof
      [ DatatypeT "List" <$> ((: []) <$> arbitraryType (n - 1))
      , DatatypeT "Maybe" <$> ((: []) <$> arbitraryType (n - 1))
      , DatatypeT "Fun" <$> liftM2 (\a b -> [a, b])
                                   (arbitraryType (n - 1))
                                   (arbitraryType (n - 1))
      , TypeVarT <$> oneof [return "a", return "b", return "c", return "d"]
      , FunctionT
      <$> oneof [return "x", return "y"]
      <*> arbitraryType (n - 1)
      <*> arbitraryType (n - 1)
      ]

  shrink (DatatypeT _ ts   ) = ts ++ concatMap shrink ts
  shrink (FunctionT _ t1 t2) = [t1, t2] ++ concatMap shrink [t1, t2]
  shrink _                   = []

boolType :: TypeSkeleton
boolType = DatatypeT "Bool" []

intType :: TypeSkeleton
intType = DatatypeT "Int" []

charType :: TypeSkeleton
charType = nullDatatype "Char"

doubleType :: TypeSkeleton
doubleType = nullDatatype "Double"

listType :: TypeSkeleton -> TypeSkeleton
listType t = DatatypeT "List" [t]

pairType :: TypeSkeleton -> TypeSkeleton -> TypeSkeleton
pairType t1 t2 = DatatypeT "Pair" [t1, t2]

funcType :: TypeSkeleton -> TypeSkeleton -> TypeSkeleton
funcType t1 t2 = DatatypeT "Fun" [t1, t2]

-- | Mapping from type variables to types
type TypeSubstitution = Map Id TypeSkeleton

data UnifConstraint = SubtypeOf TypeSkeleton TypeSkeleton
                    | UnifiesWith TypeSkeleton TypeSkeleton
  deriving (Eq, Ord, Show, Read, Generic)

--------------------------------------------------------------------------------
------------------------------   Abstract Types   ------------------------------
--------------------------------------------------------------------------------

-- | Refine an abstract type by separating another from it
type SplitMsg = (TypeSkeleton, TypeSkeleton)

data SplitInfo = SplitInfo
  { newPlaces    :: [TypeSkeleton] -- new types introduced by the split
  , removedTrans :: [Id]        -- transitions that are invalidated by the split
  , newTrans     :: [Id]            -- transitions that are introduced by the split
  }
  deriving (Eq, Ord, Show, Generic)

-- order matters here
unionSplitInfo :: SplitInfo -> SplitInfo -> SplitInfo
unionSplitInfo (SplitInfo places1 rmTrans1 newTrans1) (SplitInfo places2 rmTrans2 newTrans2)
  = SplitInfo (places1 ++ places2)
              (rmTrans1 ++ rmTrans2)
              ((newTrans1 ++ newTrans2) \\ (rmTrans1 ++ rmTrans2))

unionsSplitInfo :: [SplitInfo] -> SplitInfo
unionsSplitInfo = foldl1 unionSplitInfo

rootNode :: TypeSkeleton
rootNode = TopT

type AbstractCover = Map TypeSkeleton (Set TypeSkeleton)

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

wildcardPrefix :: Id
wildcardPrefix = ""

pairProj :: Id
pairProj = "pair_match"

isTyclass :: Id -> Bool
isTyclass f =
  tyclassArgBase `Text.isInfixOf` f || tyclassInstancePrefix `Text.isPrefixOf` f

--------------------------------------------------------------------------------
------------------------------   Type Operations  ------------------------------
--------------------------------------------------------------------------------

isBot :: TypeSkeleton -> Bool
isBot BotT = True
isBot _    = False

isFunctionType :: TypeSkeleton -> Bool
isFunctionType FunctionT{} = True
isFunctionType _           = False

isHigherOrder :: TypeSkeleton -> Bool
isHigherOrder (FunctionT _ tArg tRet) =
  isFunctionType tArg || isHigherOrder tRet
isHigherOrder _ = False

isPolymorphic :: SchemaSkeleton -> Bool
isPolymorphic (ForallT _ _) = True
isPolymorphic _             = False

isNullDatatype :: TypeSkeleton -> Bool
isNullDatatype (DatatypeT _ args) = null args
isNullDatatype _ = False

hasAny :: TypeSkeleton -> Bool
hasAny TopT                    = True
hasAny (DatatypeT _ tArgs    ) = any hasAny tArgs
hasAny (FunctionT _ tArg tRes) = hasAny tArg || hasAny tRes
hasAny _                       = False

typeName :: TypeSkeleton -> Id
typeName (DatatypeT name _) = name
typeName (TypeVarT name) = name
typeName t = error "scalarName error: cannot be applied to nonscalar type "

allDatatypes :: TypeSkeleton -> Set Id
allDatatypes (FunctionT _ tArg tRet) =
  allDatatypes tArg `Set.union` allDatatypes tRet
allDatatypes (DatatypeT id tArgs) =
  id `Set.insert` foldr (Set.union . allDatatypes) Set.empty tArgs
allDatatypes _ = Set.empty

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

allBaseTypes :: TypeSkeleton -> [TypeSkeleton]
allBaseTypes (FunctionT _ tArg tRet) = allBaseTypes tArg ++ allBaseTypes tRet
allBaseTypes t = [t]

toMonotype :: SchemaSkeleton -> TypeSkeleton
toMonotype (Monotype t ) = t
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
typeSubstitute subst t@(TypeVarT id) = Map.findWithDefault t id subst
typeSubstitute subst (DatatypeT name tArgs) =
  DatatypeT name (map (typeSubstitute subst) tArgs)
typeSubstitute subst (FunctionT x tArg tRes) =
  FunctionT x (typeSubstitute subst tArg) (typeSubstitute subst tRes)
typeSubstitute subst t = t

schemaSubstitute :: TypeSubstitution -> SchemaSkeleton -> SchemaSkeleton
schemaSubstitute tass (Monotype t) = Monotype $ typeSubstitute tass t
schemaSubstitute tass (ForallT a sch) =
  ForallT a $ schemaSubstitute (Map.delete a tass) sch

-- | 'typeVarsOf' @t@ : all type variables in @t@
typeVarsOf :: TypeSkeleton -> Set Id
typeVarsOf (TypeVarT name    ) = Set.singleton name
typeVarsOf (DatatypeT _ tArgs) = Set.unions (map typeVarsOf tArgs)
typeVarsOf (FunctionT _ tArg tRes) =
  typeVarsOf tArg `Set.union` typeVarsOf tRes
typeVarsOf _ = Set.empty

typeDepth :: TypeSkeleton -> Int
typeDepth (DatatypeT _ tys) | not (null tys) = 1 + maximum (map typeDepth tys)
typeDepth (FunctionT _ tArg tRet) = max (typeDepth tArg) (typeDepth tRet)
typeDepth _ = 0

typeSize :: TypeSkeleton -> Int
typeSize (DatatypeT _ tys) = 1 + sum (map typeSize tys)
typeSize (FunctionT _ tArg tRet) = 1 + typeSize tArg + typeSize tRet
typeSize _ = 1

longScalarName :: TypeSkeleton -> Id
longScalarName (DatatypeT name rs) =
  name `Text.append` Text.concat (map longScalarName rs)
longScalarName (TypeVarT name) = name
longScalarName t =
  error "longScalarName error: cannot be applied to nonscalar type "

breakdown :: TypeSkeleton -> [TypeSkeleton]
breakdown (FunctionT _ tArg tRes) = tArg : breakdown tRes
breakdown t                       = [t]

argsWithName :: TypeSkeleton -> [(Id, TypeSkeleton)]
argsWithName (FunctionT x tArg tRes) = (x, tArg) : argsWithName tRes
argsWithName _                       = []

permuteArgs :: [Int] -> SchemaSkeleton -> SchemaSkeleton
permuteArgs ords (ForallT x t) = ForallT x (permuteArgs ords t)
permuteArgs ords (Monotype t) =
  let args = argsWithName t
      ret  = lastType t
  in  Monotype $ foldr (uncurry FunctionT) ret (permuteBy ords args)

withSchema
  :: (TypeSkeleton -> TypeSkeleton) -> SchemaSkeleton -> SchemaSkeleton
withSchema f (ForallT x t) = ForallT x (withSchema f t)
withSchema f (Monotype t ) = Monotype (f t)

hoArgsOf :: TypeSkeleton -> [TypeSkeleton]
hoArgsOf (DatatypeT _ args) =
  filter isFunctionType args ++ concatMap hoArgsOf args
hoArgsOf (FunctionT _ tArg tRes) =
  (if isFunctionType tArg then [tArg] else hoArgsOf tArg) ++ hoArgsOf tRes
hoArgsOf _ = []

containsType :: TypeSkeleton -> [TypeSkeleton] -> [TypeSkeleton]
containsType t = filter (\tt -> tt == t || t `elem` hoArgsOf tt)

---------- Abstract type operations

--- notes:
--- abstractParamList is the same as allArgTypes
--- decompose is the same as allBaseTypes
--- decomposeHo is the same as breakdown
--- compactAbstractType is the same as toAbstractFun

toAbstractType :: TypeSkeleton -> TypeSkeleton
toAbstractType (FunctionT x tArg tRes) =
  FunctionT x (toAbstractFun tArg) (toAbstractType tRes)
toAbstractType (DatatypeT dt tArgs) = DatatypeT dt (map toAbstractFun tArgs)
toAbstractType t                    = t

toAbstractFun :: TypeSkeleton -> TypeSkeleton
toAbstractFun (FunctionT _ tArg tRes) =
  funcType (toAbstractFun tArg) (toAbstractFun tRes)
toAbstractFun t = toAbstractType t

typesInCover :: AbstractCover -> [TypeSkeleton]
typesInCover cover =
  nubOrd $ Map.keys cover ++ (Set.toList . Set.unions $ Map.elems cover)

coverSize :: AbstractCover -> Int
coverSize = length . typesInCover

absFunArgs :: Id -> TypeSkeleton -> [TypeSkeleton]
absFunArgs fname (FunctionT _ tArg tRes)
  | fname == pairProj = [tArg]
  | otherwise         = tArg : absFunArgs fname tRes
absFunArgs _ t = []