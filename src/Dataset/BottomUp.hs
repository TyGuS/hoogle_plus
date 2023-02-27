module Dataset.BottomUp where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Maybe (mapMaybe)

import Data.ECTA
import Data.Tuple.Extra

import Database.Dataset
import Types.Type
import Types.Program
import Types.Fresh
import Types.TypeChecker
import Types.Common

type Size = Int
type ProgramBank = Map TypeSkeleton [TProgram]
type Generator = State (Map Id Int)

data Function = Named Id | Unnamed TProgram

generateApp :: ProgramBank -> Generator ProgramBank
generateApp bank =
  let functions = filter (isFunctionType . toMonotype . snd) hplusComponents
      functionsInLib = map (\(f, t) -> (Named f, toMonotype t)) functions
      unnamedFunctions = filter (isFunctionType . fst) (Map.toList bank)
      functionsInBank = concatMap (\(t, fs) -> map (\f -> (Unnamed f, t)) fs) unnamedFunctions
   in Map.unionsWith (++) <$> mapM (uncurry go) (functionsInLib ++ functionsInBank)
  where
    -- generate application terms for a given function
    go :: Function -> TypeSkeleton -> Generator ProgramBank
    go f typ = do
      -- refresh type variables
      typ' <- freshType [] typ
      let FunctionT _ tArg tRet = typ'
      let args = unifiedTerms tArg
      let typedTerms = map (uncurry $ applyTerm f typ') args
      return (Map.fromList typedTerms)

    applyTerm :: Function -> TypeSkeleton -> TypeSubstitution -> [TProgram] -> (TypeSkeleton, [TProgram])
    applyTerm f t@(FunctionT _ _ tRet) subst args =
      let tRet' = typeSubstitute subst tRet
          (fname, pArgs) = case f of Named fname -> (fname, [])
                                     Unnamed (Program (PApp f args) _) -> (f, args)
          terms = map (\arg -> Program (PApp fname (pArgs ++ [arg])) tRet') args
       in (tRet', terms)

    addProgram :: TypeSkeleton -> TProgram -> ProgramBank -> ProgramBank
    addProgram typ p = Map.insertWith (++) typ [p]

    isUnifiedWith :: TypeSkeleton -> TypeSkeleton -> [TProgram] -> Maybe (TypeSubstitution, [TProgram])
    isUnifiedWith target k v =
      case getUnifier [] [UnifiesWith target k] of
        Nothing -> Nothing
        Just subst -> let v' = map (withContent $ typeSubstitute subst) v in Just (subst, v')

    unifiedTerms :: TypeSkeleton -> [(TypeSubstitution, [TProgram])]
    unifiedTerms typ = mapMaybe (uncurry (isUnifiedWith typ)) (Map.toList bank)

generate :: Int -> [TProgram]
generate n =
    let unary = filter (not . isFunctionType . toMonotype . snd) hplusComponents
        literals = map (\lit -> (typeOf lit, lit)) (strLits ++ intLits ++ charLits)
        inits = map (uncurry toProgram) unary ++ literals
        initBank = foldr (\(t, p) -> Map.insertWith (++) t [p]) Map.empty inits
        bank = evalState (go n initBank) Map.empty
     in concat (Map.elems bank)
  where
    go :: Int -> ProgramBank -> Generator ProgramBank
    go n bank | n <= 0 = return bank
              | otherwise = (Map.union bank <$> generateApp bank) >>= go (n-1)

    toProgram :: Text -> SchemaSkeleton -> (TypeSkeleton, TProgram)
    toProgram x t = (toMonotype t, Program (PSymbol x) (toMonotype t))

assignArgs :: TProgram -> Generator [TProgram]
assignArgs program = map mkLambda <$> go program
  where
    go :: TProgram -> Generator [(TProgram, [Id])]
    go (Program p t) = case p of
      PSymbol _ -> do
        arg <- freshId [] "arg"
        return [(program, [])
              , (Program (PSymbol arg) t, [arg])]

    mkLambda :: (TProgram, [Id]) -> TProgram
    mkLambda (p, args) = foldr (\arg body -> untyped (PFun arg body)) p args

--------------------------------------------------------------------------------
----------                     Literal Expressions                    ----------
--------------------------------------------------------------------------------
strLits :: [TProgram]
strLits = map ((`ann` listType charType) . varp) ["str"] -- ["a", "A", "Aa", "bB"]

intLits :: [TProgram]
intLits = map ((`ann` intType) . varp) ["i"] -- [-1, 0, 1]

charLits :: [TProgram]
charLits = map ((`ann` charType) . varp) ["c"] -- ['a', ' ', 'B', '+']