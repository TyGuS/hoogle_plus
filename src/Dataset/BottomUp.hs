module Dataset.BottomUp where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.List

import Control.Monad.Extra
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
      args <- unifiedTerms bank tArg
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

    unifiedTerms :: ProgramBank -> TypeSkeleton -> Generator [(TypeSubstitution, [TProgram])]
    unifiedTerms localBank t@(FunctionT _ tArg tRet) = do
      x <- freshId [] "x"
      terms <- unifiedTerms (Map.insertWith (++) tArg [varp x] localBank) tRet
      let mkFun tass es = (tass, map (\e -> Program (PFun x e) (typeSubstitute tass t)) es)
      return $ map (uncurry mkFun) terms
    unifiedTerms localBank typ =
      return $ mapMaybe (uncurry (isUnifiedWith typ)) (Map.toList localBank)

generate :: Int -> [TProgram]
generate n =
    let literals = map (\lit -> (typeOf lit, lit)) (strLits ++ intLits ++ charLits)
        inits = map (uncurry toProgram) hplusComponents ++ literals
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

      PApp f args -> do
        argsList <- sequence <$> mapM go args
        let argsList' = map (\as -> (map fst as, concatMap snd as)) argsList
        arg <- freshId [] "arg"
        ([(program, []), (varp arg, [arg])] ++) <$> concatMapM (mkApp t) argsList'

      PFun x body -> do
        bodys <- go body -- note that x may become an argument, should we prevent that?
        arg <- freshId [] "arg"
        return ([(program, [])
                ,(Program (PSymbol arg) t, [arg])
                ] ++ map (\(e, xs) -> (Program (PFun x e) t, xs)) bodys)

      _ -> error "unsupported expression in assignArgs"

    mkApp :: TypeSkeleton -> ([TProgram], [Id]) -> Generator [(TProgram, [Id])]
    mkApp tRet (args, binds) = do
      arg <- freshId [] "arg"
      return $ map (\as -> (Program (PApp arg as) tRet, arg:binds)) (tails args)

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