module Dataset.BottomUp where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.List

import Control.Monad.Extra
import Data.ECTA
import Data.Tuple.Extra
import Pipes
import qualified Pipes.Prelude as P

import Database.Dataset
import Types.Type
import Types.Program
import Types.Fresh
import Types.TypeChecker
import Types.Common
import Dataset.Configuration

import Debug.Trace

type Size = Int
type ProgramBank = Map TypeSkeleton [TProgram]
type Generator m = ListT (StateT (Map Id Int) m)

generateApp :: MonadIO m => ProgramBank -> Generator m (TypeSkeleton, TProgram)
generateApp bank = do
  (t, fs) <- Select $ each $ filter (isFunctionType . fst) (Map.toList bank)
  f <- Select (each fs)
  go t f
  where
    -- generate application terms for a given function
    go :: MonadIO m => TypeSkeleton -> TProgram -> Generator m (TypeSkeleton, TProgram)
    go typ f = do
      -- refresh type variables
      typ' <- lift $ freshType [] typ
      let FunctionT _ tArg tRet = typ'
      (subst, args) <- unifiedTerms bank tArg
      arg <- Select (each args)
      applyTerm f typ' subst arg

    applyTerm :: MonadIO m => TProgram -> TypeSkeleton -> TypeSubstitution -> TProgram -> Generator m (TypeSkeleton, TProgram)
    applyTerm (Program f _) t@(FunctionT _ _ tRet) subst arg = do
      let tRet' = typeSubstitute subst tRet
      let (fname, pArgs) = case f of PSymbol fname -> (fname, [])
                                     PApp f args -> (f, args)
      return (tRet', Program (PApp fname (pArgs ++ [arg])) tRet')

    isUnifiedWith :: TypeSkeleton -> TypeSkeleton -> [TProgram] -> Maybe (TypeSubstitution, [TProgram])
    isUnifiedWith target k v =
      case getUnifier [] [UnifiesWith target k] of
        Nothing -> Nothing
        Just subst -> let v' = map (withContent $ typeSubstitute subst) v in Just (subst, v')

    unifiedTerms :: MonadIO m => ProgramBank -> TypeSkeleton -> Generator m (TypeSubstitution, [TProgram])
    unifiedTerms localBank t = case t of
      FunctionT _ tArg tRet -> do
        x <- lift $ freshId [] "x"
        (tass, es) <- unifiedTerms (Map.insertWith (++) tArg [varp x] localBank) tRet
        return (tass, map (\e -> Program (PFun x e) (typeSubstitute tass t)) es)
      _ -> do
        (typ, terms) <- Select $ each (Map.toList localBank)
        case isUnifiedWith t typ terms of
          Nothing -> mempty
          Just v -> return v

generate :: MonadIO m => [(Text, SchemaSkeleton)] -> Int -> m [TProgram]
generate components n = do
    let literals = map (\lit -> (typeOf lit, lit)) (intLits ++ charLits)
    let inits = map (uncurry toProgram) components ++ literals
    let initBank = foldr (\(t, p) -> Map.insertWith (++) t [p]) Map.empty inits
    bank <- evalStateT (go n initBank) Map.empty
    return $ Set.toList $ Set.fromList $ concat (Map.elems bank)
  where
    go :: MonadIO m => Int -> ProgramBank -> StateT (Map Id Int) m ProgramBank
    go n bank | n <= 0 = return bank
              | otherwise = do results <- P.toListM (every (generateApp bank))
                               let bank' = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) results
                               bank' <- return $ Map.unionWith (++) bank bank'
                               go (n-1) (Map.map (Set.toList . Set.fromList) bank')

    toProgram :: Text -> SchemaSkeleton -> (TypeSkeleton, TProgram)
    toProgram x t = (toMonotype t, Program (PSymbol x) (toMonotype t))

assignArgs :: MonadIO m => Configuration -> TProgram -> Generator m TProgram
assignArgs config program = mkLambda <$> go program
  where
    go :: MonadIO m => TProgram -> Generator m (TProgram, [Id])
    go prog@(Program p t) = case p of
      PSymbol _ -> do
        arg <- lift $ freshId [] "arg"
        Select (each [ (prog, [])
                     , (Program (PSymbol arg) t, [arg])
                     ])

      PApp f args -> (do
          arg <- lift $ freshId [] "arg"
          return (Program (PSymbol arg) t, [arg])
        ) `mplus` (do
          argsList <- sequence $ map go args
          -- trace ("argsList: " ++ show argsList ++ " for " ++ show f) $ return ()
          let (args', binds) = (map fst argsList, concatMap snd argsList)
          let withFname = (Program (PApp f args') t, binds)
          return withFname `mplus` mkApp t args' binds
        )

      PFun x body -> do
        (e, xs) <- go body -- note that x may become an argument, should we prevent that?
        arg <- lift $ freshId [] "arg"
        Select (each [(prog, []), (Program (PSymbol arg) t, [arg])])
          `mplus` return (Program (PFun x e) t, xs)

      _ -> error "unsupported expression in assignArgs"

    mkApp :: MonadIO m => TypeSkeleton -> [TProgram] -> [Id] -> Generator m (TProgram, [Id])
    mkApp tRet args binds
      | 1 + length binds > maxArgs config = mempty
      | otherwise = do
        arg <- lift $ freshId [] "arg"
        args' <- Select $ each $ init $ tails args
        return (Program (PApp arg args') tRet, arg:binds)

    mkLambda :: (TProgram, [Id]) -> TProgram
    mkLambda (p, args) = let args' = filter (`Set.member` (symbolsOf p)) args
                          in foldr (\arg body -> untyped (PFun arg body)) p args'

--------------------------------------------------------------------------------
----------                     Literal Expressions                    ----------
--------------------------------------------------------------------------------
-- strLits :: [TProgram]
-- strLits = map ((`ann` listType charType) . varp) ["str"] -- ["a", "A", "Aa", "bB"]

intLits :: [TProgram]
intLits = map ((`ann` intType) . varp) ["i"] -- [-1, 0, 1]

charLits :: [TProgram]
charLits = map ((`ann` charType) . varp) ["c"] -- ['a', ' ', 'B', '+']