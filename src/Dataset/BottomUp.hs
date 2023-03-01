module Dataset.BottomUp where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
import Dataset.Configuration

import Debug.Trace

type Size = Int
type ProgramBank = Map TypeSkeleton [TProgram]
type Generator = State (Map Id Int)

generateApp :: ProgramBank -> Generator ProgramBank
generateApp bank =
  let functions = filter (isFunctionType . fst) (Map.toList bank)
   in Map.unionsWith (++) <$> mapM (uncurry go) functions
  where
    -- generate application terms for a given function
    go :: TypeSkeleton -> [TProgram] -> Generator ProgramBank
    go typ fs = do
      -- refresh type variables
      typ' <- freshType [] typ
      let FunctionT _ tArg tRet = typ'
      args <- unifiedTerms bank tArg
      -- trace ("get args " ++ show args ++ " for " ++ show fs) $ return ()
      let applyArgs f = map (uncurry $ applyTerm f typ') args
      let typedTerms = concatMap applyArgs fs
      -- trace ("get typed terms " ++ show typedTerms) $ return ()
      return (Map.fromListWith (++) typedTerms)

    applyTerm :: TProgram -> TypeSkeleton -> TypeSubstitution -> [TProgram] -> (TypeSkeleton, [TProgram])
    applyTerm (Program f _) t@(FunctionT _ _ tRet) subst args =
      let tRet' = typeSubstitute subst tRet
          (fname, pArgs) = case f of PSymbol fname -> (fname, [])
                                     PApp f args -> (f, args)
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

generate :: [(Text, SchemaSkeleton)] -> Int -> [TProgram]
generate components n =
    let literals = map (\lit -> (typeOf lit, lit)) (intLits ++ charLits)
        inits = map (uncurry toProgram) components ++ literals
        initBank = foldr (\(t, p) -> Map.insertWith (++) t [p]) Map.empty inits
        bank = evalState (go n initBank) Map.empty
     in concat (Map.elems bank)
  where
    go :: Int -> ProgramBank -> Generator ProgramBank
    go n bank | n <= 0 = return bank
              | otherwise = do bank' <- generateApp bank
                              --  traceShow bank $ return ()
                               bank' <- return $ Map.unionWith (++) bank bank'
                              --  traceShow bank' $ return ()
                               go (n-1) (Map.map (Set.toList . Set.fromList) bank')

    toProgram :: Text -> SchemaSkeleton -> (TypeSkeleton, TProgram)
    toProgram x t = (toMonotype t, Program (PSymbol x) (toMonotype t))

assignArgs :: Configuration -> TProgram -> Generator [TProgram]
assignArgs config program = map mkLambda <$> go program
  where
    go :: TProgram -> Generator [(TProgram, [Id])]
    go prog@(Program p t) = case p of
      PSymbol _ -> do
        arg <- freshId [] "arg"
        return [(prog, [])
              , (Program (PSymbol arg) t, [arg])]

      PApp f args -> do
        argsList <- sequence <$> mapM go args
        -- traceShow (argsList, p) $ return ()
        let argsList' = map (\as -> (map fst as, concatMap snd as)) argsList
        let withFname = map (\(as, binds) -> (Program (PApp f as) t, binds)) argsList'
        withoutFname <- concatMapM (mkApp t) argsList'
        arg <- freshId [] "arg"
        return ([ (prog, [])
                , (Program (PSymbol arg) t, [arg])
                ] ++ withFname ++ withoutFname)

      PFun x body -> do
        bodys <- go body -- note that x may become an argument, should we prevent that?
        arg <- freshId [] "arg"
        return ([(prog, [])
                ,(Program (PSymbol arg) t, [arg])
                ] ++ map (\(e, xs) -> (Program (PFun x e) t, xs)) bodys)

      _ -> error "unsupported expression in assignArgs"

    mkApp :: TypeSkeleton -> ([TProgram], [Id]) -> Generator [(TProgram, [Id])]
    mkApp tRet (args, binds)
      | length binds > maxArgs config = return []
      | otherwise = do
        arg <- freshId [] "arg"
        return $ map (\as -> (Program (PApp arg as) tRet, arg:binds)) (init $ tails args)

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