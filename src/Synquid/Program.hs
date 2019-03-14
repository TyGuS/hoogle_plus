{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}

-- | Executable programs
module Synquid.Program where

import Synquid.Logic
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
import qualified Data.Map as Map
import Data.Map (Map)
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

eraseTypes :: RProgram -> UProgram
eraseTypes = fmap (const AnyT)

symbolName (Program (PSymbol name) _) = name
symbolList (Program (PSymbol name) _) = [name]
symbolList (Program (PApp fun arg) _) = symbolList fun ++ symbolList arg

symbolsOf (Program p _) = case p of
  PSymbol name -> Set.singleton name
  PApp fun arg -> symbolsOf fun `Set.union` symbolsOf arg
  PFun x body -> symbolsOf body
  PIf cond thn els -> symbolsOf cond `Set.union` symbolsOf thn `Set.union` symbolsOf els
  PMatch scr cases -> symbolsOf scr `Set.union` Set.unions (map (symbolsOf . expr) cases)
  PFix x body -> symbolsOf body
  PLet x def body -> symbolsOf def `Set.union` symbolsOf body
  _ -> Set.empty

hasHole (Program p _) = case p of
  PApp fun arg -> hasHole fun || hasHole arg
  PHole -> True
  _ -> False

errorProgram = Program PErr (vart dontCare ftrue)
isError (Program PErr _) = True
isError _ = False

-- | Substitute a symbol for a subterm in a program
programSubstituteSymbol :: Id -> RProgram -> RProgram -> RProgram
programSubstituteSymbol name subterm (Program p t) = Program (programSubstituteSymbol' p) t
  where
    pss = programSubstituteSymbol name subterm

    programSubstituteSymbol' (PSymbol x) = if x == name then content subterm else p
    programSubstituteSymbol' (PApp fun arg) = PApp (pss fun) (pss arg)
    programSubstituteSymbol' (PFun name pBody) = PFun name (pss pBody)
    programSubstituteSymbol' (PIf c p1 p2) = PIf (pss c) (pss p1) (pss p2)
    programSubstituteSymbol' (PMatch scr cases) = PMatch (pss scr) (map (\(Case ctr args pBody) -> Case ctr args (pss pBody)) cases)
    programSubstituteSymbol' (PFix args pBody) = PFix args (pss pBody)
    programSubstituteSymbol' (PLet x pDef pBody) = PLet x (pss pDef) (pss pBody)

-- | Convert an executable formula into a program
fmlToProgram :: Formula -> RProgram
fmlToProgram (BoolLit b) = Program (PSymbol $ show b) (ScalarT BoolT $ valBool |=| BoolLit b)
fmlToProgram (IntLit i) = Program (PSymbol $ show i) (ScalarT IntT $ valBool |=| IntLit i)
fmlToProgram (Var s x) = Program (PSymbol x) (addRefinement (fromSort s) (varRefinement x s))
fmlToProgram fml@(Unary op e) = let
    s = sortOf fml
    p = fmlToProgram e
    fun = Program (PSymbol $ unOpTokens Map.! op) (FunctionT "x" (typeOf p) opRes)
  in Program (PApp fun p) (addRefinement (fromSort s) (Var s valueVarName |=| fml))
  where
    opRes
      | op == Not = bool $ valBool |=| fnot (intVar "x")
      | otherwise = int $ valInt |=| Unary op (intVar "x")
fmlToProgram fml@(Binary op e1 e2) = let
    s = sortOf fml
    p1 = fmlToProgram e1
    p2 = fmlToProgram e2
    fun1 = Program (PSymbol $ binOpTokens Map.! op) (FunctionT "x" (typeOf p1) (FunctionT "y" (typeOf p2) opRes))
    fun2 = Program (PApp fun1 p1) (FunctionT "y" (typeOf p2) opRes)
  in Program (PApp fun2 p2) (addRefinement (fromSort s) (Var s valueVarName |=| fml))
  where
    opRes
      | op == Times || op == Times || op == Times = int $ valInt |=| Binary op (intVar "x") (intVar "y")
      | otherwise                                 = bool $ valBool |=| Binary op (intVar "x") (intVar "y")

-- | 'renameAsImpl' @p t@: change argument names in function type @t@ to be the same as in the abstraction @p@
renameAsImpl :: (Id -> Bool) -> UProgram -> RType -> RType
renameAsImpl isBound = renameAsImpl' Map.empty
  where
    renameAsImpl' subst (Program (PFun y pRes) _) (FunctionT x tArg tRes) = case tArg of
      ScalarT baseT fml -> FunctionT y (substituteInType isBound subst tArg) (renameAsImpl' (Map.insert x (Var (toSort baseT) y) subst) pRes tRes)
      _ -> FunctionT y (substituteInType isBound subst tArg) (renameAsImpl' subst pRes tRes)
    renameAsImpl' subst  _ t = substituteInType isBound subst t

{- Top-level definitions -}




mtComp :: Metadata -> Metadata -> Bool
mtComp mt1 mt2 =
  if mt1 ^. distFromGoal < mt2 ^. distFromGoal
    then True
    else if mt1 ^. distFromGoal == mt2 ^. distFromGoal
      then if mt1 ^. mWeight > mt2 ^. mWeight
        then True
        else False
      else False

instance Ord Metadata where
  (<=) mt1 mt2 = mt1 ^. distFromGoal <= mt2 ^. distFromGoal && mt1 ^. mWeight <= mt2 ^. mWeight


instance (Eq k, Hashable k, Serialize k, Serialize v) => Serialize (HashMap k v) where
  put hm = S.put (HashMap.toList hm)
  get = HashMap.fromList <$> S.getListOf S.get




-- | 'symbolsOfArity' @n env@: all symbols of arity @n@ in @env@
symbolsOfArity n env = Map.findWithDefault Map.empty n (env ^. symbols)

-- | All symbols in an environment
allSymbols :: Environment -> Map Id RSchema
allSymbols env = Map.unions $ Map.elems (env ^. symbols)

-- | 'lookupSymbol' @name env@ : type of symbol @name@ in @env@, including built-in constants
lookupSymbol :: Id -> Int -> Environment -> Maybe RSchema
lookupSymbol name a env
  | a == 0 && name == "True"                          = Just $ Monotype $ ScalarT BoolT valBool
  | a == 0 && name == "False"                         = Just $ Monotype $ ScalarT BoolT (fnot valBool)
  | a == 0 && isJust asInt                            = Just $ Monotype $ ScalarT IntT (valInt |=| IntLit (fromJust asInt))
  | a == 1 && (name `elem` Map.elems unOpTokens)      = let op = head $ Map.keys $ Map.filter (== name) unOpTokens in Just $ unOpType op
  | a == 2 && (name `elem` Map.elems binOpTokens)     = let op = head $ Map.keys $ Map.filter (== name) binOpTokens in Just $ binOpType op
  | otherwise                                         = Map.lookup name (allSymbols env)
  where
    asInt = asInteger name

symbolAsFormula :: Environment -> Id -> RType -> Formula
symbolAsFormula _ name t | arity t > 0
                      = error $ unwords ["symbolAsFormula: not a scalar symbol", name]
symbolAsFormula env name t
  | name == "True"    = BoolLit True
  | name == "False"   = BoolLit False
  | isJust asInt      = IntLit (fromJust asInt)
  | isConstructor     = Cons sort name []
  | otherwise         = Var sort name
  where
    isConstructor = isJust (lookupConstructor name env)
    sort = toSort (baseTypeOf t)
    asInt = asInteger name

unOpType Neg       = Monotype $ FunctionT "x" intAll (int (valInt |=| fneg (intVar "x")))
unOpType Not       = Monotype $ FunctionT "x" boolAll (bool (valBool |=| fnot (boolVar "x")))
binOpType Times    = Monotype $ FunctionT "x" intAll (FunctionT "y" intAll (int (valInt |=| intVar "x" |*| intVar "y")))
binOpType Plus     = Monotype $ FunctionT "x" intAll (FunctionT "y" intAll (int (valInt |=| intVar "x" |+| intVar "y")))
binOpType Minus    = Monotype $ FunctionT "x" intAll (FunctionT "y" intAll (int (valInt |=| intVar "x" |-| intVar "y")))
binOpType Eq       = ForallT "a" $ Monotype $ FunctionT "x" (vartAll "a") (FunctionT "y" (vartAll "a") (bool (valBool |=| (vartVar "a" "x" |=| vartVar "a" "y"))))
binOpType Neq      = ForallT "a" $ Monotype $ FunctionT "x" (vartAll "a") (FunctionT "y" (vartAll "a") (bool (valBool |=| (vartVar "a" "x" |/=| vartVar "a" "y"))))
binOpType Lt       = ForallT "a" $ Monotype $ FunctionT "x" (vartAll "a") (FunctionT "y" (vartAll "a") (bool (valBool |=| (vartVar "a" "x" |<| vartVar "a" "y"))))
binOpType Le       = ForallT "a" $ Monotype $ FunctionT "x" (vartAll "a") (FunctionT "y" (vartAll "a") (bool (valBool |=| (vartVar "a" "x" |<=| vartVar "a" "y"))))
binOpType Gt       = ForallT "a" $ Monotype $ FunctionT "x" (vartAll "a") (FunctionT "y" (vartAll "a") (bool (valBool |=| (vartVar "a" "x" |>| vartVar "a" "y"))))
binOpType Ge       = ForallT "a" $ Monotype $ FunctionT "x" (vartAll "a") (FunctionT "y" (vartAll "a") (bool (valBool |=| (vartVar "a" "x" |>=| vartVar "a" "y"))))
binOpType And      = Monotype $ FunctionT "x" boolAll (FunctionT "y" boolAll (bool (valBool |=| (boolVar "x" |&| boolVar "y"))))
binOpType Or       = Monotype $ FunctionT "x" boolAll (FunctionT "y" boolAll (bool (valBool |=| (boolVar "x" ||| boolVar "y"))))
binOpType Implies  = Monotype $ FunctionT "x" boolAll (FunctionT "y" boolAll (bool (valBool |=| (boolVar "x" |=>| boolVar "y"))))
binOpType Iff      = Monotype $ FunctionT "x" boolAll (FunctionT "y" boolAll (bool (valBool |=| (boolVar "x" |<=>| boolVar "y"))))

-- | Is @name@ a constant in @env@ including built-in constants)?
isConstant name env = (name `elem` ["True", "False"]) ||
                      isJust (asInteger name) ||
                      (name `elem` Map.elems unOpTokens) ||
                      (name `elem` Map.elems binOpTokens) ||
                      (name `Set.member` (env ^. constants))

-- | 'isBound' @tv env@: is type variable @tv@ bound in @env@?
isBound :: Environment -> Id -> Bool
isBound env tv = tv `elem` env ^. boundTypeVars

addArgument :: Id -> RType -> Environment -> Environment
addArgument name t = (arguments %~ Map.insert name (Monotype t))

addVariable :: Id -> RType -> Environment -> Environment
addVariable name t = addPolyVariable name (Monotype t)

addPolyVariable :: Id -> RSchema -> Environment -> Environment
addPolyVariable name sch = let n = arity (toMonotype sch) in (symbols %~ Map.insertWith Map.union n (Map.singleton name sch))

-- | 'addConstant' @name t env@ : add type binding @name@ :: Monotype @t@ to @env@
addConstant :: Id -> RType -> Environment -> Environment
addConstant name t = addPolyConstant name (Monotype t)

-- | 'addPolyConstant' @name sch env@ : add type binding @name@ :: @sch@ to @env@
addPolyConstant :: Id -> RSchema -> Environment -> Environment
addPolyConstant name sch = addPolyVariable name sch . (constants %~ Set.insert name)

addUnresolvedConstant :: Id -> RSchema -> Environment -> Environment
addUnresolvedConstant name sch = unresolvedConstants %~ Map.insert name sch

removeVariable :: Id -> Environment -> Environment
removeVariable name env = case Map.lookup name (allSymbols env) of
  Nothing -> env
  Just sch -> over symbols (Map.insertWith (flip Map.difference) (arity $ toMonotype sch) (Map.singleton name sch)) . over constants (Set.delete name) $ env


unfoldAllVariables env = over unfoldedVars (Set.union (Map.keysSet (symbolsOfArity 0 env) Set.\\ (env ^. constants))) env

addTypeSynonym :: Id -> [Id] -> RType -> Environment -> Environment
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
typeSubstituteEnv tass = over symbols (Map.map (Map.map (schemaSubstitute tass)))

-- | Insert weakest refinement
refineTop :: Environment -> SType -> RType
refineTop env (ScalarT (DatatypeT name tArgs pArgs) _) =
  let variances = env ^. (datatypes . to (Map.! name) . predVariances) in
  ScalarT (DatatypeT name (map (refineTop env) tArgs) (map (BoolLit . not) variances)) ftrue
refineTop _ (ScalarT IntT _) = ScalarT IntT ftrue
refineTop _ (ScalarT BoolT _) = ScalarT BoolT ftrue
refineTop _ (ScalarT (TypeVarT vSubst a) _) = ScalarT (TypeVarT vSubst a) ftrue
refineTop env (FunctionT x tArg tFun) = FunctionT x (refineBot env tArg) (refineTop env tFun)

-- | Insert strongest refinement
refineBot :: Environment -> SType -> RType
refineBot env (ScalarT (DatatypeT name tArgs pArgs) _) =
  let variances = env ^. (datatypes . to (Map.! name) . predVariances) in
  ScalarT (DatatypeT name (map (refineBot env) tArgs) (map BoolLit variances)) ffalse
refineBot _ (ScalarT IntT _) = ScalarT IntT ffalse
refineBot _ (ScalarT BoolT _) = ScalarT BoolT ffalse
refineBot _ (ScalarT (TypeVarT vSubst a) _) = ScalarT (TypeVarT vSubst a) ffalse
refineBot env (FunctionT x tArg tFun) = FunctionT x (refineTop env tArg) (refineBot env tFun)

{- Input language declarations -}


constructorName (ConstructorSig name _) = name


isSynthesisGoal (Pos _ (SynthesisGoal _ _)) = True
isSynthesisGoal _ = False

{- Misc -}


unresolvedType env ident = (env ^. unresolvedConstants) Map.! ident
-- unresolvedSpec goal = unresolvedType (gEnvironment goal) (gName goal)
unresolvedSpec goal = gSpec goal

-- | analysis of program components for exploration convenience
depth (Program p (_,t,_)) = case p of
  PApp fun arg -> case t of
    FunctionT _ _ _ -> max (depth fun) (depth arg)
    _ -> 1 + max (depth fun) (depth arg)
  _ -> 0

countHole (Program p _) = case p of
  PApp fun arg -> (countHole fun) + (countHole arg)
  PHole -> 1
  _ -> 0

holeTypes (Program p (sty, _, _)) = case p of
  PApp fun arg -> holeTypes fun `Set.union` holeTypes arg
  PHole -> Set.singleton sty
  _ -> Set.empty

termSize (Program p _) = case p of
  PApp fun arg -> 1 + (termSize fun) + (termSize arg)
  _ -> 1
