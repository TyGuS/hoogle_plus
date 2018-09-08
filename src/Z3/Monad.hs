
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Z3.Monad
-- Copyright : (c) Iago Abal, 2013-2015
--             (c) David Castro, 2013-2015
-- License   : BSD3
-- Maintainer: Iago Abal <mail@iagoabal.eu>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- A simple monadic interface to Z3 API.
--
-- Examples: <https://bitbucket.org/iago/z3-haskell/src/tip/examples/Example/Monad>

module Z3.Monad
  ( -- * Z3 monad
    MonadZ3(..)
  , Z3
  , module Z3.Opts
  , Logic(..)
  , evalZ3
  , evalZ3With
    -- ** Z3 enviroments
  , Z3Env
  , newEnv
  , newItpEnv
  , evalZ3WithEnv

  -- * Types
  , Symbol
  , AST
  , Sort
  , FuncDecl
  , App
  , Pattern
  , Constructor
  , Model
  , Base.Context
  , FuncInterp
  , FuncEntry
  , Params
  , Solver
  , SortKind(..)
  , ASTKind(..)
  -- ** Satisfiability result
  , Result(..)

  -- * Parameters
  , mkParams
  , paramsSetBool
  , paramsSetUInt
  , paramsSetDouble
  , paramsSetSymbol
  , paramsToString

  -- * Symbols
  , mkIntSymbol
  , mkStringSymbol

  -- * Sorts
  , mkUninterpretedSort
  , mkBoolSort
  , mkIntSort
  , mkRealSort
  , mkBvSort
  , mkArraySort
  , mkTupleSort
  , mkConstructor
  , mkDatatype
  , mkDatatypes
  , mkSetSort

  -- * Constants and Applications
  , mkFuncDecl
  , mkApp
  , mkConst
  , mkFreshConst
  , mkFreshFuncDecl
  -- ** Helpers
  , mkVar
  , mkBoolVar
  , mkRealVar
  , mkIntVar
  , mkBvVar
  , mkFreshVar
  , mkFreshBoolVar
  , mkFreshRealVar
  , mkFreshIntVar
  , mkFreshBvVar

  -- * Propositional Logic and Equality
  , mkTrue
  , mkFalse
  , mkEq
  , mkNot
  , mkIte
  , mkIff
  , mkImplies
  , mkXor
  , mkAnd
  , mkOr
  , mkDistinct
  -- ** Helpers
  , mkBool

  -- * Arithmetic: Integers and Reals
  , mkAdd
  , mkMul
  , mkSub
  , mkUnaryMinus
  , mkDiv
  , mkMod
  , mkRem
  , mkLt
  , mkLe
  , mkGt
  , mkGe
  , mkInt2Real
  , mkReal2Int
  , mkIsInt

  -- * Bit-vectors
  , mkBvnot
  , mkBvredand
  , mkBvredor
  , mkBvand
  , mkBvor
  , mkBvxor
  , mkBvnand
  , mkBvnor
  , mkBvxnor
  , mkBvneg
  , mkBvadd
  , mkBvsub
  , mkBvmul
  , mkBvudiv
  , mkBvsdiv
  , mkBvurem
  , mkBvsrem
  , mkBvsmod
  , mkBvult
  , mkBvslt
  , mkBvule
  , mkBvsle
  , mkBvuge
  , mkBvsge
  , mkBvugt
  , mkBvsgt
  , mkConcat
  , mkExtract
  , mkSignExt
  , mkZeroExt
  , mkRepeat
  , mkBvshl
  , mkBvlshr
  , mkBvashr
  , mkRotateLeft
  , mkRotateRight
  , mkExtRotateLeft
  , mkExtRotateRight
  , mkInt2bv
  , mkBv2int
  , mkBvnegNoOverflow
  , mkBvaddNoOverflow
  , mkBvaddNoUnderflow
  , mkBvsubNoOverflow
  , mkBvsubNoUnderflow
  , mkBvmulNoOverflow
  , mkBvmulNoUnderflow
  , mkBvsdivNoOverflow

  -- * Arrays
  , mkSelect
  , mkStore
  , mkConstArray
  , mkMap
  , mkArrayDefault

  -- * Sets
  , mkEmptySet
  , mkFullSet
  , mkSetAdd
  , mkSetDel
  , mkSetUnion
  , mkSetIntersect
  , mkSetDifference
  , mkSetComplement
  , mkSetMember
  , mkSetSubset

  -- * Numerals
  , mkNumeral
  , mkInt
  , mkReal
  , mkUnsignedInt
  , mkInt64
  , mkUnsignedInt64
  -- ** Helpers
  , mkIntegral
  , mkRational
  , mkFixed
  , mkRealNum
  , mkInteger
  , mkIntNum
  , mkBitvector
  , mkBvNum

  -- * Quantifiers
  , mkPattern
  , mkBound
  , mkForall
  , mkExists
  , mkForallConst
  , mkExistsConst

  -- * Accessors
  , getSymbolString
  , getSortKind
  , getBvSortSize
  , getDatatypeSortConstructors
  , getDatatypeSortRecognizers
  , getDatatypeSortConstructorAccessors
  , getDeclName
  , getArity
  , getDomain
  , getRange
  , appToAst
  , getAppDecl
  , getAppNumArgs
  , getAppArg
  , getAppArgs
  , getSort
  , getArraySortDomain
  , getArraySortRange
  , getBoolValue
  , getAstKind
  , isApp
  , toApp
  , getNumeralString
  , simplify
  , simplifyEx
  , getIndexValue
  , isQuantifierForall
  , isQuantifierExists
  , getQuantifierWeight
  , getQuantifierNumPatterns
  , getQuantifierPatternAST
  , getQuantifierPatterns
  , getQuantifierNumNoPatterns
  , getQuantifierNoPatternAST
  , getQuantifierNoPatterns
  , getQuantifierNumBound
  , getQuantifierBoundName
  , getQuantifierBoundSort
  , getQuantifierBoundVars
  , getQuantifierBody
  -- ** Helpers
  , getBool
  , getInt
  , getReal
  , getBv

  -- * Modifiers
  , substituteVars

  -- * Models
  , modelEval
  , evalArray
  , getConstInterp
  , getFuncInterp
  , hasInterp
  , numConsts
  , numFuncs
  , getConstDecl
  , getFuncDecl
  , getConsts
  , getFuncs
  , isAsArray
  , addFuncInterp
  , addConstInterp
  , getAsArrayFuncDecl
  , funcInterpGetNumEntries
  , funcInterpGetEntry
  , funcInterpGetElse
  , funcInterpGetArity
  , funcEntryGetValue
  , funcEntryGetNumArgs
  , funcEntryGetArg
  , modelToString
  , showModel
  -- ** Helpers
  , EvalAst
  , eval
  , evalBool
  , evalInt
  , evalReal
  , evalBv
  , evalT
  , mapEval
  , FuncModel(..)
  , evalFunc

  -- * Tactics
  , mkTactic
  , andThenTactic
  , orElseTactic
  , skipTactic
  , tryForTactic
  , mkQuantifierEliminationTactic
  , mkAndInverterGraphTactic
  , applyTactic
  , getApplyResultNumSubgoals
  , getApplyResultSubgoal
  , getApplyResultSubgoals
  , mkGoal
  , goalAssert
  , getGoalSize
  , getGoalFormula
  , getGoalFormulas

  -- * String Conversion
  , ASTPrintMode(..)
  , setASTPrintMode
  , astToString
  , patternToString
  , sortToString
  , funcDeclToString
  , benchmarkToSMTLibString

  -- * Parser interface
  , parseSMTLib2String
  , parseSMTLib2File
  , getParserError

  -- * Error Handling
  , Base.Z3Error(..)
  , Base.Z3ErrorCode(..)

  -- * Miscellaneous
  , Version(..)
  , getVersion

  -- * Fixedpoint
  , Fixedpoint
  , fixedpointPush
  , fixedpointPop
  , fixedpointAddRule
  , fixedpointSetParams
  , fixedpointRegisterRelation
  , fixedpointQueryRelations
  , fixedpointGetAnswer
  , fixedpointGetAssertions

  -- * Interpolation
  , Base.InterpolationProblem(..)
  , mkInterpolant
  , Base.mkInterpolationContext
  , getInterpolant
  , computeInterpolant
  , readInterpolationProblem
  , checkInterpolant
  , interpolationProfile
  , writeInterpolationProblem

  -- * Solvers
  , solverGetHelp
  , solverSetParams
  , solverPush
  , solverPop
  , solverReset
  , solverGetNumScopes
  , solverAssertCnstr
  , solverAssertAndTrack
  , solverCheck
  , solverCheckAssumptions
  , solverGetModel
  , solverGetUnsatCore
  , solverGetReasonUnknown
  , solverToString
  -- ** Helpers
  , assert
  , check
  , checkAssumptions
  , solverCheckAndGetModel
  , getModel
  , withModel
  , getUnsatCore
  , push
  , pop
  , local
  , reset
  , getNumScopes
  )
  where

import Z3.Opts
import Z3.Base
  ( Symbol
  , AST
  , Sort
  , FuncDecl
  , App
  , Pattern
  , Constructor
  , Model
  , FuncInterp
  , FuncEntry
  , FuncModel(..)
  , Result(..)
  , Logic(..)
  , ASTPrintMode(..)
  , Version(..)
  , Params
  , Solver
  , Fixedpoint
  , SortKind(..)
  , ASTKind(..)
  , Tactic
  , ApplyResult
  , Goal
  )
import qualified Z3.Base as Base

import Control.Applicative ( Applicative )
import Data.Fixed ( Fixed, HasResolution )
import Control.Monad.Reader ( ReaderT, runReaderT, asks )
import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad.Fix ( MonadFix )
import Data.Int ( Int64 )
import Data.Word ( Word, Word64 )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T

---------------------------------------------------------------------
-- The Z3 monad-class

class (Applicative m, Monad m, MonadIO m) => MonadZ3 m where
  getSolver  :: m Base.Solver
  getContext :: m Base.Context

-------------------------------------------------
-- Lifting

-- TODO: Rename to liftFun0 for consistency
liftScalar :: MonadZ3 z3 => (Base.Context -> IO b) -> z3 b
liftScalar f = liftIO . f =<< getContext

liftFun1 :: MonadZ3 z3 => (Base.Context -> a -> IO b) -> a -> z3 b
liftFun1 f a = getContext >>= \ctx -> liftIO (f ctx a)

liftFun2 :: MonadZ3 z3 => (Base.Context -> a -> b -> IO c) -> a -> b -> z3 c
liftFun2 f a b = getContext >>= \ctx -> liftIO (f ctx a b)

liftFun3 :: MonadZ3 z3 => (Base.Context -> a -> b -> c -> IO d)
                              -> a -> b -> c -> z3 d
liftFun3 f a b c = getContext >>= \ctx -> liftIO (f ctx a b c)

liftFun4 :: MonadZ3 z3 => (Base.Context -> a -> b -> c -> d -> IO e)
                -> a -> b -> c -> d -> z3 e
liftFun4 f a b c d = getContext >>= \ctx -> liftIO (f ctx a b c d)

liftFun5 :: MonadZ3 z3 =>
              (Base.Context -> a1 -> a2 -> a3 -> a4 -> a5 -> IO b)
                -> a1 -> a2 -> a3 -> a4 -> a5-> z3 b
liftFun5 f x1 x2 x3 x4 x5 =
  getContext >>= \ctx -> liftIO (f ctx x1 x2 x3 x4 x5)

liftFun6 :: MonadZ3 z3 =>
              (Base.Context -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> IO b)
                -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> z3 b
liftFun6 f x1 x2 x3 x4 x5 x6 =
  getContext >>= \ctx -> liftIO (f ctx x1 x2 x3 x4 x5 x6)

liftSolver0 :: MonadZ3 z3 =>
       (Base.Context -> Base.Solver -> IO b)
    -> z3 b
liftSolver0 f_s =
  do ctx <- getContext
     liftIO . f_s ctx =<< getSolver

liftSolver1 :: MonadZ3 z3 =>
       (Base.Context -> Base.Solver -> a -> IO b)
    -> a -> z3 b
liftSolver1 f_s a =
  do ctx <- getContext
     liftIO . (\s -> f_s ctx s a) =<< getSolver

liftSolver2 :: MonadZ3 z3 => (Base.Context -> Base.Solver -> a -> b -> IO c)
                             -> a -> b -> z3 c
liftSolver2 f a b = do
  ctx <- getContext
  slv <- getSolver
  liftIO $ f ctx slv a b

liftFixedpoint0 :: MonadFixedpoint z3 =>
       (Base.Context -> Base.Fixedpoint -> IO b)
    -> z3 b
liftFixedpoint0 f_s =
  do ctx <- getContext
     liftIO . f_s ctx =<< getFixedpoint

liftFixedpoint1 :: MonadFixedpoint z3 =>
       (Base.Context -> Base.Fixedpoint -> a -> IO b)
    -> a -> z3 b
liftFixedpoint1 f_s a =
  do ctx <- getContext
     liftIO . (\s -> f_s ctx s a) =<< getFixedpoint

liftFixedpoint2 :: MonadFixedpoint z3 => (Base.Context -> Base.Fixedpoint -> a -> b -> IO c)
                             -> a -> b -> z3 c
liftFixedpoint2 f a b = do
  ctx <- getContext
  slv <- getFixedpoint
  liftIO $ f ctx slv a b

-------------------------------------------------
-- A simple Z3 monad.

newtype Z3 a = Z3 { _unZ3 :: ReaderT Z3Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

-- | Z3 environment.
data Z3Env
  = Z3Env {
      envSolver     :: Base.Solver
    , envContext    :: Base.Context
    , envFixedpoint :: Base.Fixedpoint
    }

instance MonadZ3 Z3 where
  getSolver  = Z3 $ asks envSolver
  getContext = Z3 $ asks envContext

instance MonadFixedpoint Z3 where
  getFixedpoint = Z3 $ asks envFixedpoint

-- | Eval a Z3 script.
evalZ3With :: Maybe Logic -> Opts -> Z3 a -> IO a
evalZ3With mbLogic opts (Z3 s) = do
  env <- newEnv mbLogic opts
  runReaderT s env

-- | Eval a Z3 script with default configuration options.
evalZ3 :: Z3 a -> IO a
evalZ3 = evalZ3With Nothing stdOpts


newEnvWith :: (Base.Config -> IO Base.Context) -> Maybe Logic -> Opts -> IO Z3Env
newEnvWith mkContext mbLogic opts =
  Base.withConfig $ \cfg -> do
    setOpts cfg opts
    ctx <- mkContext cfg
    solver <- maybe (Base.mkSolver ctx) (Base.mkSolverForLogic ctx) mbLogic
    fixedpoint <- Base.mkFixedpoint ctx
    return $ Z3Env solver ctx fixedpoint

-- | Create a new Z3 environment.
newEnv :: Maybe Logic -> Opts -> IO Z3Env
newEnv = newEnvWith Base.mkContext

newItpEnv :: Maybe Logic -> Opts -> IO Z3Env
newItpEnv = newEnvWith Base.mkInterpolationContext

-- | Eval a Z3 script with a given environment.
--
-- Environments may facilitate running many queries under the same
-- logical context.
--
-- Note that an environment may change after each query.
-- If you want to preserve the same environment then use 'local', as in
-- @evalZ3WithEnv /env/ (local /query/)@.
evalZ3WithEnv :: Z3 a
              -> Z3Env
              -> IO a
evalZ3WithEnv (Z3 s) = runReaderT s

---------------------------------------------------------------------
-- * Parameters

-- | Create a Z3 (empty) parameter set.
--
-- Starting at Z3 4.0, parameter sets are used to configure many components
-- such as: simplifiers, tactics, solvers, etc.
mkParams :: MonadZ3 z3 => z3 Params
mkParams = liftScalar Base.mkParams

-- | Add a Boolean parameter /k/ with value /v/ to the parameter set /p/.
paramsSetBool :: MonadZ3 z3 => Params -> Symbol -> Bool -> z3 ()
paramsSetBool = liftFun3 Base.paramsSetBool

-- | Add a unsigned parameter /k/ with value /v/ to the parameter set /p/.
paramsSetUInt :: MonadZ3 z3 => Params -> Symbol -> Word -> z3 ()
paramsSetUInt = liftFun3 Base.paramsSetUInt

-- | Add a double parameter /k/ with value /v/ to the parameter set /p/.
paramsSetDouble :: MonadZ3 z3 => Params -> Symbol -> Double -> z3 ()
paramsSetDouble = liftFun3 Base.paramsSetDouble

-- | Add a symbol parameter /k/ with value /v/ to the parameter set /p/.
paramsSetSymbol :: MonadZ3 z3 => Params -> Symbol -> Symbol -> z3 ()
paramsSetSymbol = liftFun3 Base.paramsSetSymbol

-- | Convert a parameter set into a string.
--
-- This function is mainly used for printing the contents of a parameter set.
paramsToString :: MonadZ3 z3 => Params -> z3 String
paramsToString = liftFun1 Base.paramsToString

-- TODO: Z3_params_validate

---------------------------------------------------------------------
-- Symbols

-- | Create a Z3 symbol using an integer.
mkIntSymbol :: (MonadZ3 z3, Integral i) => i -> z3 Symbol
mkIntSymbol = liftFun1 Base.mkIntSymbol

-- | Create a Z3 symbol using a string.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafebb0d3c212927cf7834c3a20a84ecae>
mkStringSymbol :: MonadZ3 z3 => String -> z3 Symbol
mkStringSymbol = liftFun1 Base.mkStringSymbol

---------------------------------------------------------------------
-- Sorts

-- | Create a free (uninterpreted) type using the given name (symbol).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga736e88741af1c178cbebf94c49aa42de>
mkUninterpretedSort :: MonadZ3 z3 => Symbol -> z3 Sort
mkUninterpretedSort = liftFun1 Base.mkUninterpretedSort

-- | Create the /boolean/ type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacdc73510b69a010b71793d429015f342>
mkBoolSort :: MonadZ3 z3 => z3 Sort
mkBoolSort = liftScalar Base.mkBoolSort

-- | Create the /integer/ type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6cd426ab5748653b77d389fd3eac1015>
mkIntSort :: MonadZ3 z3 => z3 Sort
mkIntSort = liftScalar Base.mkIntSort

-- | Create the /real/ type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga40ef93b9738485caed6dc84631c3c1a0>
mkRealSort :: MonadZ3 z3 => z3 Sort
mkRealSort = liftScalar Base.mkRealSort

-- | Create a bit-vector type of the given size.
--
-- This type can also be seen as a machine integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeed000a1bbb84b6ca6fdaac6cf0c1688>
mkBvSort :: MonadZ3 z3 => Int -> z3 Sort
mkBvSort = liftFun1 Base.mkBvSort

-- | Create an array type
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafe617994cce1b516f46128e448c84445>
--
mkArraySort :: MonadZ3 z3 => Sort -> Sort -> z3 Sort
mkArraySort = liftFun2 Base.mkArraySort

-- | Create a tuple type
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7156b9c0a76a28fae46c81f8e3cdf0f1>
mkTupleSort :: MonadZ3 z3
            => Symbol                          -- ^ Name of the sort
            -> [(Symbol, Sort)]                -- ^ Name and sort of each field
            -> z3 (Sort, FuncDecl, [FuncDecl]) -- ^ Resulting sort, and function
                                               -- declarations for the
                                               -- constructor and projections.
mkTupleSort = liftFun2 Base.mkTupleSort

-- | Create a contructor
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa779e39f7050b9d51857887954b5f9b0>
mkConstructor :: MonadZ3 z3
              => Symbol                       -- ^ Name of the sonstructor
              -> Symbol                       -- ^ Name of recognizer function
              -> [(Symbol, Maybe Sort, Int)]  -- ^ Name, sort option, and sortRefs
              -> z3 Constructor
mkConstructor = liftFun3 Base.mkConstructor

-- | Create datatype, such as lists, trees, records, enumerations or unions of
--   records. The datatype may be recursive. Return the datatype sort.
--
-- Reference <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab6809d53327d807da9158abdf75df387>
mkDatatype :: MonadZ3 z3
           => Symbol
           -> [Constructor]
           -> z3 Sort
mkDatatype = liftFun2 Base.mkDatatype

-- | Create mutually recursive datatypes, such as a tree and forest.
--
-- Returns the datatype sorts
mkDatatypes :: MonadZ3 z3
            => [Symbol]
            -> [[Constructor]]
            -> z3 [Sort]
mkDatatypes = liftFun2 Base.mkDatatypes

-- | Create a set type
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6865879523e7e882d7e50a2d8445ac8b>
--
mkSetSort :: MonadZ3 z3 => Sort -> z3 Sort
mkSetSort = liftFun1 Base.mkSetSort

---------------------------------------------------------------------
-- Constants and Applications

-- | A Z3 function
mkFuncDecl :: MonadZ3 z3 => Symbol -> [Sort] -> Sort -> z3 FuncDecl
mkFuncDecl = liftFun3 Base.mkFuncDecl

-- | Create a constant or function application.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga33a202d86bf628bfab9b6f437536cebe>
mkApp :: MonadZ3 z3 => FuncDecl -> [AST] -> z3 AST
mkApp = liftFun2 Base.mkApp

-- | Declare and create a constant.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga093c9703393f33ae282ec5e8729354ef>
mkConst :: MonadZ3 z3 => Symbol -> Sort -> z3 AST
mkConst = liftFun2 Base.mkConst

-- | Declare and create a constant.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga093c9703393f33ae282ec5e8729354ef>
mkFreshConst :: MonadZ3 z3 => String -> Sort -> z3 AST
mkFreshConst = liftFun2 Base.mkFreshConst

-- | Declare a fresh constant or function.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1f60c7eb41c5603e55a188a14dc929ec>
mkFreshFuncDecl :: MonadZ3 z3 => String -> [Sort] -> Sort -> z3 FuncDecl
mkFreshFuncDecl = liftFun3 Base.mkFreshFuncDecl

-------------------------------------------------
-- ** Helpers

-- | Declare and create a variable (aka /constant/).
--
-- An alias for 'mkConst'.
mkVar :: MonadZ3 z3 => Symbol -> Sort -> z3 AST
mkVar = liftFun2 Base.mkVar

-- | Declarate and create a variable of sort /bool/.
--
-- See 'mkVar'.
mkBoolVar :: MonadZ3 z3 => Symbol -> z3 AST
mkBoolVar = liftFun1 Base.mkBoolVar

-- | Declarate and create a variable of sort /real/.
--
-- See 'mkVar'.
mkRealVar :: MonadZ3 z3 => Symbol -> z3 AST
mkRealVar = liftFun1 Base.mkRealVar

-- | Declarate and create a variable of sort /int/.
--
-- See 'mkVar'.
mkIntVar :: MonadZ3 z3 => Symbol -> z3 AST
mkIntVar = liftFun1 Base.mkIntVar

-- | Declarate and create a variable of sort /bit-vector/.
--
-- See 'mkVar'.
mkBvVar :: MonadZ3 z3 => Symbol
                   -> Int     -- ^ bit-width
                   -> z3 AST
mkBvVar = liftFun2 Base.mkBvVar

-- | Declare and create a /fresh/ variable (aka /constant/).
--
-- An alias for 'mkFreshConst'.
mkFreshVar :: MonadZ3 z3 => String -> Sort -> z3 AST
mkFreshVar = liftFun2 Base.mkFreshConst

-- | Declarate and create a /fresh/ variable of sort /bool/.
--
-- See 'mkFreshVar'.
mkFreshBoolVar :: MonadZ3 z3 => String -> z3 AST
mkFreshBoolVar = liftFun1 Base.mkFreshBoolVar

-- | Declarate and create a /fresh/ variable of sort /real/.
--
-- See 'mkFreshVar'.
mkFreshRealVar :: MonadZ3 z3 => String -> z3 AST
mkFreshRealVar = liftFun1 Base.mkFreshRealVar

-- | Declarate and create a /fresh/ variable of sort /int/.
--
-- See 'mkFreshVar'.
mkFreshIntVar :: MonadZ3 z3 => String -> z3 AST
mkFreshIntVar = liftFun1 Base.mkFreshIntVar

-- | Declarate and create a /fresh/ variable of sort /bit-vector/.
--
-- See 'mkFreshVar'.
mkFreshBvVar :: MonadZ3 z3 => String
                        -> Int     -- ^ bit-width
                        -> z3 AST
mkFreshBvVar = liftFun2 Base.mkFreshBvVar

---------------------------------------------------------------------
-- Propositional Logic and Equality

-- | Create an AST node representing /true/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae898e7380409bbc57b56cc5205ef1db7>
mkTrue :: MonadZ3 z3 => z3 AST
mkTrue = liftScalar Base.mkTrue

-- | Create an AST node representing /false/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5952ac17671117a02001fed6575c778d>
mkFalse :: MonadZ3 z3 => z3 AST
mkFalse = liftScalar Base.mkFalse

-- | Create an AST node representing /l = r/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95a19ce675b70e22bb0401f7137af37c>
mkEq :: MonadZ3 z3 => AST -> AST -> z3 AST
mkEq = liftFun2 Base.mkEq

-- | The distinct construct is used for declaring the arguments pairwise
-- distinct.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa076d3a668e0ec97d61744403153ecf7>
mkDistinct :: MonadZ3 z3 => [AST] -> z3 AST
mkDistinct = liftFun1 Base.mkDistinct

-- | Create an AST node representing /not(a)/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3329538091996eb7b3dc677760a61072>
mkNot :: MonadZ3 z3 => AST -> z3 AST
mkNot = liftFun1 Base.mkNot

-- | Create an AST node representing an if-then-else: /ite(t1, t2, t3)/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94417eed5c36e1ad48bcfc8ad6e83547>
mkIte :: MonadZ3 z3 => AST -> AST -> AST -> z3 AST
mkIte = liftFun3 Base.mkIte

-- | Create an AST node representing /t1 iff t2/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga930a8e844d345fbebc498ac43a696042>
mkIff :: MonadZ3 z3 => AST -> AST -> z3 AST
mkIff = liftFun2 Base.mkIff

-- | Create an AST node representing /t1 implies t2/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac829c0e25bbbd30343bf073f7b524517>
mkImplies :: MonadZ3 z3 => AST -> AST -> z3 AST
mkImplies = liftFun2 Base.mkImplies

-- | Create an AST node representing /t1 xor t2/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacc6d1b848032dec0c4617b594d4229ec>
mkXor :: MonadZ3 z3 => AST -> AST -> z3 AST
mkXor = liftFun2 Base.mkXor

-- | Create an AST node representing args[0] and ... and args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacde98ce4a8ed1dde50b9669db4838c61>
mkAnd :: MonadZ3 z3 => [AST] -> z3 AST
mkAnd = liftFun1 Base.mkAnd

-- | Create an AST node representing args[0] or ... or args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga00866d16331d505620a6c515302021f9>
mkOr :: MonadZ3 z3 => [AST] -> z3 AST
mkOr = liftFun1 Base.mkOr

-------------------------------------------------
-- ** Helpers

-- | Create an AST node representing the given boolean.
mkBool :: MonadZ3 z3 => Bool -> z3 AST
mkBool = liftFun1 Base.mkBool

---------------------------------------------------------------------
-- Arithmetic: Integers and Reals

-- | Create an AST node representing args[0] + ... + args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e4ac0a4e53eee0b4b0ef159ed7d0cd5>
mkAdd :: MonadZ3 z3 => [AST] -> z3 AST
mkAdd = liftFun1 Base.mkAdd

-- | Create an AST node representing args[0] * ... * args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab9affbf8401a18eea474b59ad4adc890>
mkMul :: MonadZ3 z3 => [AST] -> z3 AST
mkMul = liftFun1 Base.mkMul

-- | Create an AST node representing args[0] - ... - args[num_args - 1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4f5fea9b683f9e674fd8f14d676cc9a9>
mkSub :: MonadZ3 z3 => [AST] -> z3 AST
mkSub = liftFun1 Base.mkSub

-- | Create an AST node representing -arg.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gadcd2929ad732937e25f34277ce4988ea>
mkUnaryMinus :: MonadZ3 z3 => AST -> z3 AST
mkUnaryMinus = liftFun1 Base.mkUnaryMinus

-- | Create an AST node representing arg1 div arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1ac60ee8307af8d0b900375914194ff3>
mkDiv :: MonadZ3 z3 => AST -> AST -> z3 AST
mkDiv = liftFun2 Base.mkDiv

-- | Create an AST node representing arg1 mod arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8e350ac77e6b8fe805f57efe196e7713>
mkMod :: MonadZ3 z3 => AST -> AST -> z3 AST
mkMod = liftFun2 Base.mkMod

-- | Create an AST node representing arg1 rem arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2fcdb17f9039bbdaddf8a30d037bd9ff>
mkRem :: MonadZ3 z3 => AST -> AST -> z3 AST
mkRem = liftFun2 Base.mkRem

-- | Create less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga58a3dc67c5de52cf599c346803ba1534>
mkLt :: MonadZ3 z3 => AST -> AST -> z3 AST
mkLt = liftFun2 Base.mkLt

-- | Create less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa9a33d11096841f4e8c407f1578bc0bf>
mkLe :: MonadZ3 z3 => AST -> AST -> z3 AST
mkLe = liftFun2 Base.mkLe

-- | Create greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46167b86067586bb742c0557d7babfd3>
mkGt :: MonadZ3 z3 => AST -> AST -> z3 AST
mkGt = liftFun2 Base.mkGt

-- | Create greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad9245cbadb80b192323d01a8360fb942>
mkGe :: MonadZ3 z3 => AST -> AST -> z3 AST
mkGe = liftFun2 Base.mkGe

-- | Coerce an integer to a real.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7130641e614c7ebafd28ae16a7681a21>
mkInt2Real :: MonadZ3 z3 => AST -> z3 AST
mkInt2Real = liftFun1 Base.mkInt2Real

-- | Coerce a real to an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga759b6563ba1204aae55289009a3fdc6d>
mkReal2Int :: MonadZ3 z3 => AST -> z3 AST
mkReal2Int = liftFun1 Base.mkReal2Int

-- | Check if a real number is an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaac2ad0fb04e4900fdb4add438d137ad3>
mkIsInt :: MonadZ3 z3 => AST -> z3 AST
mkIsInt = liftFun1 Base.mkIsInt

---------------------------------------------------------------------
-- Bit-vectors

-- | Bitwise negation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga36cf75c92c54c1ca633a230344f23080>
mkBvnot :: MonadZ3 z3 => AST -> z3 AST
mkBvnot = liftFun1 Base.mkBvnot

-- | Take conjunction of bits in vector, return vector of length 1.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaccc04f2b58903279b1b3be589b00a7d8>
mkBvredand :: MonadZ3 z3 => AST -> z3 AST
mkBvredand = liftFun1 Base.mkBvredand

-- | Take disjunction of bits in vector, return vector of length 1.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafd18e127c0586abf47ad9cd96895f7d2>
mkBvredor :: MonadZ3 z3 => AST -> z3 AST
mkBvredor = liftFun1 Base.mkBvredor

-- | Bitwise and.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab96e0ea55334cbcd5a0e79323b57615d>
mkBvand :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvand  = liftFun2 Base.mkBvand

-- | Bitwise or.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga77a6ae233fb3371d187c6d559b2843f5>
mkBvor :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvor = liftFun2 Base.mkBvor

-- | Bitwise exclusive-or.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0a3821ea00b1c762205f73e4bc29e7d8>
mkBvxor :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvxor = liftFun2 Base.mkBvxor

-- | Bitwise nand.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga96dc37d36efd658fff5b2b4df49b0e61>
mkBvnand :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvnand = liftFun2 Base.mkBvnand

-- | Bitwise nor.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabf15059e9e8a2eafe4929fdfd259aadb>
mkBvnor :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvnor = liftFun2 Base.mkBvnor

-- | Bitwise xnor.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga784f5ca36a4b03b93c67242cc94b21d6>
mkBvxnor :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvxnor = liftFun2 Base.mkBvxnor

-- | Standard two's complement unary minus.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0c78be00c03eda4ed6a983224ed5c7b7
mkBvneg :: MonadZ3 z3 => AST -> z3 AST
mkBvneg = liftFun1 Base.mkBvneg

-- | Standard two's complement addition.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga819814e33573f3f9948b32fdc5311158>
mkBvadd :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvadd = liftFun2 Base.mkBvadd

-- | Standard two's complement subtraction.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga688c9aa1347888c7a51be4e46c19178e>
mkBvsub :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsub = liftFun2 Base.mkBvsub

-- | Standard two's complement multiplication.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6abd3dde2a1ceff1704cf7221a72258c>
mkBvmul :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvmul = liftFun2 Base.mkBvmul

-- | Unsigned division.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga56ce0cd61666c6f8cf5777286f590544>
mkBvudiv :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvudiv = liftFun2 Base.mkBvudiv

-- | Two's complement signed division.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad240fedb2fda1c1005b8e9d3c7f3d5a0>
mkBvsdiv :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsdiv = liftFun2 Base.mkBvsdiv

-- | Unsigned remainder.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5df4298ec835e43ddc9e3e0bae690c8d>
mkBvurem :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvurem = liftFun2 Base.mkBvurem

-- | Two's complement signed remainder (sign follows dividend).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46c18a3042fca174fe659d3185693db1>
mkBvsrem :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsrem = liftFun2 Base.mkBvsrem

-- | Two's complement signed remainder (sign follows divisor).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95dac8e6eecb50f63cb82038560e0879>
mkBvsmod :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsmod = liftFun2 Base.mkBvsmod

-- | Unsigned less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5774b22e93abcaf9b594672af6c7c3c4>
mkBvult :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvult = liftFun2 Base.mkBvult

-- | Two's complement signed less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8ce08af4ed1fbdf08d4d6e63d171663a>
mkBvslt :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvslt = liftFun2 Base.mkBvslt

-- | Unsigned less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab738b89de0410e70c089d3ac9e696e87>
mkBvule :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvule = liftFun2 Base.mkBvule

-- | Two's complement signed less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab7c026feb93e7d2eab180e96f1e6255d>
mkBvsle :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsle = liftFun2 Base.mkBvsle

-- | Unsigned greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gade58fbfcf61b67bf8c4a441490d3c4df>
mkBvuge :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvuge = liftFun2 Base.mkBvuge

-- | Two's complement signed greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeec3414c0e8a90a6aa5a23af36bf6dc5>
mkBvsge :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsge = liftFun2 Base.mkBvsge

-- | Unsigned greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga063ab9f16246c99e5c1c893613927ee3>
mkBvugt :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvugt = liftFun2 Base.mkBvugt

-- | Two's complement signed greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e93a985aa2a7812c7c11a2c65d7c5f0>
mkBvsgt :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsgt = liftFun2 Base.mkBvsgt

-- | Concatenate the given bit-vectors.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae774128fa5e9ff7458a36bd10e6ca0fa>
mkConcat :: MonadZ3 z3 => AST -> AST -> z3 AST
mkConcat = liftFun2 Base.mkConcat

-- | Extract the bits high down to low from a bitvector of size m to yield a new
-- bitvector of size /n/, where /n = high - low + 1/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga32d2fe7563f3e6b114c1b97b205d4317>
mkExtract :: MonadZ3 z3 => Int -> Int -> AST -> z3 AST
mkExtract = liftFun3 Base.mkExtract

-- | Sign-extend of the given bit-vector to the (signed) equivalent bitvector
-- of size /m+i/, where /m/ is the size of the given bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad29099270b36d0680bb54b560353c10e>
mkSignExt :: MonadZ3 z3 => Int -> AST -> z3 AST
mkSignExt = liftFun2 Base.mkSignExt

-- | Extend the given bit-vector with zeros to the (unsigned) equivalent
-- bitvector of size /m+i/, where /m/ is the size of the given bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac9322fae11365a78640baf9078c428b3>
mkZeroExt :: MonadZ3 z3 => Int -> AST -> z3 AST
mkZeroExt = liftFun2 Base.mkZeroExt

-- | Repeat the given bit-vector up length /i/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga03e81721502ea225c264d1f556c9119d>
mkRepeat :: MonadZ3 z3 => Int -> AST -> z3 AST
mkRepeat = liftFun2 Base.mkRepeat

-- | Shift left.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8d5e776c786c1172fa0d7dfede454e1>
mkBvshl :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvshl = liftFun2 Base.mkBvshl

-- | Logical shift right.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac59645a6edadad79a201f417e4e0c512>
mkBvlshr :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvlshr = liftFun2 Base.mkBvlshr

-- | Arithmetic shift right.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga674b580ad605ba1c2c9f9d3748be87c4>
mkBvashr :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvashr = liftFun2 Base.mkBvashr

-- | Rotate bits of /t1/ to the left /i/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4932b7d08fea079dd903cd857a52dcda>
mkRotateLeft :: MonadZ3 z3 => Int -> AST -> z3 AST
mkRotateLeft = liftFun2 Base.mkRotateLeft

-- | Rotate bits of /t1/ to the right /i/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3b94e1bf87ecd1a1858af8ebc1da4a1c>
mkRotateRight :: MonadZ3 z3 => Int -> AST -> z3 AST
mkRotateRight = liftFun2 Base.mkRotateRight

-- | Rotate bits of /t1/ to the left /t2/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf46f1cb80e5a56044591a76e7c89e5e7>
mkExtRotateLeft :: MonadZ3 z3 => AST -> AST -> z3 AST
mkExtRotateLeft = liftFun2 Base.mkExtRotateLeft

-- | Rotate bits of /t1/ to the right /t2/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabb227526c592b523879083f12aab281f>
mkExtRotateRight :: MonadZ3 z3 => AST -> AST -> z3 AST
mkExtRotateRight = liftFun2 Base.mkExtRotateRight

-- | Create an /n/ bit bit-vector from the integer argument /t1/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga35f89eb05df43fbd9cce7200cc1f30b5>
mkInt2bv :: MonadZ3 z3 => Int -> AST -> z3 AST
mkInt2bv = liftFun2 Base.mkInt2bv

-- | Create an integer from the bit-vector argument /t1/. If /is_signed/ is false,
-- then the bit-vector /t1/ is treated as unsigned. So the result is non-negative
-- and in the range [0..2^/N/-1], where /N/ are the number of bits in /t1/.
-- If /is_signed/ is true, /t1/ is treated as a signed bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac87b227dc3821d57258d7f53a28323d4>
mkBv2int :: MonadZ3 z3 => AST -> Bool -> z3 AST
mkBv2int = liftFun2 Base.mkBv2int

-- | Create a predicate that checks that the bit-wise addition of /t1/ and /t2/
-- does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga88f6b5ec876f05e0d7ba51e96c4b077f>
mkBvaddNoOverflow :: MonadZ3 z3 => AST -> AST -> Bool -> z3 AST
mkBvaddNoOverflow = liftFun3 Base.mkBvaddNoOverflow

-- | Create a predicate that checks that the bit-wise signed addition of /t1/
-- and /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1e2b1927cf4e50000c1600d47a152947>
mkBvaddNoUnderflow :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvaddNoUnderflow = liftFun2 Base.mkBvaddNoUnderflow

-- | Create a predicate that checks that the bit-wise signed subtraction of /t1/
-- and /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga785f8127b87e0b42130e6d8f52167d7c>
mkBvsubNoOverflow :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsubNoOverflow = liftFun2 Base.mkBvsubNoOverflow

-- | Create a predicate that checks that the bit-wise subtraction of /t1/ and
-- /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6480850f9fa01e14aea936c88ff184c4>
mkBvsubNoUnderflow :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsubNoUnderflow = liftFun2 Base.mkBvsubNoUnderflow

-- | Create a predicate that checks that the bit-wise signed division of /t1/
-- and /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa17e7b2c33dfe2abbd74d390927ae83e>
mkBvsdivNoOverflow :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvsdivNoOverflow = liftFun2 Base.mkBvsdivNoOverflow

-- | Check that bit-wise negation does not overflow when /t1/ is interpreted as
-- a signed bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae9c5d72605ddcd0e76657341eaccb6c7>
mkBvnegNoOverflow :: MonadZ3 z3 => AST -> z3 AST
mkBvnegNoOverflow = liftFun1 Base.mkBvnegNoOverflow

-- | Create a predicate that checks that the bit-wise multiplication of /t1/ and
-- /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga86f4415719d295a2f6845c70b3aaa1df>
mkBvmulNoOverflow :: MonadZ3 z3 => AST -> AST -> Bool -> z3 AST
mkBvmulNoOverflow = liftFun3 Base.mkBvmulNoOverflow

-- | Create a predicate that checks that the bit-wise signed multiplication of
-- /t1/ and /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga501ccc01d737aad3ede5699741717fda>
mkBvmulNoUnderflow :: MonadZ3 z3 => AST -> AST -> z3 AST
mkBvmulNoUnderflow = liftFun2 Base.mkBvmulNoUnderflow

---------------------------------------------------------------------
-- Arrays

-- | Array read. The argument a is the array and i is the index of the array
-- that gets read.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga38f423f3683379e7f597a7fe59eccb67>
mkSelect :: MonadZ3 z3 => AST -> AST -> z3 AST
mkSelect = liftFun2 Base.mkSelect

-- | Array update.   
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae305a4f54b4a64f7e5973ae6ccb13593>
mkStore :: MonadZ3 z3 => AST -> AST -> AST -> z3 AST
mkStore = liftFun3 Base.mkStore

-- | Create the constant array.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga84ea6f0c32b99c70033feaa8f00e8f2d>
mkConstArray :: MonadZ3 z3 => Sort -> AST -> z3 AST
mkConstArray = liftFun2 Base.mkConstArray

-- | map f on the the argument arrays.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga9150242d9430a8c3d55d2ca3b9a4362d>
mkMap :: MonadZ3 z3 => FuncDecl -> [AST] -> z3 AST
mkMap = liftFun2 Base.mkMap

-- | Access the array default value. Produces the default range value, for
-- arrays that can be represented as finite maps with a default range value.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga78e89cca82f0ab4d5f4e662e5e5fba7d>
mkArrayDefault :: MonadZ3 z3 => AST -> z3 AST
mkArrayDefault = liftFun1 Base.mkArrayDefault

---------------------------------------------------------------------
-- Sets

-- | Create the empty set.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga358b6b80509a567148f1c0ca9252118c>
mkEmptySet :: MonadZ3 z3 => Sort -> z3 AST
mkEmptySet = liftFun1 Base.mkEmptySet

-- | Create the full set.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5e92662c657374f7332aa32ce4503dd2>
mkFullSet :: MonadZ3 z3 => Sort -> z3 AST
mkFullSet = liftFun1 Base.mkFullSet

-- | Add an element to a set.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga856c3d0e28ce720f53912c2bbdd76175>
mkSetAdd :: MonadZ3 z3 => AST -> AST -> z3 AST
mkSetAdd = liftFun2 Base.mkSetAdd

-- | Remove an element from a set.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga80e883f39dd3b88f9d0745c8a5b91d1d>
mkSetDel :: MonadZ3 z3 => AST -> AST -> z3 AST
mkSetDel = liftFun2 Base.mkSetDel

-- | Take the union of a list of sets.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4050162a13d539b8913200963bb4743c>
mkSetUnion :: MonadZ3 z3 => [AST] -> z3 AST
mkSetUnion = liftFun1 Base.mkSetUnion

-- | Take the intersection of a list of sets.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8a8abff0ebe6aeeaa6c919eaa013049d>
mkSetIntersect :: MonadZ3 z3 => [AST] -> z3 AST
mkSetIntersect = liftFun1 Base.mkSetIntersect

-- | Take the set difference between two sets.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabb49c62f70b8198362e1a29ba6d8bde1>
mkSetDifference :: MonadZ3 z3 => AST -> AST -> z3 AST
mkSetDifference = liftFun2 Base.mkSetDifference

-- | Take the complement of a set.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5c57143c9229cdf730c5103ff696590f>
mkSetComplement :: MonadZ3 z3 => AST -> z3 AST
mkSetComplement = liftFun1 Base.mkSetComplement

-- | Check for set membership.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac6e516f3dce0bdd41095c6d6daf56063>
mkSetMember :: MonadZ3 z3 => AST -> AST -> z3 AST
mkSetMember = liftFun2 Base.mkSetMember

-- | Check if the first set is a subset of the second set.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga139c5803af0e86464adc7cedc53e7f3a>
mkSetSubset :: MonadZ3 z3 => AST -> AST -> z3 AST
mkSetSubset = liftFun2 Base.mkSetSubset

---------------------------------------------------------------------
-- * Numerals

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8aca397e32ca33618d8024bff32948c>
mkNumeral :: MonadZ3 z3 => String -> Sort -> z3 AST
mkNumeral = liftFun2 Base.mkNumeral

-- | Create a numeral of sort /real/.
mkReal :: MonadZ3 z3 => Int -> Int -> z3 AST
mkReal = liftFun2 Base.mkReal

-- | Create a numeral of an int, bit-vector, or finite-domain sort.
--
-- This function can be use to create numerals that fit in a
-- /machine integer/.
-- It is slightly faster than 'mkNumeral' since it is not necessary
-- to parse a string.
mkInt :: MonadZ3 z3 => Int -> Sort -> z3 AST
mkInt = liftFun2 Base.mkInt

-- | Create a numeral of an int, bit-vector, or finite-domain sort.
--
-- This function can be use to create numerals that fit in a
-- /machine unsigned integer/.
-- It is slightly faster than 'mkNumeral' since it is not necessary
-- to parse a string.
mkUnsignedInt :: MonadZ3 z3 => Word -> Sort -> z3 AST
mkUnsignedInt = liftFun2 Base.mkUnsignedInt

-- | Create a numeral of an int, bit-vector, or finite-domain sort.
--
-- This function can be use to create numerals that fit in a
-- /machine 64-bit integer/.
-- It is slightly faster than 'mkNumeral' since it is not necessary
-- to parse a string.
mkInt64 :: MonadZ3 z3 => Int64 -> Sort -> z3 AST
mkInt64 = liftFun2 Base.mkInt64

-- | Create a numeral of an int, bit-vector, or finite-domain sort.
--
-- This function can be use to create numerals that fit in a
-- /machine unsigned 64-bit integer/.
-- It is slightly faster than 'mkNumeral' since it is not necessary
-- to parse a string.
mkUnsignedInt64 :: MonadZ3 z3 => Word64 -> Sort -> z3 AST
mkUnsignedInt64 = liftFun2 Base.mkUnsignedInt64

-------------------------------------------------
-- ** Helpers

-- | Create a numeral of an int, bit-vector, or finite-domain sort.
mkIntegral :: (MonadZ3 z3, Integral a) => a -> Sort -> z3 AST
mkIntegral = liftFun2 Base.mkIntegral

-- | Create a numeral of sort /real/ from a 'Rational'.
mkRational :: MonadZ3 z3 => Rational -> z3 AST
mkRational = liftFun1 Base.mkRational

-- | Create a numeral of sort /real/ from a 'Fixed'.
mkFixed :: (MonadZ3 z3, HasResolution a) => Fixed a -> z3 AST
mkFixed = liftFun1 Base.mkFixed

-- | Create a numeral of sort /real/ from a 'Real'.
mkRealNum :: (MonadZ3 z3, Real r) => r -> z3 AST
mkRealNum = liftFun1 Base.mkRealNum

-- | Create a numeral of sort /int/ from an 'Integer'.
mkInteger :: MonadZ3 z3 => Integer -> z3 AST
mkInteger = liftFun1 Base.mkInteger

-- | Create a numeral of sort /int/ from an 'Integral'.
mkIntNum :: (MonadZ3 z3, Integral a) => a -> z3 AST
mkIntNum = liftFun1 Base.mkIntNum

-- | Create a numeral of sort /Bit-vector/ from an 'Integer'.
mkBitvector :: MonadZ3 z3 => Int      -- ^ bit-width
                          -> Integer  -- ^ integer value
                          -> z3 AST
mkBitvector = liftFun2 Base.mkBitvector

-- | Create a numeral of sort /Bit-vector/ from an 'Integral'.
mkBvNum :: (MonadZ3 z3, Integral i) => Int    -- ^ bit-width
                                    -> i      -- ^ integer value
                                    -> z3 AST
mkBvNum = liftFun2 Base.mkBvNum

---------------------------------------------------------------------
-- Quantifiers

mkPattern :: MonadZ3 z3 => [AST] -> z3 Pattern
mkPattern = liftFun1 Base.mkPattern

mkBound :: MonadZ3 z3 => Int -> Sort -> z3 AST
mkBound = liftFun2 Base.mkBound

mkForall :: MonadZ3 z3 => [Pattern] -> [Symbol] -> [Sort] -> AST -> z3 AST
mkForall = liftFun4 Base.mkForall

mkForallConst :: MonadZ3 z3 => [Pattern] -> [App] -> AST -> z3 AST
mkForallConst = liftFun3 Base.mkForallConst

mkExistsConst :: MonadZ3 z3 => [Pattern] -> [App] -> AST -> z3 AST
mkExistsConst = liftFun3 Base.mkExistsConst

mkExists :: MonadZ3 z3 => [Pattern] -> [Symbol] -> [Sort] -> AST -> z3 AST
mkExists = liftFun4 Base.mkExists

---------------------------------------------------------------------
-- Accessors

-- | Return the symbol name.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf1683d9464f377e5089ce6ebf2a9bd31>
getSymbolString :: MonadZ3 z3 => Symbol -> z3 String
getSymbolString = liftFun1 Base.getSymbolString

-- | Return the sort kind.
--
-- Reference: <http://z3prover.github.io/api/html/group__capi.html#gacd85d48842c7bfaa696596d16875681a>
getSortKind :: MonadZ3 z3 => Sort -> z3 SortKind
getSortKind = liftFun1 Base.getSortKind

-- | Return the size of the given bit-vector sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8fc3550edace7bc046e16d1f96ddb419>
getBvSortSize :: MonadZ3 z3 => Sort -> z3 Int
getBvSortSize = liftFun1 Base.getBvSortSize

-- | Get list of constructors for datatype.
getDatatypeSortConstructors :: MonadZ3 z3
                            => Sort           -- ^ Datatype sort.
                            -> z3 [FuncDecl]  -- ^ Constructor declarations.
getDatatypeSortConstructors = liftFun1 Base.getDatatypeSortConstructors

-- | Get list of recognizers for datatype.
getDatatypeSortRecognizers :: MonadZ3 z3
                           => Sort           -- ^ Datatype sort.
                           -> z3 [FuncDecl]  -- ^ Constructor recognizers.
getDatatypeSortRecognizers = liftFun1 Base.getDatatypeSortRecognizers

-- | Get list of accessors for datatype.
getDatatypeSortConstructorAccessors :: MonadZ3 z3
                           => Sort           -- ^ Datatype sort.
                           -> z3 [[FuncDecl]]  -- ^ Constructor recognizers.
getDatatypeSortConstructorAccessors = liftFun1 Base.getDatatypeSortConstructorAccessors

-- | Return the constant declaration name as a symbol.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga741b1bf11cb92aa2ec9ef2fef73ff129>
getDeclName :: MonadZ3 z3 => FuncDecl -> z3 Symbol
getDeclName = liftFun1 Base.getDeclName

-- | Returns the number of parameters of the given declaration
getArity :: MonadZ3 z3 => FuncDecl -> z3 Int
getArity = liftFun1 Base.getArity

-- | Returns the sort of the i-th parameter of the given function declaration
getDomain :: MonadZ3 z3
             => FuncDecl         -- ^ A function declaration
             -> Int              -- ^ i
             -> z3 Sort
getDomain = liftFun2 Base.getDomain

-- | Returns the range of the given declaration.
getRange :: MonadZ3 z3 => FuncDecl -> z3 Sort
getRange = liftFun1 Base.getRange

-- | Convert an app into AST. This is just type casting.
appToAst :: MonadZ3 z3 => App -> z3 AST
appToAst = liftFun1 Base.appToAst

-- | Return the declaration of a constant or function application.
getAppDecl :: MonadZ3 z3 => App -> z3 FuncDecl
getAppDecl = liftFun1 Base.getAppDecl

-- | Return the number of argument of an application. If t is an constant, then the number of arguments is 0.
getAppNumArgs :: MonadZ3 z3 => App -> z3 Int
getAppNumArgs = liftFun1 Base.getAppNumArgs

-- | Return the i-th argument of the given application.
getAppArg :: MonadZ3 z3 => App -> Int -> z3 AST
getAppArg = liftFun2 Base.getAppArg

-- | Return a list of all the arguments of the given application.
getAppArgs :: MonadZ3 z3 => App -> z3 [AST]
getAppArgs = liftFun1 Base.getAppArgs

-- | Return the sort of an AST node.
getSort :: MonadZ3 z3 => AST -> z3 Sort
getSort = liftFun1 Base.getSort

getArraySortDomain :: MonadZ3 z3 => Sort -> z3 Sort
getArraySortDomain = liftFun1 Base.getArraySortDomain

getArraySortRange :: MonadZ3 z3 => Sort -> z3 Sort
getArraySortRange = liftFun1 Base.getArraySortRange

-- | Returns @Just True@, @Just False@, or @Nothing@ for /undefined/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga133aaa1ec31af9b570ed7627a3c8c5a4>
getBoolValue :: MonadZ3 z3 => AST -> z3 (Maybe Bool)
getBoolValue = liftFun1 Base.getBoolValue

-- | Return the kind of the given AST.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4c43608feea4cae363ef9c520c239a5c>
getAstKind :: MonadZ3 z3 => AST -> z3 ASTKind
getAstKind = liftFun1 Base.getAstKind

-- | Return True if an ast is APP_AST, False otherwise.
isApp :: MonadZ3 z3 => AST -> z3 Bool
isApp = liftFun1 Base.isApp

-- | Cast AST into an App.
toApp :: MonadZ3 z3 => AST -> z3 App
toApp = liftFun1 Base.toApp

-- | Return numeral value, as a string of a numeric constant term.
getNumeralString :: MonadZ3 z3 => AST -> z3 String
getNumeralString = liftFun1 Base.getNumeralString

getIndexValue :: MonadZ3 z3 => AST -> z3 Int
getIndexValue = liftFun1 Base.getIndexValue

isQuantifierForall :: MonadZ3 z3 => AST -> z3 Bool
isQuantifierForall = liftFun1 Base.isQuantifierForall

isQuantifierExists :: MonadZ3 z3 => AST -> z3 Bool
isQuantifierExists = liftFun1 Base.isQuantifierExists

getQuantifierWeight :: MonadZ3 z3 => AST -> z3 Int
getQuantifierWeight = liftFun1 Base.getQuantifierWeight

getQuantifierNumPatterns :: MonadZ3 z3 => AST -> z3 Int
getQuantifierNumPatterns = liftFun1 Base.getQuantifierNumPatterns

getQuantifierPatternAST :: MonadZ3 z3 => AST -> Int -> z3 AST
getQuantifierPatternAST = liftFun2 Base.getQuantifierPatternAST

getQuantifierPatterns :: MonadZ3 z3 => AST -> z3 [AST]
getQuantifierPatterns = liftFun1 Base.getQuantifierPatterns

getQuantifierNumNoPatterns :: MonadZ3 z3 => AST -> z3 Int
getQuantifierNumNoPatterns = liftFun1 Base.getQuantifierNumNoPatterns

getQuantifierNoPatternAST :: MonadZ3 z3 => AST -> Int -> z3 AST
getQuantifierNoPatternAST = liftFun2 Base.getQuantifierNoPatternAST

getQuantifierNoPatterns :: MonadZ3 z3 => AST -> z3 [AST]
getQuantifierNoPatterns = liftFun1 Base.getQuantifierNoPatterns

getQuantifierNumBound :: MonadZ3 z3 => AST -> z3 Int
getQuantifierNumBound = liftFun1 Base.getQuantifierNumBound

getQuantifierBoundName :: MonadZ3 z3 => AST -> Int -> z3 Symbol
getQuantifierBoundName = liftFun2 Base.getQuantifierBoundName

getQuantifierBoundSort :: MonadZ3 z3 => AST -> Int -> z3 Sort
getQuantifierBoundSort = liftFun2 Base.getQuantifierBoundSort

getQuantifierBoundVars :: MonadZ3 z3 => AST -> z3 [AST]
getQuantifierBoundVars = liftFun1 Base.getQuantifierBoundVars

getQuantifierBody :: MonadZ3 z3 => AST -> z3 AST
getQuantifierBody = liftFun1 Base.getQuantifierBody

-- | Simplify the expression.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gada433553406475e5dd6a494ea957844c>
simplify :: MonadZ3 z3 => AST -> z3 AST
simplify = liftFun1 Base.simplify

-- | Simplify the expression using the given parameters.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga34329d4c83ca8c98e18b2884b679008c>
simplifyEx :: MonadZ3 z3 => AST -> Params -> z3 AST
simplifyEx = liftFun2 Base.simplifyEx

-------------------------------------------------
-- ** Helpers

-- | Read a 'Bool' value from an 'AST'
getBool :: MonadZ3 z3 => AST -> z3 Bool
getBool = liftFun1 Base.getBool

-- | Return the integer value
getInt :: MonadZ3 z3 => AST -> z3 Integer
getInt = liftFun1 Base.getInt

-- | Return rational value
getReal :: MonadZ3 z3 => AST -> z3 Rational
getReal = liftFun1 Base.getReal

-- | Read the 'Integer' value from an 'AST' of sort /bit-vector/.
--
-- See 'mkBv2int'.
getBv :: MonadZ3 z3 => AST
                    -> Bool  -- ^ signed?
                    -> z3 Integer
getBv = liftFun2 Base.getBv


---------------------------------------------------------------------
-- Modifiers

substituteVars :: MonadZ3 z3 => AST -> [AST] -> z3 AST
substituteVars = liftFun2 Base.substituteVars

---------------------------------------------------------------------
-- Models

-- | Evaluate an AST node in the given model.
--
-- The evaluation may fail for the following reasons:
--
--     * /t/ contains a quantifier.
--     * the model /m/ is partial.
--     * /t/ is type incorrect.
modelEval :: MonadZ3 z3 => Model -> AST
             -> Bool  -- ^ Model completion?
             -> z3 (Maybe AST)
modelEval = liftFun3 Base.modelEval

-- | Get array as a list of argument/value pairs, if it is
-- represented as a function (ie, using as-array).
evalArray :: MonadZ3 z3 => Model -> AST -> z3 (Maybe FuncModel)
evalArray = liftFun2 Base.evalArray

getConstInterp :: MonadZ3 z3 => Model -> FuncDecl -> z3 (Maybe AST)
getConstInterp = liftFun2 Base.getConstInterp

-- | Return the interpretation of the function f in the model m.
-- Return NULL, if the model does not assign an interpretation for f.
-- That should be interpreted as: the f does not matter.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafb9cc5eca9564d8a849c154c5a4a8633>
getFuncInterp :: MonadZ3 z3 => Model -> FuncDecl -> z3 (Maybe FuncInterp)
getFuncInterp = liftFun2 Base.getFuncInterp

hasInterp :: MonadZ3 z3 => Model -> FuncDecl -> z3 Bool
hasInterp = liftFun2 Base.hasInterp

numConsts :: MonadZ3 z3 => Model -> z3 Word
numConsts = liftFun1 Base.numConsts

numFuncs :: MonadZ3 z3 => Model -> z3 Word
numFuncs = liftFun1 Base.numFuncs

getConstDecl :: MonadZ3 z3 => Model -> Word -> z3 FuncDecl
getConstDecl = liftFun2 Base.getConstDecl

getFuncDecl :: MonadZ3 z3 => Model -> Word -> z3 FuncDecl
getFuncDecl = liftFun2 Base.getFuncDecl

getConsts :: MonadZ3 z3 => Model -> z3 [FuncDecl]
getConsts = liftFun1 Base.getConsts

getFuncs :: MonadZ3 z3 => Model -> z3 [FuncDecl]
getFuncs = liftFun1 Base.getFuncs

-- | The (_ as-array f) AST node is a construct for assigning interpretations
-- for arrays in Z3. It is the array such that forall indices i we have that
-- (select (_ as-array f) i) is equal to (f i). This procedure returns Z3_TRUE
-- if the a is an as-array AST node.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4674da67d226bfb16861829b9f129cfa>
isAsArray :: MonadZ3 z3 => AST -> z3 Bool
isAsArray = liftFun1 Base.isAsArray

addFuncInterp :: MonadZ3 z3 => Model -> FuncDecl -> AST -> z3 FuncInterp
addFuncInterp = liftFun3 Base.addFuncInterp

addConstInterp :: MonadZ3 z3 => Model -> FuncDecl -> AST -> z3 ()
addConstInterp = liftFun3 Base.addConstInterp


-- | Return the function declaration f associated with a (_ as_array f) node.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7d9262dc6e79f2aeb23fd4a383589dda>
getAsArrayFuncDecl :: MonadZ3 z3 => AST -> z3 FuncDecl
getAsArrayFuncDecl = liftFun1 Base.getAsArrayFuncDecl

-- | Return the number of entries in the given function interpretation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2bab9ae1444940e7593729beec279844>
funcInterpGetNumEntries :: MonadZ3 z3 => FuncInterp -> z3 Int
funcInterpGetNumEntries = liftFun1 Base.funcInterpGetNumEntries

-- | Return a "point" of the given function intepretation.
-- It represents the value of f in a particular point.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf157e1e1cd8c0cfe6a21be6370f659da>
funcInterpGetEntry :: MonadZ3 z3 => FuncInterp -> Int -> z3 FuncEntry
funcInterpGetEntry = liftFun2 Base.funcInterpGetEntry

-- | Return the 'else' value of the given function interpretation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46de7559826ba71b8488d727cba1fb64>
funcInterpGetElse :: MonadZ3 z3 => FuncInterp -> z3 AST
funcInterpGetElse = liftFun1 Base.funcInterpGetElse

-- | Return the arity (number of arguments) of the given function
-- interpretation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaca22cbdb6f7787aaae5d814f2ab383d8>
funcInterpGetArity :: MonadZ3 z3 => FuncInterp -> z3 Int
funcInterpGetArity = liftFun1 Base.funcInterpGetArity

-- | Return the value of this point.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga9fd65e2ab039aa8e40608c2ecf7084da>
funcEntryGetValue :: MonadZ3 z3 => FuncEntry -> z3 AST
funcEntryGetValue = liftFun1 Base.funcEntryGetValue

-- | Return the number of arguments in a Z3_func_entry object.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga51aed8c5bc4b1f53f0c371312de3ce1a>
funcEntryGetNumArgs :: MonadZ3 z3 => FuncEntry -> z3 Int
funcEntryGetNumArgs = liftFun1 Base.funcEntryGetNumArgs

-- | Return an argument of a Z3_func_entry object.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6fe03fe3c824fceb52766a4d8c2cbeab>
funcEntryGetArg :: MonadZ3 z3 => FuncEntry -> Int -> z3 AST
funcEntryGetArg = liftFun2 Base.funcEntryGetArg

-- | Convert the given model into a string.
modelToString :: MonadZ3 z3 => Model -> z3 String
modelToString = liftFun1 Base.modelToString

-- | Alias for 'modelToString'.
showModel :: MonadZ3 z3 => Model -> z3 String
showModel = modelToString

-------------------------------------------------
-- ** Helpers

-- | Type of an evaluation function for 'AST'.
--
-- Evaluation may fail (i.e. return 'Nothing') for a few
-- reasons, see 'modelEval'.
type EvalAst m a = Model -> AST -> m (Maybe a)

-- | An alias for 'modelEval' with model completion enabled.
eval :: MonadZ3 z3 => EvalAst z3 AST
eval = liftFun2 Base.eval

-- | Evaluate an AST node of sort /bool/ in the given model.
--
-- See 'modelEval' and 'getBool'.
evalBool :: MonadZ3 z3 => EvalAst z3 Bool
evalBool = liftFun2 Base.evalBool

-- | Evaluate an AST node of sort /int/ in the given model.
--
-- See 'modelEval' and 'getInt'.
evalInt :: MonadZ3 z3 => EvalAst z3 Integer
evalInt = liftFun2 Base.evalInt

-- | Evaluate an AST node of sort /real/ in the given model.
--
-- See 'modelEval' and 'getReal'.
evalReal :: MonadZ3 z3 => EvalAst z3 Rational
evalReal = liftFun2 Base.evalReal

-- | Evaluate an AST node of sort /bit-vector/ in the given model.
--
-- The flag /signed/ decides whether the bit-vector value is
-- interpreted as a signed or unsigned integer.
--
-- See 'modelEval' and 'getBv'.
evalBv :: MonadZ3 z3 => Bool -- ^ signed?
                     -> EvalAst z3 Integer
evalBv = liftFun3 Base.evalBv

-- | Evaluate a collection of AST nodes in the given model.
evalT :: (MonadZ3 z3,Traversable t) => Model -> t AST -> z3 (Maybe (t AST))
evalT = liftFun2 Base.evalT

-- | Run a evaluation function on a 'Traversable' data structure of 'AST's
-- (e.g. @[AST]@, @Vector AST@, @Maybe AST@, etc).
--
-- This a generic version of 'evalT' which can be used in combination with
-- other helpers. For instance, @mapEval evalInt@ can be used to obtain
-- the 'Integer' interpretation of a list of 'AST' of sort /int/.
mapEval :: (MonadZ3 z3, Traversable t) => EvalAst z3 a
                                       -> Model
                                       -> t AST
                                       -> z3 (Maybe (t a))
mapEval f m = fmap T.sequence . T.mapM (f m)

-- | Get function as a list of argument/value pairs.
evalFunc :: MonadZ3 z3 => Model -> FuncDecl -> z3 (Maybe FuncModel)
evalFunc = liftFun2 Base.evalFunc

---------------------------------------------------------------------
-- Tactics

mkTactic :: MonadZ3 z3 => String -> z3 Tactic
mkTactic = liftFun1 Base.mkTactic

andThenTactic :: MonadZ3 z3 => Tactic -> Tactic -> z3 Tactic
andThenTactic = liftFun2 Base.andThenTactic

orElseTactic :: MonadZ3 z3 => Tactic -> Tactic -> z3 Tactic
orElseTactic = liftFun2 Base.andThenTactic

skipTactic :: MonadZ3 z3 => z3 Tactic
skipTactic = liftScalar Base.skipTactic

tryForTactic :: MonadZ3 z3 => Tactic -> Int -> z3 Tactic
tryForTactic = liftFun2 Base.tryForTactic

mkQuantifierEliminationTactic :: MonadZ3 z3 => z3 Tactic
mkQuantifierEliminationTactic = liftScalar Base.mkQuantifierEliminationTactic

mkAndInverterGraphTactic :: MonadZ3 z3 => z3 Tactic
mkAndInverterGraphTactic = liftScalar Base.mkAndInverterGraphTactic

applyTactic :: MonadZ3 z3 => Tactic -> Goal -> z3 ApplyResult
applyTactic = liftFun2 Base.applyTactic

getApplyResultNumSubgoals :: MonadZ3 z3 => ApplyResult -> z3 Int
getApplyResultNumSubgoals = liftFun1 Base.getApplyResultNumSubgoals

getApplyResultSubgoal :: MonadZ3 z3 => ApplyResult -> Int -> z3 Goal
getApplyResultSubgoal = liftFun2 Base.getApplyResultSubgoal

getApplyResultSubgoals :: MonadZ3 z3 => ApplyResult -> z3 [Goal]
getApplyResultSubgoals = liftFun1 Base.getApplyResultSubgoals

mkGoal :: MonadZ3 z3 => Bool -> Bool -> Bool -> z3 Goal
mkGoal = liftFun3 Base.mkGoal

goalAssert :: MonadZ3 z3 => Goal -> AST -> z3 ()
goalAssert = liftFun2 Base.goalAssert

getGoalSize :: MonadZ3 z3 => Goal -> z3 Int
getGoalSize = liftFun1 Base.getGoalSize

getGoalFormula :: MonadZ3 z3 => Goal -> Int -> z3 AST
getGoalFormula = liftFun2 Base.getGoalFormula

getGoalFormulas :: MonadZ3 z3 => Goal -> z3 [AST]
getGoalFormulas = liftFun1 Base.getGoalFormulas

---------------------------------------------------------------------
-- String Conversion

-- | Set the mode for converting expressions to strings.
setASTPrintMode :: MonadZ3 z3 => ASTPrintMode -> z3 ()
setASTPrintMode = liftFun1 Base.setASTPrintMode

-- | Convert an AST to a string.
astToString :: MonadZ3 z3 => AST -> z3 String
astToString = liftFun1 Base.astToString

-- | Convert a pattern to a string.
patternToString :: MonadZ3 z3 => Pattern -> z3 String
patternToString = liftFun1 Base.patternToString

-- | Convert a sort to a string.
sortToString :: MonadZ3 z3 => Sort -> z3 String
sortToString = liftFun1 Base.sortToString

-- | Convert a FuncDecl to a string.
funcDeclToString :: MonadZ3 z3 => FuncDecl -> z3 String
funcDeclToString = liftFun1 Base.funcDeclToString

-- | Convert the given benchmark into SMT-LIB formatted string.
--
-- The output format can be configured via 'setASTPrintMode'.
benchmarkToSMTLibString :: MonadZ3 z3 =>
                               String   -- ^ name
                            -> String   -- ^ logic
                            -> String   -- ^ status
                            -> String   -- ^ attributes
                            -> [AST]    -- ^ assumptions1
                            -> AST      -- ^ formula
                            -> z3 String
benchmarkToSMTLibString = liftFun6 Base.benchmarkToSMTLibString


---------------------------------------------------------------------
-- Parser interface

-- | Parse SMT expressions from a string
--
-- The sort and declaration arguments allow parsing in a context in which variables and functions have already been declared. They are almost never used.
parseSMTLib2String :: MonadZ3 z3 =>
                      String     -- ^ string to parse
                   -> [Symbol]   -- ^ sort names
                   -> [Sort]     -- ^ sorts
                   -> [Symbol]   -- ^ declaration names
                   -> [FuncDecl] -- ^ declarations
                   -> z3 AST
parseSMTLib2String = liftFun5 Base.parseSMTLib2String

-- | Parse SMT expressions from a file
--
-- The sort and declaration arguments allow parsing in a context in which variables and functions have already been declared. They are almost never used.
parseSMTLib2File :: MonadZ3 z3 =>
                    String     -- ^ string to parse
                 -> [Symbol]   -- ^ sort names
                 -> [Sort]     -- ^ sorts
                 -> [Symbol]   -- ^ declaration names
                 -> [FuncDecl] -- ^ declarations
                 -> z3 AST
parseSMTLib2File = liftFun5 Base.parseSMTLib2File

getParserError :: MonadZ3 z3 => z3 String
getParserError = liftScalar Base.getParserError

---------------------------------------------------------------------
-- Miscellaneous

-- | Return Z3 version number information.
getVersion :: MonadZ3 z3 => z3 Version
getVersion = liftIO Base.getVersion

---------------------------------------------------------------------
-- Fixedpoint

class MonadZ3 m => MonadFixedpoint m where
  getFixedpoint :: m Base.Fixedpoint

fixedpointPush :: MonadFixedpoint z3 => z3 ()
fixedpointPush = liftFixedpoint0 Base.fixedpointPush

fixedpointPop :: MonadFixedpoint z3 => z3 ()
fixedpointPop = liftFixedpoint0 Base.fixedpointPush

fixedpointAddRule :: MonadFixedpoint z3 => AST -> Symbol -> z3 ()
fixedpointAddRule = liftFixedpoint2 Base.fixedpointAddRule

fixedpointSetParams :: MonadFixedpoint z3 => Params -> z3 ()
fixedpointSetParams = liftFixedpoint1 Base.fixedpointSetParams

fixedpointRegisterRelation :: MonadFixedpoint z3 => FuncDecl -> z3 ()
fixedpointRegisterRelation = liftFixedpoint1 Base.fixedpointRegisterRelation

fixedpointQueryRelations :: MonadFixedpoint z3 => [FuncDecl] -> z3 Result
fixedpointQueryRelations = liftFixedpoint1 Base.fixedpointQueryRelations

fixedpointGetAnswer :: MonadFixedpoint z3 => z3 AST
fixedpointGetAnswer = liftFixedpoint0 Base.fixedpointGetAnswer

fixedpointGetAssertions :: MonadFixedpoint z3 => z3 [AST]
fixedpointGetAssertions = liftFixedpoint0 Base.fixedpointGetAssertions

---------------------------------------------------------------------
-- * Interpolation

mkInterpolant :: MonadZ3 z3 => AST -> z3 AST
mkInterpolant = liftFun1 Base.mkInterpolant

getInterpolant :: MonadZ3 z3 => AST -> AST -> Params -> z3 [AST]
getInterpolant = liftFun3 Base.getInterpolant

computeInterpolant :: MonadZ3 z3 => AST -> Params
                   -> z3 (Maybe (Either Model [AST]))
computeInterpolant = liftFun2 Base.computeInterpolant

readInterpolationProblem :: MonadZ3 z3 => FilePath -> z3 (Either String Base.InterpolationProblem)
readInterpolationProblem = liftFun1 Base.readInterpolationProblem

checkInterpolant :: MonadZ3 z3 => Base.InterpolationProblem -> [AST] -> z3 (Result, Maybe String)
checkInterpolant = liftFun2 Base.checkInterpolant

interpolationProfile :: MonadZ3 z3 => z3 String
interpolationProfile = liftScalar Base.interpolationProfile

writeInterpolationProblem :: MonadZ3 z3 => FilePath -> Base.InterpolationProblem -> z3 ()
writeInterpolationProblem = liftFun2 Base.writeInterpolationProblem

---------------------------------------------------------------------
-- * Solvers

-- mkSolver :: Context -> IO Solver
-- mkSolver = liftFun0 z3_mk_solver

-- mkSimpleSolver :: Context -> IO Solver
-- mkSimpleSolver = liftFun0 z3_mk_simple_solver

-- mkSolverForLogic :: Context -> Logic -> IO Solver
-- mkSolverForLogic c logic = withContextError c $ \cPtr ->
--   do sym <- mkStringSymbol c (show logic)
--      c2h c =<< z3_mk_solver_for_logic cPtr (unSymbol sym)

-- | Return a string describing all solver available parameters.
solverGetHelp :: MonadZ3 z3 => z3 String
solverGetHelp = liftSolver0 Base.solverGetHelp

-- | Set the solver using the given parameters.
solverSetParams :: MonadZ3 z3 => Params -> z3 ()
solverSetParams = liftSolver1 Base.solverSetParams

-- | Create a backtracking point.
solverPush :: MonadZ3 z3 => z3 ()
solverPush = liftSolver0 Base.solverPush

-- | Backtrack /n/ backtracking points.
solverPop :: MonadZ3 z3 => Int -> z3 ()
solverPop = liftSolver1 Base.solverPop

solverReset :: MonadZ3 z3 => z3 ()
solverReset = liftSolver0 Base.solverReset

-- | Number of backtracking points.
solverGetNumScopes :: MonadZ3 z3 => z3 Int
solverGetNumScopes = liftSolver0 Base.solverGetNumScopes

-- | Assert a constraing into the logical context.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1a05ff73a564ae7256a2257048a4680a>
solverAssertCnstr :: MonadZ3 z3 => AST -> z3 ()
solverAssertCnstr = liftSolver1 Base.solverAssertCnstr

-- | Assert a constraint a into the solver, and track it
-- (in the unsat) core using the Boolean constant /p/.
--
-- This API is an alternative to Z3_solver_check_assumptions
-- for extracting unsat cores. Both APIs can be used in the same
-- solver. The unsat core will contain a combination of the Boolean
-- variables provided using Z3_solver_assert_and_track and the
-- Boolean literals provided using Z3_solver_check_assumptions.
solverAssertAndTrack :: MonadZ3 z3 => AST -> AST -> z3 ()
solverAssertAndTrack = liftSolver2 Base.solverAssertAndTrack

-- | Check whether the assertions in a given solver are consistent or not.
solverCheck :: MonadZ3 z3 => z3 Result
solverCheck = liftSolver0 Base.solverCheck

-- | Check whether the assertions in the given solver and optional assumptions are consistent or not.
solverCheckAssumptions :: MonadZ3 z3 => [AST] -> z3 Result
solverCheckAssumptions = liftSolver1 Base.solverCheckAssumptions

-- | Retrieve the model for the last 'solverCheck'.
--
-- The error handler is invoked if a model is not available because
-- the commands above were not invoked for the given solver,
-- or if the result was 'Unsat'.
solverGetModel :: MonadZ3 z3 => z3 Model
solverGetModel = liftSolver0 Base.solverGetModel

-- | Retrieve the unsat core for the last 'solverCheckAssumptions'; the unsat core is a subset of the assumptions
solverGetUnsatCore :: MonadZ3 z3 => z3 [AST]
solverGetUnsatCore = liftSolver0 Base.solverGetUnsatCore

-- | Return a brief justification for an 'Unknown' result for the commands 'solverCheck' and 'solverCheckAssumptions'.
solverGetReasonUnknown :: MonadZ3 z3 => z3 String
solverGetReasonUnknown = liftSolver0 Base.solverGetReasonUnknown

-- | Convert the given solver into a string.
solverToString :: MonadZ3 z3 => z3 String
solverToString = liftSolver0 Base.solverToString

-------------------------------------------------
-- ** Helpers

-- | Create a backtracking point.
--
-- For @push; m; pop 1@ see 'local'.
push :: MonadZ3 z3 => z3 ()
push = solverPush

-- | Backtrack /n/ backtracking points.
--
-- Contrary to 'solverPop' this funtion checks whether /n/ is within
-- the size of the solver scope stack.
pop :: MonadZ3 z3 => Int -> z3 ()
pop n = do
  scopes <- solverGetNumScopes
  if n <= scopes
    then solverPop n
    else error "Z3.Monad.safePop: too many scopes to backtrack"

-- | Run a query and restore the initial logical context.
--
-- This is a shorthand for 'push', run the query, and 'pop'.
local :: MonadZ3 z3 => z3 a -> z3 a
local q = do
  push
  r <- q
  pop 1
  return r

-- | Backtrack all the way.
reset :: MonadZ3 z3 => z3 ()
reset = solverReset

-- | Get number of backtracking points.
getNumScopes :: MonadZ3 z3 => z3 Int
getNumScopes = liftSolver0 Base.solverGetNumScopes

assert :: MonadZ3 z3 => AST -> z3 ()
assert = solverAssertCnstr

-- | Check whether the given logical context is consistent or not.
check :: MonadZ3 z3 => z3 Result
check = solverCheck

-- | Check whether the assertions in the given solver and optional assumptions are consistent or not.
checkAssumptions :: MonadZ3 z3 => [AST] -> z3 Result
checkAssumptions = solverCheckAssumptions

solverCheckAndGetModel :: MonadZ3 z3 => z3 (Result, Maybe Model)
solverCheckAndGetModel = liftSolver0 Base.solverCheckAndGetModel

-- | Get model.
--
-- Reference : <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaff310fef80ac8a82d0a51417e073ec0a>
getModel :: MonadZ3 z3 => z3 (Result, Maybe Model)
getModel = solverCheckAndGetModel

-- | Check satisfiability and, if /sat/, compute a value from the given model.
--
-- E.g.
-- @
-- withModel $ \\m ->
--   fromJust \<$\> evalInt m x
-- @
withModel :: (Applicative z3, MonadZ3 z3) =>
                (Base.Model -> z3 a) -> z3 (Result, Maybe a)
withModel f = do
 (r,mb_m) <- getModel
 mb_e <- T.traverse f mb_m
 return (r, mb_e)

-- | Retrieve the unsat core for the last 'checkAssumptions'; the unsat core is a subset of the assumptions.
getUnsatCore :: MonadZ3 z3 => z3 [AST]
getUnsatCore = solverGetUnsatCore
