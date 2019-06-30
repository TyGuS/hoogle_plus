{-# LANGUAGE DeriveDataTypeable#-}
module Types.Encoder where

import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Z3.Base as Z3
import Z3.Monad hiding(Z3Env, newEnv)
import Control.Monad.State
import Data.Data
import Data.Typeable
import GHC.Generics
import qualified Language.Haskell.Exts.Syntax as HSE
import Data.Function

import Types.Common
import Types.Abstract

data EncoderType = Normal | Arity
    deriving(Eq, Show, Data, Typeable)

data VarType = VarPlace | VarTransition | VarFlow | VarTimestamp
    deriving(Eq, Ord, Show)

data FunctionCode = FunctionCode {
  funName   :: String,  -- function name
  hoParams  :: [FunctionCode],
  funParams :: [AbstractSkeleton], -- function parameter types and their count
  funReturn :: [AbstractSkeleton]   -- function return type
}

instance Eq FunctionCode where
  fc1 == fc2 = let
    areEq arg = on (==) arg fc1 fc2
    in areEq hoParams && areEq funParams && areEq funReturn

instance Ord FunctionCode where
  compare fc1 fc2 = let
    thenCmp EQ       ordering = ordering
    thenCmp ordering _        = ordering
    cmp arg = on compare arg fc1 fc2
    in foldr1 thenCmp [cmp hoParams, cmp funParams, cmp funReturn]

data Z3Env = Z3Env {
  envSolver  :: Z3.Solver,
  envContext :: Z3.Context,
  envOptimize :: Z3.Optimize
}

data EncodeState = EncodeState {
  z3env :: Z3Env,
  counter :: Int,
  block :: Z3.AST,
  loc :: Int,
  transitionNb :: Int,
  variableNb :: Int,
  place2variable :: HashMap (Id, Int) Int, -- place name and timestamp
  time2variable :: HashMap Int Int, -- timestamp and abstraction level
  transition2id :: HashMap Id Int, -- transition name and abstraction level
  id2transition :: HashMap Int Id,
  mustFirers :: HashMap Id [Id],
  ty2tr :: HashMap Id [Id],
  prevChecked :: Bool,
  incrementalSolving :: Bool,
  disabledTrans :: [Id],
  returnTyps :: [Id],
  persistConstraints :: [Z3.AST],
  optionalConstraints :: [Z3.AST],
  finalConstraints :: [Z3.AST],
  blockConstraints :: [Z3.AST]
}

emptyEncodeState = EncodeState {
  z3env = undefined,
  counter = 0,
  block = undefined,
  loc = 0,
  transitionNb = 0,
  variableNb = 1,
  place2variable = HashMap.empty,
  time2variable = HashMap.empty,
  transition2id = HashMap.empty,
  id2transition = HashMap.empty,
  mustFirers = HashMap.empty,
  ty2tr = HashMap.empty,
  prevChecked = False,
  incrementalSolving = False,
  disabledTrans = [],
  returnTyps = [],
  persistConstraints = [],
  optionalConstraints = [],
  finalConstraints = [],
  blockConstraints = []
}

newEnv :: Maybe Logic -> Opts -> IO Z3Env
newEnv mbLogic opts =
  Z3.withConfig $ \cfg -> do
    setOpts cfg opts
    ctx <- Z3.mkContext cfg
    opt <- Z3.mkOptimize ctx
    solver <- maybe (Z3.mkSolver ctx) (Z3.mkSolverForLogic ctx) mbLogic
    return $ Z3Env solver ctx opt

initialZ3Env = newEnv Nothing stdOpts

freshEnv :: Z3.Context -> IO Z3Env
freshEnv ctx = 
  Z3.withConfig $ \cfg -> do
    setOpts cfg stdOpts
    solver <- Z3.mkSolver ctx
    opt <- Z3.mkOptimize ctx
    return $ Z3Env solver ctx opt

type Encoder = StateT EncodeState IO
