{-# LANGUAGE DeriveDataTypeable#-}
module Types.Encoder where

import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import qualified Z3.Base as Z3
import Z3.Monad hiding(Z3Env, newEnv)
import Control.Monad.State
import Data.Data
import Data.Typeable
import GHC.Generics

import Types.Common
import Types.Abstract

data EncoderType = Normal | Arity
    deriving(Eq, Show, Data, Typeable)

data VarType = VarPlace | VarTransition | VarFlow | VarTimestamp
    deriving(Eq, Ord, Show)

data FoldedFunction = FoldedFunction {
  funId :: Int, -- for transition id
  funPretokens :: [(Id, Int)], -- for construction of preconditions
  funPostokens :: [(Id, Int)]  -- for construction of postconditions
} deriving(Eq, Ord, Show)

data Variable = Variable {
  varId :: Int,
  varName :: String,
  varTimestamp :: Int,
  varValue :: Int,
  varType :: VarType
} deriving(Eq, Ord, Show)

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
  place2variable :: HashMap (Id, Int) Variable, -- place name and timestamp
  time2variable :: HashMap Int Variable, -- timestamp and abstraction level
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
