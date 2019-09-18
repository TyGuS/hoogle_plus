{-# LANGUAGE DeriveDataTypeable#-}
module Types.Encoder where

import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Z3.Base as Z3
import Z3.Monad hiding(Z3Env, newEnv)
import Control.Monad.State
import Data.Data
import Data.Typeable
import GHC.Generics
import qualified Language.Haskell.Exts.Syntax as HSE


import Types.Common
import Types.Abstract

data EncoderType = Normal | Arity
    deriving(Eq, Show, Data, Typeable)

data VarType = VarPlace | VarTransition | VarFlow | VarTimestamp
    deriving(Eq, Ord, Show)

data Z3Env = Z3Env {
  envSolver  :: Z3.Solver,
  envContext :: Z3.Context
}

data EncodeState = EncodeState {
  z3env :: Z3Env,
  counter :: Int,
  block :: Z3.AST,
  loc :: Int,
  transitionNb :: Int,
  variableNb :: Int,
  place2variable :: HashMap (Int, Int) Z3.AST, -- place name and timestamp
  time2variable :: HashMap Int Z3.AST, -- timestamp and abstraction level
  transition2id :: HashMap Id Z3.AST, -- transition name and abstraction level
  id2transition :: HashMap Int Id,
  mustFirers :: HashMap Id [Id],
  ty2tr :: HashMap Int (Set Id),
  prevChecked :: Bool,
  incrementalSolving :: Bool,
  disabledTrans :: [Id],
  returnTyps :: [Int],
  persistConstraints :: [Z3.AST],
  fireConstraints :: HashMap Int (HashMap Id Z3.AST),
  optionalConstraints :: [Z3.AST],
  finalConstraints :: [Z3.AST],
  blockConstraints :: [Z3.AST],
  useArguments :: Bool,
  disableClones :: Bool
}

emptyEncodeState = EncodeState {
  z3env = undefined,
  counter = 0,
  block = undefined,
  loc = 1,
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
  fireConstraints = HashMap.empty,
  optionalConstraints = [],
  finalConstraints = [],
  blockConstraints = [],
  useArguments = True,
  disableClones = False
}

newEnv :: Opts -> IO Z3Env
newEnv opts =
  Z3.withConfig $ \cfg -> do
    setOpts cfg opts
    ctx <- Z3.mkContext cfg
    solver <- Z3.mkSolver ctx
    return $ Z3Env solver ctx

initialZ3Env = newEnv stdOpts

type Encoder = StateT EncodeState IO
