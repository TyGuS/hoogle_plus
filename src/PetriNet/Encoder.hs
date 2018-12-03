module PetriNet.Encoder where

import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Z3.Base as Z3
import Z3.Monad hiding(Z3Env, newEnv)

import PetriNet.PNBuilder
import Synquid.Util

type EncoderType = Either EncodeState EncodeSMTState

data VarType = VarPlace | VarTransition | VarFlow
    deriving(Eq, Ord, Show)

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
  petriNet :: PetriNet,
  loc :: Int,
  nbVariable :: Int,
  place2variable :: HashMap (Place, Int, Int) Variable,
  transition2variable :: HashMap (Transition, Int) Variable,
  id2variable :: HashMap Int Variable,
  mustFirers :: [Id]
}

data EncodeSMTState = EncodeSMTState {
  z3envSMT :: Z3Env,
  petriNetSMT :: PetriNet,
  locSMT :: Int,
  nbVariableSMT :: Int,
  place2variableSMT :: HashMap (Place, Int) Variable,
  transition2variableSMT :: HashMap (Transition, Int) Variable,
  id2variableSMT :: HashMap Int Variable,
  mustFirersSMT :: [Id]
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