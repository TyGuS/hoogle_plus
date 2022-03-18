{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Encoder where

import           Control.Lens
import           Control.Monad.State            ( StateT )
import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Language.Haskell.Exts.Syntax  as HSE
import qualified Z3.Base                       as Z3
import           Z3.Monad                       ( Logic
                                                , Opts
                                                , setOpts
                                                , stdOpts
                                                )

import           Types.Common
import           Types.Experiments
import           Types.Type


data EncodedFunction = EncodedFunction
  { funName   :: Id -- function name
  , funParams :: [TypeSkeleton] -- function parameter types
  , funReturn :: [TypeSkeleton]   -- function return type
  }

instance Eq EncodedFunction where
  EncodedFunction _ params1 rets1 == EncodedFunction _ params2 rets2 =
    params1 == params2 && rets1 == rets2

instance Ord EncodedFunction where
  compare (EncodedFunction _ params1 rets1) (EncodedFunction _ params2 rets2) =
    compare params1 params2 <> compare rets1 rets2

data Z3Env = Z3Env
  { envSolver  :: Z3.Solver
  , envContext :: Z3.Context
  }
  deriving Eq

data Constraints = Constraints
  { _persistConstraints  :: [Z3.AST]
  , _optionalConstraints :: [Z3.AST]
  , _finalConstraints    :: [Z3.AST]
  , _blockConstraints    :: [Z3.AST]
  }
  deriving Eq

emptyConstraints :: Constraints
emptyConstraints = Constraints { _persistConstraints  = []
                               , _optionalConstraints = []
                               , _finalConstraints    = []
                               , _blockConstraints    = []
                               }

makeLenses ''Constraints

data EncodeVariables = EncodeVariables
  { _transitionNb    :: Int
  , _variableNb      :: Int
  , _place2variable  :: Map (TypeSkeleton, Int) Z3.AST -- place name and timestamp
  , _time2variable   :: Map Int Z3.AST -- timestamp and abstraction level
  , _transition2id   :: Map Id Z3.AST -- transition name and abstraction level
  , _id2transition   :: Map Int Id
  , _type2transition :: Map TypeSkeleton (Set Id)
  }
  deriving Eq

emptyVariables :: EncodeVariables
emptyVariables = EncodeVariables { _transitionNb    = 0
                                 , _variableNb      = 1
                                 , _place2variable  = Map.empty
                                 , _time2variable   = Map.empty
                                 , _transition2id   = Map.empty
                                 , _id2transition   = Map.empty
                                 , _type2transition = Map.empty
                                 }

makeLenses ''EncodeVariables

data SolverMode = Enumeration | FreshSearch
  deriving ( Eq, Show )

data EncodeState = EncodeState
  { _z3env           :: Z3Env
  , _encSearchParams :: SearchParams
  , _variables       :: EncodeVariables
  , _constraints     :: Constraints
  , _block           :: Z3.AST
  , _prevChecked     :: Bool
  , _pathLength      :: Int
  , _mustFirers      :: Map Id [Id]
  , _disabledTrans   :: [Id]
  , _returnTyps      :: [TypeSkeleton]
  , _currentMode     :: SolverMode
  }
  deriving Eq

emptyEncodeState :: EncodeState
emptyEncodeState = EncodeState { _z3env           = undefined
                               , _encSearchParams = defaultSearchParams
                               , _variables       = emptyVariables
                               , _constraints     = emptyConstraints
                               , _block           = undefined
                               , _prevChecked     = False
                               , _pathLength      = 1
                               , _mustFirers      = Map.empty
                               , _disabledTrans   = []
                               , _returnTyps      = []
                               , _currentMode     = FreshSearch
                               }

makeLenses ''EncodeState

newEnv :: Maybe Logic -> Opts -> IO Z3Env
newEnv mbLogic opts = Z3.withConfig $ \cfg -> do
  setOpts cfg opts
  ctx    <- Z3.mkContext cfg
  solver <- maybe (Z3.mkSolver ctx) (Z3.mkSolverForLogic ctx) mbLogic
  return $ Z3Env solver ctx

initialZ3Env :: IO Z3Env
initialZ3Env = newEnv Nothing stdOpts

freshEnv :: Z3.Context -> IO Z3Env
freshEnv ctx = Z3.withConfig $ \cfg -> do
  setOpts cfg stdOpts
  solver <- Z3.mkSolver ctx
  return $ Z3Env solver ctx

type Encoder = StateT EncodeState IO
