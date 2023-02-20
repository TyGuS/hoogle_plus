module Dataset.BottomUp where

import Database.Dataset
import Types.Type
import Types.Program

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

type ProgramBank = Map Type (Set Program)

-- generate programs with at least two function calls
generate :: m (Set Program)
generate = evalStateT go (Map.fromList unaryPrograms)
  where
    go :: StateT ProgramBank m (Set Program)
    go = undefined

    (binary, unary) = partition (isFunctionT . toMonotype . snd) hplusComponents
    unaryPrograms = map (\(x, t) -> (toMonotype t, Program (PSymbol x) (toMonotype t))) unary

-- adjoin trees