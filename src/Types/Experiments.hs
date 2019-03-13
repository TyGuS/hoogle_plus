{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Types.Experiments where

import Synquid.Type
import Synquid.Program
import Synquid.Error
import Synquid.Util
import qualified PetriNet.PNSolver as PNSolver
import qualified HooglePlus.Encoder as HEncoder

-- import Control.Monad.List
import Data.Data
import Control.Lens hiding (index, indices)

{- Interface -}

-- | Choices for the type of path search
data PathStrategy =
  MaxSAT -- ^ Use SMT solver to find a path
  | PetriNet -- ^ Use PetriNet and SyPet
  | PNSMT -- ^ Use PetriNet and SMT solver
  deriving (Eq, Show, Data)

-- | Parameters of program exploration
data SearchParams = SearchParams {
  _eGuessDepth :: Int,                    -- ^ Maximum depth of application trees
  _sourcePos :: SourcePos,                -- ^ Source position of the current goal
  _explorerLogLevel :: Int,               -- ^ How verbose logging is
  _solutionCnt :: Int,
  _pathSearch :: PathStrategy,
  _useHO :: Bool,
  _encoderType :: HEncoder.EncoderType,
  _useRefine :: PNSolver.RefineStrategy
}

makeLenses ''SearchParams
