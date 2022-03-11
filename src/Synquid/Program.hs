{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveGeneric #-}

-- | Executable programs
module Synquid.Program where

import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Hashable
import Data.Maybe
import Data.Either
import Data.List
import Data.List.Extra
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import GHC.Generics hiding (to)

import Control.Monad
import Control.Lens

import Database.Prelude
import Synquid.Type
import Synquid.Error
import Synquid.Tokens
import Synquid.Util
import Types.Common
import Types.Type
import Types.Program
import Types.Environment
