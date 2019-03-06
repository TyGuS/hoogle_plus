{-# LANGUAGE FlexibleContexts #-}

module Synquid.Graph where

import Synquid.Type hiding (set)
import Synquid.Util
import Synquid.Logic
import Synquid.Program
import Synquid.Pretty
import Database.Util
import Types.Abstract

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Serialize (Serialize)
import Data.Aeson
import qualified Data.Serialize as S
import Control.Monad.State
import Control.Applicative hiding (empty)
import Control.Lens
import Debug.Trace
import qualified Data.Text as Text
import qualified Data.Vector as Vector

type NameCount = Map Id Int

-- | Replace all bound type variables with fresh free variables
instantiate :: (Monad s) => RSchema -> StateT NameCount s RType
instantiate sch = instantiate' Map.empty sch
  where
    instantiate' subst (ForallT a sch) = do
      a' <- freshId "A"
      instantiate' (Map.insert a (vart a' (BoolLit True)) subst) sch
    instantiate' subst (Monotype t) = return $ typeSubstitute subst $ t
    freshId pre = do
      nameCnt <- get
      let cnt = Map.findWithDefault 0 pre nameCnt
      put $ Map.insert pre (cnt+1) nameCnt
      return $ pre ++ show cnt

instance (Eq k, Hashable k, Serialize k, Serialize v) => Serialize (HashMap k v) where
  put hm = S.put (HashMap.toList hm)
  get = HashMap.fromList <$> S.getListOf S.get

instance Serialize Formula
instance Serialize Sort
instance Serialize UnOp
instance Serialize BinOp
instance Serialize Environment
instance Serialize PredSig
instance Serialize DatatypeDef
instance Serialize MeasureCase
instance Serialize MeasureDef
instance Serialize Metadata
instance Serialize t => Serialize (Case t)
instance Serialize t => Serialize (BareProgram t)
instance Serialize t => Serialize (Program t)
instance Serialize r => Serialize (TypeSkeleton r)
instance Serialize r => Serialize (BaseType r)
instance Serialize r => Serialize (SchemaSkeleton r)
instance Serialize AbstractSkeleton
