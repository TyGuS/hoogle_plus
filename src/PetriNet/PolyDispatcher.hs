{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module PetriNet.PolyDispatcher where

import Synquid.Program
import Synquid.Type
import Synquid.Util
import Synquid.Pretty
import Database.Convert

import Control.Monad.State
import Data.Foldable
import Control.Lens
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

data DispatchState = DispatchState {
    _tdBoundary :: Int,
    _functionList :: Map Id RSchema,
    _functionIdx :: Map Id Int,
    _usedTypes :: Set RType,
    _resultList :: Map Id RType
} deriving(Eq, Ord, Show)

makeLenses ''DispatchState

-- substitute the type variables in each function
dispatch :: (MonadPlus m, MonadIO m) => Set RType -> StateT DispatchState m ()
dispatch ts = do
    let typs = Set.map (addTrue . shape) ts
    when (Set.size typs > 0)
         (do
            liftIO $ print typs
            state <- get
            -- get new RSchema for each input symbol
            let symbols = state ^. functionList 
            symbols' <- foldM (\acc -> (<$>) (Map.union acc) . uncurry (dispatchType typs)) Map.empty $ Map.toList symbols
            -- store the temporary result to our state
            modify (over resultList $ Map.union symbols')
            modify (over usedTypes $ Set.union typs)
            state' <- get
            -- iteratively compute for new added types
            let typs' = Map.foldr (Set.union . Set.fromList . allBaseTypes) Set.empty symbols'
            when (not (typs' `Set.isSubsetOf` (state' ^. usedTypes)))
                 (dispatch $ Set.filter ((>=) (state ^. tdBoundary) . typeDepth) $ typs' `Set.difference` (state' ^. usedTypes))
            )

dispatchType :: (MonadPlus m, MonadIO m) => Set RType -> Id -> RSchema -> StateT DispatchState m (Map Id RType)
dispatchType typs id sch = do
    let rtyp           = toMonotype sch
    let vars           = boundVarsOf sch
    -- liftIO $ print vars
    -- liftIO $ print $ multiPermutation (length vars) typs
    let multiSubsts    = map (Map.fromList . zip vars) $ multiPermutation (length vars) (Set.toList typs)
    let substedSymbols = map (\subst -> typeSubstitute subst rtyp) multiSubsts
    -- liftIO $ print substedSymbols
    foldrM (\t accMap -> do
        newId <- newSymbolName id
        return $ Map.insert newId t accMap) Map.empty substedSymbols
    where
        multiPermutation len elmts | len == 0 = []
        multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
        multiPermutation len elmts            = [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]
        newSymbolName prefix = do
            indices <- flip (^.) functionIdx <$> get
            let idx = Map.findWithDefault 0 prefix indices
            modify (over functionIdx $ Map.insert prefix (idx+1))
            return $ prefix ++ "_" ++ show idx