{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module PetriNet.PolyDispatcher (
      dispatch
    , DispatchState(DispatchState)
    , resultList
    ) where

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
    liftIO $ print typs
    let typList = Set.toList typs
    state <- get
    -- get new RSchema for each input symbol
    let symbols = state ^. functionList 
    -- let perms  = [ []
    --              , [[e] | e <- typList]
    --              , nub $ [[e1,e2] | e1 <- typList, e2 <- typList]
    --              , nub $ [[e1,e2,e3] | e1 <- typList, e2 <- typList, e3 <- typList]
    --              , nub $ [[e1,e2,e3,e4] | e1 <- typList, e2 <- typList, e3 <- typList, e4 <- typList]
    --              ]
    symbols' <- foldM (\acc -> (<$>) (Map.union acc) . uncurry (dispatchType typs)) Map.empty $ Map.toList symbols
    -- store the temporary result to our state
    modify (over resultList $ Map.union symbols')
    
    state' <- get
    -- liftIO $ print $ state' ^. usedTypes
    -- iteratively compute for new added types
    let typs' = Set.map (addTrue . shape) $ Map.foldr (Set.union . Set.fromList . allBaseTypes) Set.empty symbols'
    modify (over usedTypes $ Set.union typs')
    when (typs' /= state ^. usedTypes)
         (dispatch $ Set.filter ((>) (state ^. tdBoundary) . typeDepth) $ Set.union typs' (state' ^. usedTypes))

-- This does not work to generate all the possible instantiations for prelude functions
-- Is there any better way to do this?
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
        -- liftIO $ print t
        return $ Map.insert newId t accMap) Map.empty substedSymbols
    where
        multiPermutation len elmts | len == 0 = []
        multiPermutation len elmts | len == 1 = [[e] | e <- elmts]
        multiPermutation len elmts            = nub $ [ l:r | l <- elmts, r <- multiPermutation (len - 1) elmts]
        newSymbolName prefix = do
            indices <- flip (^.) functionIdx <$> get
            let idx = Map.findWithDefault 0 prefix indices
            modify (over functionIdx $ Map.insert prefix (idx+1))
            return $ prefix ++ "_" ++ show idx

