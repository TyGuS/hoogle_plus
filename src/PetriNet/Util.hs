{-# LANGUAGE FlexibleContexts #-} 
module PetriNet.Util where

import Control.Concurrent.Chan ( writeChan )
import Control.Lens ( (^.), view, over )
import Control.Monad.State ( foldM, MonadIO(..), gets, modify )
import Data.List.Extra ( (\\), splitOn )
import Data.Maybe ( fromJust, fromMaybe )
import qualified Data.Text as Text
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Text.Printf ( printf )
import Text.PrettyPrint.ANSI.Leijen ( plain, string, Doc, Pretty(pretty) )

import qualified Hoogle

import Types.Common
import Types.Solver
import Types.Experiments
import Types.Environment
import Types.Encoder
import Types.IOFormat
import Types.Filtering (AssociativeExamples)
import HooglePlus.Utils
import Utility.Utils
import Types.Program
import Types.Pretty


getExperiment exp = gets $ view (searchParams . exp)

-------------------------------------------------------------------------------
-- | helper functions
-------------------------------------------------------------------------------



toOutput :: Environment -> RProgram -> AssociativeExamples -> IO QueryOutput
toOutput env soln exs = do
    let symbols = Set.toList $ symbolsOf soln
    let args = getArguments env
    let argNames = map fst args
    let argDocs = map (\(n, ty) -> FunctionDoc (Text.unpack n) (show ty) "") args
    let symbolsWoArgs = symbols \\ argNames
    entries <- mapM mkEntry exs
    return $ QueryOutput entries "" argDocs
    where
        mkEntry ((unqualSol, qualSol), ex) = do
            ex' <- mapM niceInputs ex
            let qualSol' = toHaskellSolution $ show qualSol
            let unqualSol' = toHaskellSolution $ show unqualSol
            return (ResultEntry qualSol' unqualSol' ex')
        hoogleIt syms = do
            dbPath <- Hoogle.defaultDatabaseLocation
            Hoogle.withDatabase dbPath (\db -> do
                let targets = map (head . Hoogle.searchDatabase db) syms
                let docs = map targetToDoc targets
                return docs)

        targetToDoc tg = let wholeSig = unHTML $ Hoogle.targetItem tg
                             segs = splitOn " :: " wholeSig
                             name = head segs
                             sig = unwords $ tail segs
                             doc = unHTML $ Hoogle.targetDocs tg
                          in FunctionDoc name sig doc