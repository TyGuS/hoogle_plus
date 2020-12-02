
module Datalog.Datalog where

import Database.Environment
import Database.Utils
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Filtering
import Types.Type
import Types.IOFormat
import Types.Program
import Synquid.Type
import Synquid.Program
import PetriNet.Utils
import HooglePlus.Utils
import HooglePlus.GHCChecker
import HooglePlus.IOFormat

import Control.Monad.Logic
import Control.Monad.State
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Debug.Trace

enumeratePath :: SearchParams -> Environment -> SchemaSkeleton -> [Example] -> UProgram -> LogicT IO ()
enumeratePath params env goal examples prog = do
    let gm = env ^. symbolGroups
    let getFuncs p = Map.findWithDefault Set.empty p gm
    let foArgs = map fst (foArgsOf env)
    let syms = Set.toList (symbolsOf prog) \\ foArgs
    let allPaths = map (Set.toList . getFuncs) syms
    msum $ map (\path ->
        let subst = Map.fromList (zip syms path)
         in checkPath params env goal examples (recoverNames subst prog)) (sequence allPaths)

checkPath :: SearchParams -> Environment -> SchemaSkeleton -> [Example] -> UProgram -> LogicT IO ()
checkPath params env goal examples prog = do
    -- ensure the usage of all arguments
    let args = map fst (env ^. arguments)
    let getRealName = replaceId hoPostfix ""
    let filterPaths p = all (`Set.member` Set.map getRealName (symbolsOf p)) args
    liftIO $ print prog
    guard (filterPaths prog)

    (checkResult, _) <- liftIO $
        runStateT (check env params examples prog goal) emptyFilterState
    maybe mzero (liftIO . (toOutput env prog >=> (printResult . encodeWithPrefix))) checkResult
