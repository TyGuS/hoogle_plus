module Datalog.Souffle where

import Database.Environment
import Database.Util
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Filtering
import Types.Type
import Types.IOFormat
import Types.Program
import Synquid.Type
import Synquid.Program
import PetriNet.Util
import HooglePlus.Utils
import HooglePlus.GHCChecker
import HooglePlus.IOFormat

import Control.Monad.Logic
import Control.Monad.State
import Control.Lens
import Control.Concurrent.Chan
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe

enumeratePath :: SearchParams -> Environment -> RSchema -> [Example] -> UProgram -> LogicT IO ()
enumeratePath params env goal examples prog = do
    let gm = env ^. symbolGroups
    let getFuncs p = Map.findWithDefault Set.empty p gm
    let foArgs = Map.keys $ Map.filter (not . isFunctionType . toMonotype) (env ^. arguments)
    let syms = Set.toList (symbolsOf prog) \\ foArgs
    let allPaths = map (Set.toList . getFuncs) syms
    msum $ map (\path -> 
        let subst = Map.fromList (zip syms path)
         in checkPath params env goal examples (recoverNames subst prog)) (sequence allPaths)

checkPath :: SearchParams -> Environment -> RSchema -> [Example] -> UProgram -> LogicT IO ()
checkPath params env goal examples prog = do
    -- ensure the usage of all arguments
    let args = Map.keys (env ^. arguments)
    let getRealName = replaceId hoPostfix ""
    let filterPaths p = all (`Set.member` Set.map getRealName (symbolsOf p)) args
    guard (filterPaths prog)

    (checkResult, _) <- liftIO $ do
        msgChan <- newChan
        runStateT (check env params examples prog goal msgChan) emptyFilterState
    maybe mzero (\exs -> toOutput env prog exs >>= (printResult . encodeWithPrefix)) checkResult
