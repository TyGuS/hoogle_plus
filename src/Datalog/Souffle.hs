module Datalog.Souffle 
    ( runSouffle
    , writeSouffle
    ) where

import Datalog.Datalog
import Datalog.DatalogType
import Datalog.SouffleType
import Datalog.Utils
import Database.Environment
import Types.Common
import Types.Experiments
import Types.Environment
import Types.Type
import Types.IOFormat
import Types.Program
import HooglePlus.Utils
import Synquid.Type
import Synquid.Pretty

import Control.Lens
import Control.Monad.Logic
import qualified Data.Map as Map
import System.Process
import System.IO
import System.Directory
import Text.Read
import Text.Printf

soufflePreamble :: [String]
soufflePreamble =   [ ".decl Nullary(t: symbol)"
                    , ".decl Unary(t: symbol)"
                    , ".decl Binary(t: symbol)"
                    , ".decl TyApp(dt: symbol, arg: symbol, t: symbol, k: number, d: number)"
                    , "TyApp(X, \"\", X, 0, 0) :- Nullary(X)."
                    , "TyApp(X, Y, cat(X, cat(\" \", Y)), 0, D+1) :- Unary(X), TyApp(_, _, Y, 0, D), D <= 4."
                    , "TyApp(X, Y, cat(X, cat(\" \", Y)), 1, D+1) :- Binary(X), TyApp(_, _, Y, 0, D), D <= 4."
                    , "TyApp(T, Z, cat(T, cat(\" \", Z)), 0, D+1) :- Binary(X), TyApp(X, _, T, 1, D1), TyApp(_, _, Z, 0, D2), D1 + D2 = D, D <= 4."
                    , ".decl program(t: symbol, p: symbol, d: number)"
                    , ".decl Inh(x: symbol, t: symbol)"
                    , ".decl IsGoal(t: symbol)"
                    , ".decl IsGoalDepth(d: number)"
                    , ".decl Query(p: symbol)"
                    , ".output Query"
                    , "Query(P) :- Program(T, P, D), IsGoal(T), IsGoalDepth(D)."
                    , "Program(T, X, 0) :- Inh(X, T)."
                    ]

runSouffle :: SearchParams -> Environment -> SchemaSkeleton -> [Example] -> Int -> LogicT IO ()
runSouffle params env goal examples d = do
    paths <- liftIO $ findPath env goal d
    liftIO $ print paths
    ifte (msum $ map (enumeratePath params env goal examples) paths)
         return
         (runSouffle params env goal examples (d + 1))

findPath :: Environment -> SchemaSkeleton -> Int -> IO [UProgram]
findPath env goal d = do
    -- get higher-order arguments
    let args = map (over _2 toMonotype) (env ^. arguments)
    let hoArgs = filter (isFunctionType . snd) args
    let hoArgSat = map (uncurry writeFunctionSouffle) hoArgs
    -- write query into the file
    let dstTyp = varToDatatype (lastType (toMonotype goal))
    error "not implemented"

writeSouffle :: Environment -> IO ()
writeSouffle env =
    -- write datalog templates
    withFile "./data/souffle/input.dl" WriteMode $ \hdl -> do
        -- write preamble above
        mapM_ (hPutStrLn hdl) soufflePreamble
        -- write function definitions
        mapM_ (hPutStrLn hdl . uncurry writeFunctionSouffle) (Map.toList $ env ^. groups)

writeFunctionSouffle :: Id -> TypeSkeleton -> String
writeFunctionSouffle name t =
    let funcStr = writeFunction "Program(%s, cat(cat(\"(\", cat(\"%s\", %s)), \")\"), %s)"
                                (foldr (printf "cat(\" \", cat(%s, %s))") "\"\"")
                                SoufflePack name t
     in printf "//%s :: %s\n%s" name (show t) funcStr
