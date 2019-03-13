module Database.Environment(writeEnv, generateEnv) where

import Data.Either
import Data.Serialize (encode)
import Data.List.Extra
import Control.Lens ((^.))
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (evalStateT)
import System.Exit (exitFailure)
import Text.Parsec.Pos (initialPos)

import Synquid.Error (Pos(Pos))
import Synquid.Logic (ftrue)
import Synquid.Type (BaseType(..), TypeSkeleton(..), SchemaSkeleton(..), isHigherOrder, toMonotype)
import Synquid.Pretty as Pretty
import qualified Database.Util as DU
import qualified Database.Download as DD
import qualified Database.Convert as DC
import Types.Environment (Environment, symbols, _symbols, _included_modules)
import Synquid.Program (BareDeclaration(..), Declaration(..), ConstructorSig(..))
import Synquid.Resolver (resolveDecls)


writeEnv :: Environment -> String -> IO ()
writeEnv env path = B.writeFile path (encode env)

generateEnv :: [DU.PkgName] -> [String] -> Int -> Bool -> IO Environment
generateEnv pkgs mdls depth useHO = do
  -- print pkgs
  pkgDecls <- mapM (\pkgName -> do
    DD.downloadFile pkgName Nothing >> DD.downloadCabal pkgName Nothing
    declMap <- DC.readDeclarations pkgName Nothing
    let fileDecls = concatMap (\mdl -> Map.findWithDefault [] mdl declMap) mdls
    parsedDecls <- mapM (\decl -> evalStateT (DC.toSynquidDecl decl) 0) fileDecls
    dependsPkg <- DC.packageDependencies pkgName True
    dependsDecls <- concatMap (concat . Map.elems) <$> (mapM (flip DC.readDeclarations Nothing) $ nub dependsPkg)
    additionalDts <- DC.declDependencies pkgName fileDecls dependsDecls >>= mapM (flip evalStateT 0 . DC.toSynquidDecl)
    return $ additionalDts ++ parsedDecls
    ) pkgs
  let decls = DC.reorderDecls $ nub $ defaultFuncs ++ defaultDts ++ concat pkgDecls
  case resolveDecls decls of
    Left resolutionError -> (pdoc $ pretty resolutionError) >> pdoc empty >> exitFailure
    Right (env, _, _, _) -> do
      return env {
          _symbols = if useHO then env ^. symbols
                              else Map.map (Map.filter (not . isHigherOrder . toMonotype)) $ env ^. symbols,
         _included_modules = Set.fromList mdls
        }
  where
    pdoc = putStrLn . show


-- Default Library
defaultFuncs = [ Pos (initialPos "fst") $ FuncDecl "fst" (Monotype (FunctionT "p" (ScalarT (DatatypeT "Pair" [ScalarT (TypeVarT Map.empty "a") ftrue, ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue) (ScalarT (TypeVarT Map.empty "a") ftrue)))
                , Pos (initialPos "snd") $ FuncDecl "snd" (Monotype (FunctionT "p" (ScalarT (DatatypeT "Pair" [ScalarT (TypeVarT Map.empty "a") ftrue, ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue) (ScalarT (TypeVarT Map.empty "b") ftrue)))
                ]

defaultDts = [defaultList, defaultPair, defaultUnit, defaultInt, defaultBool]

defaultInt = Pos (initialPos "Int") $ DataDecl "Int" [] [] []

defaultBool = Pos (initialPos "Bool") $ DataDecl "Bool" [] [] []

defaultList = Pos (initialPos "List") $ DataDecl "List" ["a"] [] [
    ConstructorSig "Nil"  $ ScalarT (DatatypeT "List" [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue
  , ConstructorSig "Cons" $ FunctionT "x" (ScalarT (TypeVarT Map.empty "a") ftrue) (FunctionT "xs" (ScalarT (DatatypeT "List" [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue) (ScalarT (DatatypeT "List" [ScalarT (TypeVarT Map.empty "a") ftrue] []) ftrue))
  ]

defaultPair = Pos (initialPos "Pair") $ DataDecl "Pair" ["a", "b"] [] [
    ConstructorSig "Pair" $ FunctionT "x" (ScalarT (TypeVarT Map.empty "a") ftrue) (FunctionT "y" (ScalarT (TypeVarT Map.empty "b") ftrue) (ScalarT (DatatypeT "Pair" [ScalarT (TypeVarT Map.empty "a") ftrue, ScalarT (TypeVarT Map.empty "b") ftrue] []) ftrue))
  ]

defaultUnit = Pos (initialPos "Unit") $ DataDecl "Unit" [] [] []
