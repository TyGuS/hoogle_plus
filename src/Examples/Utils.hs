{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Examples.Utils where

import GHC hiding (Id)
import GHC.Paths
import qualified EnumSet as ES
import GHC.LanguageExtensions.Type
import HsUtils
import HsTypes
import TcRnDriver
import qualified Language.Haskell.Interpreter as LHI
import System.Timeout
import Text.Printf
import qualified Data.Map as Map
import Control.Exception
import Data.Char
import Outputable
import Control.Monad.IO.Class

import Types.Filtering (defaultTimeoutMicro, defaultDepth, defaultInterpreterTimeoutMicro, frameworkModules)
import Types.IOFormat
import Types.Type
import Database.Util
import Synquid.Logic
import HooglePlus.FilterTest (runInterpreter')

askInterpreter :: [String] -> String -> String -> IO (Either ErrorMessage String)
askInterpreter mdls preamble funcCall = do
    let progCall = printf "%s showCBResult <$> (CB.timeOutMicro' %d (CB.approxShow %d (%s)))" preamble defaultTimeoutMicro defaultDepth funcCall 
    catch (do
        result <- runInterpreter' defaultInterpreterTimeoutMicro $ do
            LHI.setImportsQ (zip mdls (repeat Nothing) ++ frameworkModules)
            r <- LHI.interpret progCall (LHI.as :: IO String) >>= liftIO
            return r
        print result
        case result of
          Left e -> return (Left $ show e)
          Right r -> return (Right r))
        (\(e :: SomeException) -> return (Left $ show e))

askGhc :: [String] -> Ghc a -> IO a
askGhc mdls f = do
    mbResult <- timeout (5*defaultTimeoutMicro) $ runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = dflags { 
            generalFlags = ES.delete Opt_OmitYields (generalFlags dflags),
            extensionFlags = ES.insert FlexibleContexts (extensionFlags dflags)
            }
        setSessionDynFlags dflags'
        prepareModules ("System.Timeout":"Prelude":mdls) >>= setContext
        f
    case mbResult of
        Just r -> return r
        Nothing -> error "timeout"
    where
        prepareModules mdls = do
            let imports = map (printf "import %s") mdls
            decls <- mapM parseImportDecl imports
            return (map IIDecl decls)

antiSubstitute :: SType -> Id -> SType -> SType
antiSubstitute pat name t | t == pat = vart_ name
antiSubstitute pat name (ScalarT (DatatypeT dt args _) _) = 
    ScalarT (DatatypeT dt (map (antiSubstitute pat name) args) []) ()
antiSubstitute pat name (FunctionT x tArg tRes) = FunctionT x tArg' tRes'
    where
        tArg' = antiSubstitute pat name tArg
        tRes' = antiSubstitute pat name tRes
antiSubstitute _ _ t = t

antiUnification :: SType -> SType -> IO SType
antiUnification t1 t2 = evalStateT (antiUnification' t1 t2) emptyAntiUnifState

antiUnification' :: SType -> SType -> AntiUnifier IO SType
antiUnification' AnyT t = return t
antiUnification' t AnyT = return t
antiUnification' t@(ScalarT (TypeVarT {}) _) (ScalarT (TypeVarT {}) _) = return t
antiUnification' (ScalarT (TypeVarT {}) _) t = return t
antiUnification' t (ScalarT (TypeVarT {}) _) = return t
antiUnification' t1@(ScalarT (DatatypeT dt1 args1 _) _) t2@(ScalarT (DatatypeT dt2 args2 _) _)
  | dt1 == dt2 = do
      args' <- mapM (uncurry antiUnification') (zip args1 args2)
      return $ ScalarT (DatatypeT dt1 args' []) ()
  | dt1 /= dt2 = do
      tass1 <- gets $ view typeAssignment1
      tass2 <- gets $ view typeAssignment2
      let overlap = (tass1 Map.! t1) `intersect` (tass2 Map.! t2)
      if t1 `Map.member` tass1 && t2 `Map.member` tass2 && not (null overlap)
         then if length overlap > 1 then error "antiUnficiation fails"
                                    else return $ vart_ (head overlap)
         else do 
             v <- freshId "a"
             modify $ over typeAssignment1 (Map.insertWith (++) t1 [v])
             modify $ over typeAssignment2 (Map.insertWith (++) t2 [v])
             return $ vart_ v
antiUnification' (FunctionT x1 tArg1 tRes1) (FunctionT x2 tArg2 tRes2) = do
    tArg <- antiUnification' tArg1 tArg2
    tRes <- antiUnification' tRes1 tRes2
    return $ FunctionT x1 tArg tRes

skipTyclass :: TypeSkeleton r -> TypeSkeleton r
skipTyclass (FunctionT x (ScalarT (DatatypeT name args _) _) tRes)
    | tyclassPrefix `isPrefixOf` name = skipTyclass tRes
skipTyclass t = t

seqChars = map (:[]) ['a'..'z']

integerToInt :: TypeSkeleton r -> TypeSkeleton r
integerToInt (ScalarT (DatatypeT dt args _) r) 
  | dt == "Integer" = ScalarT (DatatypeT "Int" (map integerToInt args) []) r
  | otherwise = ScalarT (DatatypeT dt (map integerToInt args) []) r 
integerToInt (FunctionT x tArg tRes) =
    FunctionT x (integerToInt tArg) (integerToInt tRes)
integerToInt t = t

wrapParens :: String -> String
wrapParens = printf "(%s)"
