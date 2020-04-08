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

resolveType :: LHsType GhcPs -> RSchema
resolveType (L _ (HsForAllTy bs t)) = foldr ForallT (resolveType t) vs
    where
        vs = map vname bs
        vname (L _ (UserTyVar (L _ id))) = showSDocUnsafe (ppr id)
        vname (L _ (KindedTyVar (L _ id) _)) = showSDocUnsafe (ppr id)
resolveType (L _ (HsFunTy f _)) = Monotype (resolveType' f)
resolveType (L _ (HsQualTy ctx body)) = Monotype bodyWithTcArgs
    where
        unlocatedCtx = let L _ c = ctx in c
        tyConstraints = map resolveType' unlocatedCtx

        prefixTc (ScalarT (DatatypeT name args rs) r) = ScalarT (DatatypeT (tyclassPrefix ++ name) args rs) r
        prefixTc t = error $ "Unhandled " ++ show t

        tcArgs = map prefixTc tyConstraints
        bodyWithTcArgs = foldr (FunctionT "") (resolveType' body) tcArgs
resolveType t = error (showSDocUnsafe $ ppr t)

resolveType' :: LHsType GhcPs -> RType
resolveType' (L _ (HsFunTy f r)) = FunctionT "" (resolveType' f) (resolveType' r)
resolveType' (L _ (HsQualTy _ t)) = resolveType' t
resolveType' (L _ (HsTyVar _ (L _ v))) = 
    if isLower (head name)
       then ScalarT (TypeVarT Map.empty name) ftrue
       else ScalarT (DatatypeT name [] []) ftrue
    where
        name = showSDocUnsafe $ ppr v
resolveType' t@(L _ HsAppTy{}) = ScalarT (DatatypeT dtName dtArgs []) ftrue
    where
        dtName = case datatypeOf t of
                   "[]" -> "List"
                   "(,)" -> "Pair"
                   n -> n
        dtArgs = datatypeArgs t

        datatypeOf (L _ (HsAppTy f _)) = datatypeOf f
        datatypeOf (L _ (HsTyVar _ (L _ v))) = showSDocUnsafe (ppr v)

        datatypeArgs (L _ (HsAppTy (L _ HsTyVar {}) a)) = [resolveType' a]
        datatypeArgs (L _ (HsAppTy f a)) = datatypeArgs f ++ datatypeArgs a
        datatypeArgs t = [resolveType' t]

resolveType' (L _ (HsListTy t)) = ScalarT (DatatypeT "List" [resolveType' t] []) ftrue
resolveType' (L _ (HsTupleTy _ ts)) = foldr mkPair basePair otherTyps
    where
        mkPair acc t = ScalarT (DatatypeT "Pair" [acc, t] []) ftrue
        resolveTyps = map resolveType' ts
        (baseTyps, otherTyps) = splitAt (length ts - 2) resolveTyps
        basePair = ScalarT (DatatypeT "Pair" baseTyps []) ftrue
resolveType' (L _ (HsParTy t)) = resolveType' t
resolveType' t = error $ showSDocUnsafe (ppr t)

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
