{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Database.Haddock where

-- for current haddock test
import qualified Data.Map as Map
import Documentation.Haddock (processModules, withGhc, Interface(..), Documentation(..), LinkEnv, DocOption(..))
import Documentation.Haddock.Types (MetaDoc(..), DocH(..), Meta(..))
import Outputable (ppr, showSDocUnsafe, Outputable)
import qualified Outputable as O
import HsDecls hiding (DataDecl)
import SrcLoc
import HsTypes
import HsBinds
import HsDecls
import HsExtension
import Module
import Text.Parsec.Pos
import Distribution.Verbosity
import Control.Monad.State
import Debug.Trace
import Data.List
import Data.Maybe
-- import Distribution.PackageDescription

import Database.Download
import Database.Util
import Synquid.Util
import Synquid.Logic
import Synquid.Type
import Synquid.Pretty
import Synquid.Error
import Synquid.Program (emptyEnv, BareDeclaration, Declaration, Environment)
import qualified Synquid.Program as SP

isTypeSig :: LHsDecl GhcRn -> Bool
isTypeSig (L _ (SigD {})) = True
isTypeSig _ = False

isTyDecl :: LHsDecl GhcRn -> Bool
isTyDecl (L _ (TyClD {})) = True
isTyDecl _ = False

namesOf (HsFunTy (L _ fun) (L _ ret)) = namesOf fun ++ namesOf ret
namesOf (HsTyVar _ (L _ id)) = [id]
namesOf (HsAppTy (L _ fun) (L _ arg)) = namesOf fun ++ namesOf arg
namesOf (HsParTy (L _ ty)) = namesOf ty
namesOf _ = []

toSynquidSkeleton :: MonadIO m => LinkEnv -> HsType GhcRn -> StateT Int m RType
toSynquidSkeleton linkEnv (HsForAllTy _ (L _ ty)) = toSynquidSkeleton linkEnv ty -- type boundaries here
toSynquidSkeleton linkEnv (HsQualTy _ (L _ ty)) = toSynquidSkeleton linkEnv ty -- type qualifiers here
toSynquidSkeleton linkEnv (HsTyVar _ id) = return $ ScalarT (TypeVarT Map.empty $ toStrName linkEnv id) ftrue
toSynquidSkeleton linkEnv (HsAppTy (L _ fun) (L _ arg)) | HsAppTy _ _ <- fun = do
    ScalarT (DatatypeT id ts _) _ <- toSynquidSkeleton linkEnv fun
    (\t -> ScalarT (DatatypeT id (ts ++ [t]) []) ftrue) <$> toSynquidSkeleton linkEnv arg
toSynquidSkeleton linkEnv (HsAppTy (L _ fun) (L _ arg)) | HsTyVar _ (L _ id) <- fun =
    (\t -> ScalarT (DatatypeT (showSDocUnsafe $ ppr id) [t] []) ftrue) <$> toSynquidSkeleton linkEnv arg
toSynquidSkeleton linkEnv (HsFunTy (L _ fun) (L _ ret)) = do
    modify (1 +)
    liftM3 FunctionT (((++) "arg" . show) <$> get) (toSynquidSkeleton linkEnv fun) (toSynquidSkeleton linkEnv ret)
toSynquidSkeleton linkEnv (HsListTy (L _ ty)) = (\t -> ScalarT (DatatypeT ("List") [t] []) ftrue) <$> toSynquidSkeleton linkEnv ty
toSynquidSkeleton linkEnv (HsTupleTy _ tys) | length tys == 0 = return $ ScalarT (DatatypeT "Unit" [] []) ftrue
toSynquidSkeleton linkEnv (HsTupleTy _ tys) | length tys == 2 = 
    (\ts -> ScalarT (DatatypeT ("Pair") ts []) ftrue) <$> mapM (\(L _ t) -> toSynquidSkeleton linkEnv t) tys
toSynquidSkeleton linkEnv (HsTupleTy s tys) | length tys  > 2 = 
    (\ts -> ScalarT (DatatypeT ("Pair") ts []) ftrue) <$> sequence [(\(L _ t) -> toSynquidSkeleton linkEnv t) (head tys), toSynquidSkeleton linkEnv (HsTupleTy s $ tail tys)]
toSynquidSkeleton linkEnv t@(HsTupleTy _ tys) | otherwise = 
    error $ "Tuple type " ++ (showSDocUnsafe $ ppr t) ++ " should have at least two types between parentheses"
toSynquidSkeleton linkEnv (HsParTy (L _ ty)) = toSynquidSkeleton linkEnv ty
toSynquidSkeleton linkEnv (HsKindSig (L _ ty) _) = toSynquidSkeleton linkEnv ty
toSynquidSkeleton linkEnv (HsDocTy (L _ ty) _) = toSynquidSkeleton linkEnv ty
toSynquidSkeleton linkEnv (HsBangTy _ (L _ ty)) = toSynquidSkeleton linkEnv ty
toSynquidSkeleton linkEnv ty = return AnyT

-- helper functions for conversion to synquid declaractions
moduleStr env name = case Map.lookup name env of
    Just mdl -> (moduleNameString $ moduleName $ mdl) ++ "."
    Nothing -> ""
toStrName env (L _ name) = (moduleStr env name) ++ (showSDocUnsafe $ ppr name)
toStrVars env vars = map (outOfBound env) $ hsq_explicit vars
outOfBound env (L _ bnd) = case bnd of
    UserTyVar (L _ id) -> (moduleStr env id) ++ (showSDocUnsafe $ ppr id)
    KindedTyVar (L _ id) _ -> (moduleStr env id) ++ (showSDocUnsafe $ ppr id)
outOfLoc (L _ a) = a

-- convert the result from GHC to the declarations in Synquid
toSynquidDecl :: MonadIO m => LinkEnv -> LHsDecl GhcRn -> StateT Int m Declaration
-- type signatures (the normal one, ignore the others for now)
toSynquidDecl linkEnv (L _ (SigD (TypeSig ids (HsWC _ (HsIB _ (L _ ty) _))))) = 
    Pos (initialPos $ toStrName linkEnv $ head ids) . SP.FuncDecl (toStrName linkEnv $ head ids) . Monotype <$> toSynquidSkeleton linkEnv ty
-- type signatures (the class method)
toSynquidDecl linkEnv (L _ (SigD (ClassOpSig _ ids (HsIB _ (L _ ty) _)))) = 
    Pos (initialPos $ toStrName linkEnv $ head ids) . SP.FuncDecl (toStrName linkEnv $ head ids) . Monotype <$> toSynquidSkeleton linkEnv ty
-- type synonyms
toSynquidDecl linkEnv (L _ (TyClD (SynDecl name vars _ (L _ rhs) _))) = 
    Pos (initialPos $ toStrName linkEnv name) . SP.TypeDecl (toStrName linkEnv name) (toStrVars linkEnv vars) <$> toSynquidSkeleton linkEnv rhs
-- data declarations
toSynquidDecl linkEnv (L _ (TyClD (DataDecl name vars _ def _ _))) = 
    Pos (initialPos $ toStrName linkEnv name) . SP.DataDecl (toStrName linkEnv name) (toStrVars linkEnv vars) [] <$> getConstructors
  where
    getConstructors = mapM toConstructor $ dd_cons def
    processConDetail (PrefixCon args) = 
        if length args == 0
            then return $ ScalarT (DatatypeT (toStrName linkEnv name) (map (flip ScalarT ftrue . TypeVarT Map.empty) $ toStrVars linkEnv vars) []) ftrue
            else liftM3 FunctionT (modify (1 +) >> ((++) "arg" . show) <$> get) 
                         (toSynquidSkeleton linkEnv . outOfLoc $ head args) 
                         (processConDetail (PrefixCon $ tail args))
    processConDetail (RecCon (L a recs)) = 
        if length recs == 0
            then return $ ScalarT (DatatypeT (toStrName linkEnv name) (map (flip ScalarT ftrue . TypeVarT Map.empty) $ toStrVars linkEnv vars) []) ftrue
            else liftM3 FunctionT (modify (1 +) >> ((++) "arg" . show) <$> get) 
                         (toSynquidSkeleton linkEnv . outOfLoc . cd_fld_type . outOfLoc $ head recs) 
                         (processConDetail (RecCon (L a $ tail recs)))
    processConDetail (InfixCon (L _ arg0) (L _ arg1)) = liftM3 FunctionT (modify (1 +) >> ((++) "arg" . show) <$> get) 
                                                                         (toSynquidSkeleton linkEnv arg0) 
                                                                         (toSynquidSkeleton linkEnv arg1) 
    toConstructor (L _ conDecl) = case conDecl of
        ConDeclGADT names (HsIB _ (L _ typ) _) _ -> SP.ConstructorSig (toStrName linkEnv $ head names) <$> toSynquidSkeleton linkEnv typ
        ConDeclH98 name _ _ arg _ -> SP.ConstructorSig (toStrName linkEnv name) <$> processConDetail arg
-- class declarations
toSynquidDecl linkEnv (L a (TyClD (decl@(ClassDecl {})))) = 
    Pos (initialPos $ toStrName linkEnv $ tcdLName decl) . SP.ClassDecl (toStrName linkEnv $ tcdLName decl) (toStrVars linkEnv $ tcdTyVars decl) 
    <$> (mapM (toSynquidDecl linkEnv . L a . SigD . outOfLoc) $ tcdSigs decl) -- we have a list of declarations here
toSynquidDecl _ decl = return $ Pos (initialPos "") $ SP.MutualDecl [showSDocUnsafe $ ppr decl] -- [TODO] a fake conversion

-- for debug
tyNames (L _ (SigD (TypeSig ids (HsWC _ (HsIB _ (L _ ty) _))))) = namesOf ty
-- TyClD (TyClDecl id)   Type or Class Declaration
tyNames _ = ([])

typeOf (L _ (SigD (TypeSig ids (HsWC _ (HsIB _ (L _ ty) _))))) = ty

declName (SP.FuncDecl id _) = id
declName (SP.TypeDecl id _ _) = id
declName (SP.DataDecl id _ _ _) = id
declName (SP.ClassDecl id _ _) = id
declName _ = ""

getMethodName = reverse . takeWhile ((/=) '.') . reverse

renameDecl mdl (SP.FuncDecl id ty) = SP.FuncDecl (mdl ++ "." ++ getMethodName id) ty
renameDecl mdl (SP.TypeDecl id var ty) = SP.TypeDecl (mdl ++ "." ++ getMethodName id) var ty
renameDecl mdl (SP.DataDecl id var preds cons) = SP.DataDecl (mdl ++ "." ++ getMethodName id) var preds cons
renameDecl mdl (SP.ClassDecl id var mtds) = SP.ClassDecl (mdl ++ "." ++ getMethodName id) var mtds
renameDecl _ decl = decl

outOfPos (Pos _ decl) = decl
withPos f (Pos p decl) = Pos p (f decl)

parseSigs :: PkgName ->Maybe Version -> IO ()
parseSigs pkg version = do
    (mdls, expMdls) <- downloadPkgSource pkg version
    traceShow (plain . text $ show expMdls) $ return ()
    let dsts = map (\mdl -> downloadDir ++ mdl ++ ".hs") mdls
    (interfaces, linkEnv) <- withGhc [] $ processModules verbose dsts [] []
    res <- mapM (\iface -> do
        traceShow (plain . text . moduleNameString . moduleName $ ifaceMod iface) $ return ()
        traceShow (plain . text . show . map (showSDocUnsafe . ppr) $ ifaceExports iface) $ return ()
        -- traceShow (plain . text . show . map (showSDocUnsafe . ppr) $ ifaceOptions iface) $ return ()
        let mdl = moduleNameString . moduleName $ ifaceMod iface
        let elems = Map.elems $ ifaceDeclMap iface
        let exportedNames = map (showSDocUnsafe . ppr) $ ifaceExports iface
        mapM (\decls -> do
            let sigs = filterOr isTypeSig isTyDecl decls
            -- traceShow (plain $ text $ showSDocUnsafe $ ppr decls) $ return ()
            convertedDecls <- mapM (flip evalStateT 0 . toSynquidDecl linkEnv) sigs
            -- traceShow (plain $ text $ show $ map ((reverse . takeWhile ((/=) '.') . reverse) . declName . outOfPos) convertedDecls) $ return ()
            let exportedDecls = filter (flip elem exportedNames . getMethodName . declName . outOfPos) convertedDecls
            return $ map (withPos $ renameDecl mdl) exportedDecls
            ) elems
        ) $ filter (\iface -> (elem (moduleNameString . moduleName $ ifaceMod iface) expMdls) 
                              && (not $ elem OptHide $ ifaceOptions iface)) $ mergeHiddenModules interfaces linkEnv
            
                -- typ <- evalStateT (toSynquidSkeleton linkEnv (typeOf decl)) 0
                -- return (decl, map (\n -> (n, Map.lookup n linkEnv)) $ tyNames decl, typ)) sigs) elems) interfaces
    writeFile "test.log" $ showSDocUnsafe . ppr $ nub . concat . concat $ res
  where
    mergeHiddenModules interfaces linkEnv = foldr (\iface newIfaces -> 
        let exportedNames = ifaceExports iface
            modules = nub $ map fromJust . filter isJust $ map (flip Map.lookup linkEnv) exportedNames
            declsOf mdl = ifaceDeclMap $ head $ filter ((==) mdl . ifaceMod) interfaces
        in (iface { ifaceDeclMap = Map.unions (map declsOf modules) }) : newIfaces
        ) [] interfaces

-- | Useful for debugging
instance Outputable Meta where
  ppr (Meta v pkg) = ppr pkg O.<+> O.text "-" O.<+> ppr v

instance Outputable RType where
  ppr = O.text . show

instance (Outputable m, Outputable id) => Outputable (DocH m id) where
  ppr (DocModule m) = ppr m
  ppr (DocIdentifier id) = ppr id
  ppr (DocString str) = ppr str
  ppr (DocEmpty) = O.empty
  ppr (DocAppend a b) = ppr a O.<+> ppr b
  ppr (DocCodeBlock d) = ppr d
  ppr (DocWarning w) = O.text "warning"
  ppr (DocParagraph p) = O.text "paragraph"
  ppr (DocIdentifierUnchecked m) = O.text "idUnchecked"
  ppr (DocEmphasis emp) = O.text "emphasis"
  ppr (DocMonospaced ms) = O.text "monospaced"
  ppr (DocBold _) = O.text "bold"
  ppr (DocUnorderedList _) = O.text "unordered"
  ppr (DocOrderedList _) = O.text "ordered"
  ppr (DocDefList _) = O.text "defList"
  ppr (DocHyperlink _) = O.text "Hyperlink"
  ppr (DocPic _) = O.text "pic"
  ppr (DocMathInline _) = O.text "DocMathInline"
  ppr (DocMathDisplay _) = O.text "DocMathDisplay"
  ppr (DocAName _) = O.text "DocAName"
  ppr (DocProperty _) = O.text "DocProperty"
  ppr (DocExamples _) = O.text "DocExamples"
  ppr (DocHeader _) = O.text "DocHeader"
  ppr (DocTable _) = O.text "DocTable"

instance (Outputable a, Outputable b) => Outputable (MetaDoc a b) where
  ppr (MetaDoc a b)= ppr a O.<+> ppr b

instance Outputable a => Outputable (Documentation a) where
  ppr (Documentation doc w) = ppr doc

instance Outputable Declaration where
  ppr (Pos _ decl) = O.text $ show decl

instance Outputable DocOption where
  ppr OptHide = O.text "OptHide"
  ppr OptPrune = O.text "OptPrune"
  ppr OptIgnoreExports = O.text "OptIgnoreExports"
  ppr OptNotHome = O.text "OptNotHome"
  ppr OptShowExtensions = O.text "OptShowExtensions"