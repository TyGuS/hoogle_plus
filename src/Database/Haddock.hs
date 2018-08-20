{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Database.Haddock where

-- for current haddock test
import qualified Data.Map as Map
import Documentation.Haddock (processModules, withGhc, Interface(..), Documentation(..))
import Documentation.Haddock.Types (MetaDoc(..), DocH(..), Meta(..))
import Outputable (ppr, showSDocUnsafe, Outputable)
import qualified Outputable as O
import HsDecls hiding (DataDecl)
import SrcLoc
import HsTypes
import HsBinds
import HsDecls
import HsExtension
import Text.Parsec.Pos
import Distribution.Verbosity
import Control.Monad.State
-- import Distribution.PackageDescription

import Database.Download
import Database.Util
import Synquid.Logic
import Synquid.Type
import Synquid.Pretty
import Synquid.Error
import Synquid.Program (emptyEnv, BareDeclaration, Declaration, Environment)
import qualified Synquid.Program as SP

isTypeSig :: LHsDecl GhcRn -> Bool
isTypeSig (L _ (SigD {})) = True
isTypeSig _ = False

namesOf (HsFunTy (L _ fun) (L _ ret)) = namesOf fun ++ namesOf ret
namesOf (HsTyVar _ (L _ id)) = [id]
namesOf (HsAppTy (L _ fun) (L _ arg)) = namesOf fun ++ namesOf arg
namesOf (HsParTy (L _ ty)) = namesOf ty
namesOf _ = []

toSynquidSkeleton :: Monad m => HsType GhcRn -> StateT Int m RType
toSynquidSkeleton (HsForAllTy _ (L _ ty)) = toSynquidSkeleton ty -- type boundaries here
toSynquidSkeleton (HsQualTy _ (L _ ty)) = toSynquidSkeleton ty -- type qualifiers here
toSynquidSkeleton (HsTyVar _ (L _ id)) = return $ ScalarT (TypeVarT Map.empty $ showSDocUnsafe $ ppr id) ftrue
toSynquidSkeleton (HsAppTy (L _ fun) (L _ arg)) | HsAppTy _ _ <- fun = do
    ScalarT (DatatypeT id ts _) _ <- toSynquidSkeleton fun
    (\t -> ScalarT (DatatypeT id (ts ++ [t]) []) ftrue) <$> toSynquidSkeleton arg
toSynquidSkeleton (HsAppTy (L _ fun) (L _ arg)) | HsTyVar _ (L _ id) <- fun =
    (\t -> ScalarT (DatatypeT (showSDocUnsafe $ ppr id) [t] []) ftrue) <$> toSynquidSkeleton arg
toSynquidSkeleton (HsFunTy (L _ fun) (L _ ret)) = do
    modify (1 +)
    liftM3 FunctionT (((++) "arg" . show) <$> get) (toSynquidSkeleton fun) (toSynquidSkeleton ret)
toSynquidSkeleton (HsListTy (L _ ty)) = (\t -> ScalarT (DatatypeT ("List") [t] []) ftrue) <$> toSynquidSkeleton ty
toSynquidSkeleton (HsTupleTy _ tys) | length tys == 0 = return $ ScalarT (DatatypeT "Unit" [] []) ftrue
toSynquidSkeleton (HsTupleTy _ tys) | length tys == 2 = 
    (\ts -> ScalarT (DatatypeT ("Pair") ts []) ftrue) <$> mapM (\(L _ t) -> toSynquidSkeleton t) tys
toSynquidSkeleton (HsTupleTy s tys) | length tys  > 2 = 
    (\ts -> ScalarT (DatatypeT ("Pair") ts []) ftrue) <$> sequence [(\(L _ t) -> toSynquidSkeleton t) (head tys), toSynquidSkeleton (HsTupleTy s $ tail tys)]
toSynquidSkeleton t@(HsTupleTy _ tys) | otherwise = 
    error $ "Tuple type " ++ (showSDocUnsafe $ ppr t) ++ " should have at least two types between parentheses"
toSynquidSkeleton (HsParTy (L _ ty)) = toSynquidSkeleton ty
toSynquidSkeleton (HsKindSig (L _ ty) _) = toSynquidSkeleton ty
toSynquidSkeleton (HsDocTy (L _ ty) _) = toSynquidSkeleton ty
toSynquidSkeleton ty = return AnyT

-- helper functions for conversion to synquid declaractions
toStrName (L _ name) = showSDocUnsafe $ ppr name
toStrVars vars = map outOfBound $ hsq_explicit vars
outOfBound (L _ bnd) = case bnd of
    UserTyVar (L _ id) -> showSDocUnsafe $ ppr id
    KindedTyVar (L _ id) _ -> showSDocUnsafe $ ppr id

outOfLoc (L _ a) = a

-- convert the result from GHC to the declarations in Synquid
toSynquidDecl :: Monad m => LHsDecl GhcRn -> StateT Int m Declaration
-- type signatures (the normal one, ignore the others for now)
toSynquidDecl (L _ (SigD (TypeSig ids (HsWC _ (HsIB _ (L _ ty) _))))) = 
    Pos (initialPos $ toStrName $ head ids) . SP.FuncDecl (toStrName $ head ids) . Monotype <$> toSynquidSkeleton ty
-- type synonyms
toSynquidDecl (L _ (TyClD (SynDecl name vars _ (L _ rhs) _))) = 
    Pos (initialPos $ toStrName name) . SP.TypeDecl (toStrName name) (toStrVars vars) <$> toSynquidSkeleton rhs
-- data declarations
toSynquidDecl (L _ (TyClD (DataDecl name vars _ def _ _))) = 
    Pos (initialPos $ toStrName name) . SP.DataDecl (toStrName name) (toStrVars vars) [] <$> getConstructors
  where
    getConstructors = mapM toConstructor $ dd_cons def
    processConDetail (PrefixCon args) = liftM3 FunctionT (((++) "arg" . show) <$> get) 
                                                         (toSynquidSkeleton . outOfLoc $ head args) 
                                                         (processConDetail (PrefixCon $ tail args))
    processConDetail (RecCon (L a recs)) = liftM3 FunctionT (((++) "arg" . show) <$> get) 
                                                            (toSynquidSkeleton . outOfLoc . cd_fld_type . outOfLoc $ head recs) 
                                                            (processConDetail (RecCon (L a $ tail recs)))
    processConDetail (InfixCon (L _ arg0) (L _ arg1)) = liftM3 FunctionT (((++) "arg" . show) <$> get) 
                                                                         (toSynquidSkeleton arg0) 
                                                                         (toSynquidSkeleton arg1) 
    toConstructor (L _ conDecl) = case conDecl of
        ConDeclGADT names (HsIB _ (L _ typ) _) _ -> SP.ConstructorSig (toStrName $ head names) <$> toSynquidSkeleton typ
        ConDeclH98 name _ _ arg _ -> SP.ConstructorSig (toStrName name) <$> processConDetail arg
-- class declarations
toSynquidDecl (L a (TyClD (decl@(ClassDecl {})))) = 
    Pos (initialPos $ toStrName $ tcdLName decl) . SP.ClassDecl (toStrName $ tcdLName decl) (toStrVars $ tcdTyVars decl) 
    <$> (mapM (toSynquidDecl . L a . SigD . outOfLoc) $ tcdSigs decl) -- we have a list of declarations here

-- for debug
tyNames (L _ (SigD (TypeSig ids (HsWC _ (HsIB _ (L _ ty) _))))) = namesOf ty
-- TyClD (TyClDecl id)   Type or Class Declaration
tyNames _ = ([])

typeOf (L _ (SigD (TypeSig ids (HsWC _ (HsIB _ (L _ ty) _))))) = ty

parseSigs :: PkgName ->Maybe Version -> IO ()
parseSigs pkg version = do
    mdls <- downloadPkgSource pkg version
    let dsts = map (\mdl -> downloadDir ++ mdl ++ ".hs") mdls
    (interfaces, linkEnv) <- withGhc [] $ processModules verbose dsts [] []
    res <- mapM (\ifaces -> do
        let elems = Map.elems $ ifaceDeclMap ifaces
        mapM (\decls -> do
            let sigs = filter isTypeSig decls
            mapM (\decl -> do
                typ <- evalStateT (toSynquidSkeleton (typeOf decl)) 0
                return (decl, map (\n -> (n, Map.lookup n linkEnv)) $ tyNames decl, typ)) sigs) elems) interfaces
    writeFile "test.log" $ showSDocUnsafe $ ppr res

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