module Interpreter.Imports (
--     addPhantomModule
--   , newImportList
) where

import Data.List
import qualified GHC
import System.Random
-- import System.FilePath
import System.Directory

import Interpreter.Session

data PhantomModule = PhantomModule {
    pmName :: ModuleName
  , pmFile :: FilePath
} deriving (Eq, Show)

-- newPhantomModule :: MonadInterpreter m => m PhantomModule
-- newPhantomModule = do
--     n <- liftIO randomIO
--     (ls, is) <- lift getContextNames
--     let nums = concat [show (abs n::Int), filter isDigit $ concat (ls ++ is)]
--     let modName = 'M':nums
--     -- tmpDir <- getPhantomDirectory
--     --
--     return PhantomModule{pmName = modName, pmFile = modName <.> "hs"}
--     where
--         name = GHC.moduleNameString . GHC.moduleName
--         decl = GHC.moduleNameString . GHC.unLoc . GHC.ideclName

--         getContextNames :: GHC.GhcMonad m => m ([String], [String])
--         getContextNames = fmap (map name *** map decl) getContext

-- addPhantomModule :: MonadInterpreter m => (ModuleName -> String) -> m PhantomModule
-- addPhantomModule toModText = do
--     pm <- newPhantomModule
--     -- let fileTarget = GHC.Target (GHC.TargetFile (pmFile pm) $ Just (GHC.Cpp GHC.HsSrcFile)) True Nothing
--     -- let mdlName = GHC.mkModuleName (pmName pm)
--     -- --
--     -- liftIO $ writeFile (pmFile pm) (mod_text $ pmName pm)
--     -- --
--     return pm

newImportList :: ModuleImport -> ModuleName
newImportList (ModuleImport name qual imList) = unwords ["import", qualName, importStr]
    where
        qualName = case qual of
            NotQualified -> name
            ImportAs q -> unwords [name, "as", q]
            QualifiedAs Nothing -> unwords ["qualified", name]
            QualifiedAs (Just q) -> unwords ["qualified", name, "as", q]

        importStr = case imList of
            NoImportList -> ""
            ImportList l -> "(" ++ intercalate "," l ++ ")"
            HidingList l -> "hiding (" ++ intercalate "," l ++ ")"