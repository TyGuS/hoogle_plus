
module Database.Graph where

import System.Environment
import Data.List.Extra
import Data.Either
import Control.Monad.State
import Control.Monad.Extra
import Language.Haskell.Exts

import Synquid.Succinct
import Synquid.Pretty
import Database.Convert
import Database.Generate
import Database.Util
import Database.Download

renameSigs :: String -> [Entry] -> [Entry]
renameSigs _ [] = []
renameSigs currModule (decl:decls) = case decl of
    EModule mdl -> decl:(renameSigs mdl decls)
    EPackage _ -> decl:(renameSigs currModule decls)
    EDecl (TypeSig _ names ty) -> (EDecl (TypeSig () (map (prependName currModule) names) ty)):(renameSigs currModule decls)
    _ -> decl:(renameSigs currModule decls)

addSynonym :: [Entry] -> [Entry]
addSynonym [] = []
addSynonym (decl:decls) = case decl of
    EDecl (TypeDecl _ (DHead _ name) typ) -> let typ' = TyFun () (TyCon () (UnQual () name)) typ
        in (EDecl (TypeSig () [Ident () ((nameStr name)++"To"++(consStr typ))] typ')):(addSynonym decls)
    _ -> decl:(addSynonym decls)

-- readDeclations :: PkgName -> Maybe Version -> State Int [Entry]
readDeclations pkg version = do
    vpkg <- do 
        case version of
            Nothing -> return pkg
            Just v -> ifM (checkVersion pkg v) (return $ pkg ++ "-" ++ v) (return pkg)
    s   <- readFile $ downloadDir ++ vpkg ++ ".txt"
    let code = concat . rights . (map parseLine) $ splitOn "\n" s
    return $ renameSigs "" $ addSynonym code

-- printDeclaration :: Entry -> IO ()
printDeclaration decl = case decl of
    EDecl (TypeSig _ names ty) -> do
        let typ = evalState (toSynquidSchema ty) 0
        putStrLn $ (nameStr ((!!) names 0)) ++ " :: " ++ (show typ)
        let styp = toSuccinctType $ evalState (toSynquidRType ty) 0
        putStrLn $ "===> " ++ (show styp)
    -- EDecl (TypeDecl _ (DHead _ name) typ) -> do
    --     typ' <- return $ TyFun () (TyCon () (UnQual () name)) typ
    --     printSigs (EDecl (TypeSig () [Ident () ((nameStr name)++"To"++(consStr typ))] typ'))
    EDecl decl -> print decl
    EPackage pkg -> putStrLn pkg
    EModule mdl -> putStrLn mdl

printDeclarations :: PkgName -> Maybe Version -> IO ()
printDeclarations pkg version = do
    decls <- readDeclations pkg version
    mapM_ printDeclaration decls


