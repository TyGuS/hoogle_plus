module AppUtils where

import Debug.Trace
import Language.Haskell.Exts (parseFile, ParseResult(..))
import Language.Haskell.Exts.Syntax (
  Module (..), 
  ImportDecl (..),
  ModuleName (..),
  Decl (..),
  Match (..),
  Name (..),
  Exp (..),
  Pat (..),
  QName (..),
  Rhs (..)
  )
import Language.Haskell.Exts.Pretty (prettyPrint)
import System.Environment (getArgs)
import Language.Haskell.Exts.ExactPrint (exactPrint)
import Data.Foldable (foldl, foldMap)
import Data.Generics.Uniplate.Data 
import Data.List (isInfixOf)
import Control.Monad.Writer
import System.IO
import Language.Haskell.Ghcid
import Hoogle
import Control.Monad.State  
import ImportUtils


getNameFromMatches :: [(Match l)] -> String
getNameFromMatches l = 
  let names            = map getNameFromMatch l
      allNamesAreEqual = areAllElementsEqual names
      in (if allNamesAreEqual then (names !! 0) else (error errMessage))
  where getNameFromMatch (Match _ (Ident _ name') _ _ _)        = name'
        getNameFromMatch (InfixMatch _ _ (Ident _ name') _ _ _) = name'
        errMessage = "unexpected parsing of function names"

areAllElementsEqual :: Eq a => [a] -> Bool
areAllElementsEqual []     = True
areAllElementsEqual (x:xs) =  all (\elem -> elem == x) xs 




getIdentifiers ast = 
  let allNames       = (universeBi ast)::[(QName ())]
      allIdentifiers1  = [(x, n) | x@(UnQual _ (Ident () n)) <- allNames]
      allIdentifiers2  = [(x, n)  | x@(Qual _ _ (Ident () n)) <- allNames]
      
    in (allIdentifiers1 ++ allIdentifiers2)

removeAnnotations ast = fmap (\_ -> ()) ast

-- TODO: can we call removeAnnotations earlier on?
getAllApplications fileAST = allFunctionApplications
  where annotatedDeclASTs       = getDecls fileAST
        declASTs                = map removeAnnotations annotatedDeclASTs
        functionDeclASTs        = filter isFunctionDecl declASTs
        getExpList x     = (universeBi x)::([Exp ()])
        expsPerFunction  = map getExpList functionDeclASTs

        getAppList x     = [app | Just app <- (map maybeFuncApp x)]
        appsPerFunction  = map getAppList expsPerFunction
        allFunctionApplications = concat appsPerFunction


printExcerpt ast = putStrLn $ prettyPrint ast

isFunctionDecl (FunBind _ _)     = True
isFunctionDecl (PatBind _ _ _ _) = True
isFunctionDecl _                 = False


getDecls (ParseOk (Module _ _ _ _ decls)) = decls
getDecls _                                = []

getDecls' (Module _ _ _ _ decls) = decls
getDecls' _                      = []



getFunctionName :: (Decl l) -> String
getFunctionName ast = 
  case ast of
    ast@(PatBind _ (PVar _ (Ident _ name)) _ _) -> name
    ast@(FunBind _ matches)                     -> getNameFromMatches matches


maybeFuncApp expAST = 
   case expAST of
     expAST@(App _ _ _ )       -> Just expAST
     expAST@(InfixApp _ _ _ _) -> Just expAST
     _                         -> Nothing


