{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, OverloadedStrings, Rank2Types, DeriveDataTypeable #-}

module Database.Generate where
  
import Control.Monad.IO.Class
import Language.Haskell.Exts
import Data.Conduit
import Data.Data
import Data.Char
import Data.List.Extra
import Data.Either
import qualified Data.Text as Text
import Control.Monad.Extra
import Hakyll.Web.Html
import Text.HTML.TagSoup.Entity (lookupEntity)
import Language.Preprocessor.Cpphs (runCpphs, BoolOptions (..), CpphsOptions (..))

-- | An entry in the Hoogle DB
data Entry = EPackage String
           | EModule String
           | EDecl (Decl SrcSpanInfo)
             deriving (Data,Typeable,Show,Eq)

toHaskellCode :: FilePath -> IO String
toHaskellCode fp = do
    content <- readFile fp
    let res = unescapeEntities $ stripTags content
    -- writeFile "test.hs" res
    return res

unescapeEntities :: String -> String
unescapeEntities [] = []
unescapeEntities ('&':xs) = 
  let (b, a) = break (== ';') xs in
  case (lookupEntity b, a) of
    (Just c, ';':as) ->  c  ++ unescapeEntities as    
    _                -> '&' : unescapeEntities xs
unescapeEntities (x:xs) = x : unescapeEntities xs

parseMode :: ParseMode
parseMode = defaultParseMode{extensions=map EnableExtension es, baseLanguage=Haskell2010}
    where es = [ConstraintKinds,EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses, BangPatterns
               ,TypeFamilies,FlexibleContexts,FlexibleInstances,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples
               ,ParallelArrays,UnicodeSyntax,DataKinds,PolyKinds,CPP,MultiParamTypeClasses,Safe,PolyKinds,PolyKinds,NamedFieldPuns
               ,ScopedTypeVariables,ExistentialQuantification,NPlusKPatterns,RegularPatterns,ViewPatterns,PatternSynonyms]


readModuleDecls :: String -> IO [Entry]
readModuleDecls moduleCont = do
    let myBoolOpts = BoolOptions False False False False False False False False False False False  
    let myCpphsOpts = CpphsOptions [] [] [("WORD_SIZE_IN_BITS","32"),("FLT_RADIX","2"),("SIZEOF_HSWORD","4")] [] [] myBoolOpts
    preprocessed <- runCpphs myCpphsOpts "unknown" moduleCont
    writeFile "test.hs" preprocessed
    case parseWithMode parseMode preprocessed of
        ParseOk (Module _ (Just mhead) _ _ decls) -> return $ (EModule (moduleName mhead)) : (map (EDecl . unGADT) $ filter usefulDecl decls)
        ParseOk _ -> error "This is not a whole module"
        ParseFailed loc err -> error $ (show loc) ++ " " ++ err
  where
    moduleName (ModuleHead _ name _ _) = let ModuleName _ mname = name in mname

usefulDecl (TypeDecl{}) = True
usefulDecl (DataDecl{}) = True
usefulDecl (GDataDecl {}) = True
usefulDecl (TypeSig {}) = True
usefulDecl _ = False

myParseDecl = parseDeclWithMode parseMode -- partial application, to share the initialisation cost

unGADT (GDataDecl a b c d _  [] f) = DataDecl a b c d [] f
unGADT x = x

fromIdentity (Ident _ name) = name
fromIdentity (Symbol _ name) = name

getNames (EDecl (TypeSig _ names _)) = map fromIdentity names
getNames _ = []
