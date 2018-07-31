{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, OverloadedStrings, Rank2Types, DeriveDataTypeable #-}

module Database.Generate where
  
import Control.Monad.IO.Class
import Language.Haskell.Exts
import Data.Conduit
import Data.Data
import Data.Char
import Data.List.Extra
import Data.Either
import Control.Monad.Extra

-- | An entry in the Hoogle DB
data Entry = EPackage String
           | EModule String
           | EDecl Decl
             deriving (Data,Typeable,Show,Eq)

parseMode :: ParseMode
parseMode = defaultParseMode{extensions=map EnableExtension es}
    where es = [ConstraintKinds,EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses
               ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples
               ,ParallelArrays,UnicodeSyntax,DataKinds,PolyKinds]

myParseDecl = parseDeclWithMode parseMode -- partial application, to share the initialisation cost

unGADT (GDataDecl a b c d e _  [] f) = DataDecl a b c d e [] f
unGADT x = x

applyFun1 :: [Type] -> Type
applyFun1 [x] = x
applyFun1 (x:xs) = TyFun x $ applyFun1 xs

unapplyFun :: Type -> [Type]
unapplyFun (TyFun x y) = x : unapplyFun y
unapplyFun x = [x]

readItem :: String -> Maybe Decl
readItem x | ParseOk y <- myParseDecl x = Just $ unGADT y
readItem x -- newtype
    | Just x <- stripPrefix "newtype " x
    , ParseOk (DataDecl a b c d e f g) <- fmap unGADT $ myParseDecl $ "data " ++ x
    = Just $ DataDecl a b c d e f g
readItem x -- constructors
    | ParseOk (GDataDecl _ _ _ _ _ _ [GadtDecl s name _ ty] _) <- myParseDecl $ "data Data where " ++ x
    , let f (TyBang _ (TyParen x@TyApp{})) = x
          f (TyBang _ x) = x
          f x = x
    = Just $ TypeSig s [name] $ applyFun1 $ map f $ unapplyFun ty
readItem ('(':xs) -- tuple constructors
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (TypeSig s [Ident{}] ty) <- myParseDecl $ replicate (length com + 2) 'a' ++ rest
    = Just $ TypeSig s [Ident $ '(':com++")"] ty
readItem (stripPrefix "data (" -> Just xs)  -- tuple data type
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (DataDecl a b c d e f g) <- fmap unGADT $ myParseDecl $
        "data " ++ replicate (length com + 2) 'A' ++ rest
    = Just $ DataDecl a b c ( (op $ '(':com++")") d) e f g
    where op s x = x
readItem _ = Nothing

parseLine :: String -> Either String [Entry]
parseLine x@('@':str) = case a of
        "package" | [b] <- words b, b /= "" -> Right [EPackage b]
        "version" -> Right []
        _ -> Left $ "unknown attribute: " ++ x
    where (a,b) = word1 str
parseLine (stripPrefix "module " -> Just x) = Right [EModule x]
parseLine x | Just x <- readItem x = case x of
    TypeSig a bs c -> Right [EDecl (TypeSig a [b] c) | b <- bs]
    x -> Right [EDecl x]
parseLine x = Left $ "failed to parse: " ++ x


fixLine :: String -> String
fixLine (stripPrefix "instance [incoherent] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlap ok] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlapping] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [safe] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "(#) " -> Just x) = "( # ) " ++ x
fixLine ('[':x:xs) | isAlpha x || x `elem` ("_(" :: String), (a,']':b) <- break (== ']') xs = x : a ++ b
fixLine ('[':':':xs) | (a,']':b) <- break (== ']') xs = "(:" ++ a ++ ")" ++ b
fixLine x | "class " `isPrefixOf` x = fst $ breakOn " where " x
fixLine x = x

fromIdentity (Ident name) = name
fromIdentity (Symbol name) = name

getNames (EDecl (TypeSig _ names _)) = map fromIdentity names
getNames _ = []
