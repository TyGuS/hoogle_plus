{-# LANGUAGE ViewPatterns, PatternGuards, OverloadedStrings, Rank2Types #-}

module Database.Generate where

import Control.Monad.IO.Class
import Language.Haskell.Exts
import Data.Conduit
import Data.Data
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.List.Extra
import Data.Either
import GHC.Generics
import Control.Monad.Extra
import Types.Type (SType, SchemaSkeleton, TypeSkeleton(..))
import Types.Generate
import Synquid.Type (isFunctionType, lastType, toMonotype, shape, arity)
import qualified Synquid.Program as SP
import Synquid.Pretty

import Database.Util


parseMode :: ParseMode
parseMode = defaultParseMode{extensions=map EnableExtension es}
    where es = [ConstraintKinds,EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses
               ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples
               ,ParallelArrays,UnicodeSyntax,DataKinds,PolyKinds]

myParseDecl = fmap (fmap $ const ()) . parseDeclWithMode parseMode -- partial application, to share the initialisation cost

unGADT (GDataDecl a b c d _  [] e) = DataDecl a b c d [] e
unGADT x = x

applyType :: Type a -> [Type a] -> Type a
applyType x (t:ts) = applyType (TyApp (ann t) x t) ts
applyType x [] = x

applyFun1 :: [Type a] -> Type a
applyFun1 [x] = x
applyFun1 (x:xs) = TyFun (ann x) x $ applyFun1 xs

unapplyFun :: Type a -> [Type a]
unapplyFun (TyFun _ x y) = x : unapplyFun y
unapplyFun x = [x]

readItem :: String -> Maybe (Decl ())
readItem x | ParseOk y <- myParseDecl x = Just $ unGADT y
readItem x -- newtype
    | Just x <- stripPrefix "newtype " x
    , ParseOk (DataDecl an _ b c d e) <- fmap unGADT $ myParseDecl $ "data " ++ x
    = Just $ DataDecl an (NewType ()) b c d e
readItem x -- constructors
    | ParseOk (GDataDecl _ _ _ _ _ [GadtDecl s name _ ty] _) <- myParseDecl $ "data Data where " ++ x
    , let f (TyBang _ _ _ (TyParen _ x@TyApp{})) = x
          f (TyBang _ _ _ x) = x
          f x = x
    = Just $ TypeSig s [name] $ applyFun1 $ map f $ unapplyFun ty
readItem ('(':xs) -- tuple constructors
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (TypeSig s [Ident{}] ty) <- myParseDecl $ replicate (length com + 2) 'a' ++ rest
    = Just $ TypeSig s [Ident s $ '(':com++")"] ty
readItem (stripPrefix "data (" -> Just xs)  -- tuple data type
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (DataDecl a b c d e f) <- fmap unGADT $ myParseDecl $
        "data " ++ replicate (length com + 2) 'A' ++ rest
    = Just $ DataDecl a b c ( (op $ '(':com++")") d) e f
    where op s DHead{} = DHead () $ Ident () s
          op s x = x
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


fromIdentity (Ident _ name) = name
fromIdentity (Symbol _ name) = name

getNames (EDecl (TypeSig _ names _)) = map fromIdentity names
getNames _ = []
