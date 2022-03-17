module Types.Pretty (
  -- * Interface
  Pretty (..),
  Doc,
  renderPretty,
  putDoc,
  prettyShow,
  showSTypeWithName,
  -- * Basic documents
  empty,
  isEmpty,
  linebreak,
  semi,
  lbrace, rbrace,
  -- * Combinators
  option,
  optionMaybe,
  (<+>), ($+$), (<>), (</>),
  hcat,
  vcat,
  hsep,
  vsep,
  punctuate,
  tupled,
  -- * Enclosing
  commaSep,
  parens,
  condParens,
  squotes,
  dquotes,
  brackets,
  braces,
  angles,
  spaces,
  -- * Indentation
  nest,
  hang,
  indent,
  -- * Structures
  hMapDoc,
  vMapDoc,
  mkTable,
  mkTableLaTeX,
  -- * Highlighting
  plain,
  errorDoc
) where

import Control.Lens
import Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text

import Text.PrettyPrint.ANSI.Leijen hiding ((<+>), (<$>), hsep, vsep, text )
import qualified Text.PrettyPrint.ANSI.Leijen as L

import Types.Environment
import Types.Type
import Types.Program
import Types.Encoder
import Compiler.Error
import Utility.Utils ( asInteger, text )

infixr 5 $+$
infixr 6 <+>

prettyShow :: Pretty p => p -> String
prettyShow p = displayS (renderCompact $ pretty p) ""

tab :: Int
tab = 2

-- | Is document empty?
isEmpty :: Doc -> Bool
isEmpty d = case renderCompact d of
  SEmpty -> True
  _ -> False

-- | Separate two documents by space if both are nonempty
(<+>) :: Doc -> Doc -> Doc
doc1 <+> doc2 | isEmpty doc1 = doc2
              | isEmpty doc2 = doc1
              | otherwise    = doc1 L.<+> doc2

-- | Separate two documents by linebreak if both are nonempty
($+$) :: Doc -> Doc -> Doc
doc1 $+$ doc2 | isEmpty doc1 = doc2
              | isEmpty doc2 = doc1
              | otherwise    = doc1 L.<$> doc2

-- | Separate by spaces
hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty

-- | Separate by new lines
vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

-- | Separate by commas
commaSep :: [Doc] -> Doc
commaSep = hsep . punctuate comma

-- | Enclose in spaces
spaces :: Doc -> Doc
spaces d = space <> d <> space

-- | Conditionally enclose in parentheses
condParens :: Bool -> Doc -> Doc
condParens b doc = if b then parens doc else doc

-- | Conditionally produce a doc
option :: Bool -> Doc -> Doc
option b doc = if b then doc else empty

-- | Convert a 'Just' value to doc
optionMaybe :: Maybe Doc -> (Doc -> Doc) -> Doc
optionMaybe mVal toDoc = maybe empty toDoc mVal

entryDoc :: (Pretty a, Pretty b) => (a -> Doc) -> (b -> Doc) -> (a, b) -> Doc
entryDoc keyDoc valDoc (k, v) = nest 2 $ (keyDoc k  <+> string "->") <+> valDoc v

hMapDoc :: (Pretty k, Pretty v) => (k -> Doc) -> (v -> Doc) -> Map k v -> Doc
hMapDoc keyDoc valDoc m = brackets (commaSep (map (entryDoc keyDoc valDoc) (Map.toList m)))

vMapDoc :: (Pretty k, Pretty v) => (k -> Doc) -> (v -> Doc) -> Map k v -> Doc
vMapDoc keyDoc valDoc m = vsep $ map (entryDoc keyDoc valDoc) (Map.toList m)

{- Syntax highlighting -}

errorDoc = red
keyword = bold . blue . text
parenDoc = dullwhite
operator = dullwhite . text
special = bold . text
intLiteral = dullcyan . pretty

hlParens = enclose (parenDoc lparen) (parenDoc rparen)
hlBraces = enclose (parenDoc lbrace) (parenDoc rbrace)
hlAngles = enclose (parenDoc langle) (parenDoc rangle)
hlBrackets = enclose (parenDoc lbracket) (parenDoc rbracket)

condHlParens b doc = if b then hlParens doc else doc

instance Pretty Text where
  pretty = text

instance Pretty a => Pretty (Set a) where
  pretty = pretty . Set.toList

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = pretty . Map.toList

instance (Pretty k, Pretty v) => Pretty (HashMap k v) where
  pretty = pretty . HashMap.toList

{- Types -}

instance Pretty TypeSkeleton where
  pretty (TypeVarT v) = text v
  pretty (DatatypeT "List" [tArg]) = hlBrackets $ pretty tArg
  pretty (DatatypeT "Pair" [larg, rarg]) = hlParens $ pretty larg <+> string "," <+> pretty rarg
  pretty (DatatypeT dt tArgs) = text dt <+> hsep (map (hlParens . pretty) tArgs)
  pretty (FunctionT _ tArg tRes) = hlParens $ pretty tArg <+> string "->" <+> pretty tRes
  pretty TopT = string "_"
  pretty BotT = string "⊥"

prettySTypeWithName :: TypeSkeleton -> Doc
prettySTypeWithName (FunctionT x t1 t2)
  | isFunctionType t1 = text x <> operator ":" <> hlParens (pretty t1) <+> operator "->" <+> prettySTypeWithName t2
  | otherwise = text x <> operator ":" <> pretty t1 <+> operator "->" <+> prettySTypeWithName t2
prettySTypeWithName t = pretty t

showSTypeWithName :: TypeSkeleton -> String
showSTypeWithName = show . plain . prettySTypeWithName

instance Pretty SchemaSkeleton where
  pretty (Monotype t)     = pretty t
  pretty (ForallT a sch') = hlAngles (text a)  <+> operator "." <+> pretty sch'

{- Programs -}

prettyProgram :: (Pretty t) => Program t -> Doc
prettyProgram (Program p typ) = case p of
    PSymbol "Nil" -> string "[]"
    PSymbol "Cons" -> string "(:)"
    PSymbol "Pair" -> string "(,)"
    PSymbol s -> case asInteger (Text.unpack s) of
                  Nothing -> text s
                  Just n -> intLiteral n
    PApp f x -> let
      optParens p = case p of
        Program (PSymbol _) _ -> prettyProgram p
        Program PHole _ -> prettyProgram p
        _ -> hlParens (prettyProgram p)
      funName = case f of
                  "Cons" -> "(:)"
                  "Pair" -> "(,)"
                  _      -> f
      mbPair f = if f == "(,)" then hlParens else id
      isTcArg p = case p of
                    Program (PSymbol n) _ -> isTyclass n
                    Program (PApp n _) _ -> isTyclass n
                    _ -> False
      countArgs = filter (not . isTcArg) x
      prefix = if '(' == Text.head funName && length countArgs == 2 -- infix operators
                  then let funName' = Text.drop 1 funName
                           lastPart = Text.reverse $ Text.takeWhile ('.' /=) $ Text.tail $ Text.reverse funName'
                        in hang tab $ mbPair funName $ optParens (head countArgs) <+> text lastPart <+> optParens (countArgs !! 1)
                  else hang tab $ text funName <+> hsep (map optParens x)
      in prefix
    PFun x e -> let (args, e') = mergeLambdas (Program p typ)
                 in nest 2 $ operator "\\" <> hsep (map text args) <+> operator "->" <+> prettyProgram e'
    PHole -> hlParens $ operator "?? ::" <+> pretty typ
  where
    withType doc t = doc -- <> string ":" <+> pretty t
    mergeLambdas (Program (PFun x e) _) = let (args, e') = mergeLambdas e
                                           in (x:args, e')
    mergeLambdas p = ([], p)

instance (Pretty t) => Pretty (Program t) where
  pretty = prettyProgram

prettyBinding :: Pretty a => (Text, a) -> Doc
prettyBinding (name, typ) = text name <+> operator "::" <+> pretty typ

prettyBindings :: Environment -> Doc
prettyBindings env = commaSep (map pretty (Map.keys $ allSymbols env))

instance Pretty Environment where
  pretty = prettyBindings

instance Pretty Goal where
  pretty (Goal _ spec) = pretty spec

{- Input language -}

instance Pretty ConstructorSig where
  pretty (ConstructorSig name t) = text name <+> string "::" <+> pretty t

instance Pretty BareDeclaration where
  pretty (TypeDecl name tvs t) = keyword "type" <+> text name <+> hsep (map text tvs) <+> operator "=" <+> pretty t
  pretty (FuncDecl name t) = text name <+> operator "::" <+> pretty t
  pretty (DataDecl name tParams ctors) = hang tab $
    keyword "data" <+> text name <+> hsep (map text tParams) <+> keyword "where"
    $+$ vsep (map pretty ctors)

instance Pretty a => Pretty (Pos a) where
  pretty (Pos _ x) = pretty x

instance Show a => Show (Pos a) where
  show (Pos _ x) = show x

prettyError (ErrorMessage ParseError pos descr) = align $ hang tab $
  errorDoc (hcat $ map (<> colon) [string (sourceName pos), pretty (sourceLine pos), pretty (sourceColumn pos), string " Parse Error"]) $+$
  pretty descr
prettyError (ErrorMessage ResolutionError pos descr) = hang tab $
  errorDoc (hcat $ map (<> colon) [string (sourceName pos), pretty (sourceLine pos), string " Resolution Error"]) $+$
  pretty descr
prettyError (ErrorMessage TypeError pos descr) = hang tab $
  errorDoc (hcat $ map (<> colon) [string (sourceName pos), pretty (sourceLine pos), string " Error"]) $+$
  pretty descr

instance Pretty ErrorMessage where
  pretty = prettyError

instance Show ErrorMessage where
  show = show . plain . pretty

-- | Prints data in a table, with fixed column widths.
-- Positive widths for left justification, negative for right.
mkTable :: [Int] -> [[Doc]] -> Doc
mkTable widths docs = vsep $ map (mkTableRow widths) docs
mkTableRow widths docs = hsep $ zipWith signedFill widths docs

signedFill w doc | w < 0 = lfill (-w) doc
                 | otherwise = fill w doc

mkTableLaTeX :: [Int] -> [[Doc]] -> Doc
mkTableLaTeX widths docs =
  uncurry mkTable $ insertSeps widths docs
 where
  insertSeps widths docs = (intersperse 3 widths ++ [3],
    map ((++ [nl]) . intersperse tab) docs)
  tab = string " &"
  nl = string " \\\\"

-- | Really? They didn't think I might want to right-align something?
-- This implementation only works for simple documents with a single string.
-- Implementing the general case seemed not worth it.
lfill :: Int -> Doc -> Doc
lfill w d        = case renderCompact d of
  SText l s _ -> spaces (w - l) <> d
  _           -> d
 where
  spaces n | n <= 0    = empty
           | otherwise = string $ replicate n ' '

instance Pretty SplitInfo where
    pretty (SplitInfo p r tr) = string "Split places:" <+> string (show p)
                             $+$ string "Removed transitions:" <+> string (show r)
                             $+$ string "New transitions:" <+> string (show tr)

instance Pretty EncodedFunction where
    pretty (EncodedFunction name params rets) =
        string "function code:" <+> hlBraces
      ( string "function name:" <+> text name
        $+$ string "paramters:" <+> hlBrackets (commaSep (map pretty params))
        $+$ string "return types:" <+> hlBrackets (commaSep (map pretty rets)))