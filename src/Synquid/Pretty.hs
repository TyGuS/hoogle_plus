{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

module Synquid.Pretty (
  -- * Interface
  Pretty (..),
  Doc,
  renderPretty,
  putDoc,
  prettyShow,
  -- * Basic documents
  empty,
  isEmpty,
  text,
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
  -- * Programs
  prettySpec,
  prettySolution,
  -- * Highlighting
  plain,
  errorDoc,
  -- * Counting
  typeNodeCount,
  programNodeCount
) where

import Types.Environment
import Types.Abstract
import Types.Common
import Types.Type
import Types.Program
import Types.Encoder

import Synquid.Type
import Synquid.Error
import Synquid.Program
import Synquid.Tokens
import Synquid.Util

import Text.PrettyPrint.ANSI.Leijen hiding ((<+>), (<$>), hsep, vsep)
import qualified Text.PrettyPrint.ANSI.Leijen as L
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Hashable
import Data.Tree
import Data.Tree.Pretty

import Control.Lens

infixr 5 $+$
infixr 6 <+>

prettyShow :: Pretty p => p -> String
prettyShow p = displayS (renderCompact $ pretty p) ""

tab = 2

-- | Is document empty?
isEmpty d = case renderCompact d of
  SEmpty -> True
  _ -> False

-- | Separate two documents by space if both are nonempty
doc1 <+> doc2 | isEmpty doc1 = doc2
              | isEmpty doc2 = doc1
              | otherwise    = doc1 L.<+> doc2

-- | Separate two documents by linebreak if both are nonempty
doc1 $+$ doc2 | isEmpty doc1 = doc2
              | isEmpty doc2 = doc1
              | otherwise    = doc1 L.<$> doc2

-- | Separate by spaces
hsep = foldr (<+>) empty
-- | Separate by new lines
vsep = foldr ($+$) empty
-- | Separate by commas
commaSep = hsep . punctuate comma

-- | Enclose in spaces
spaces d = space <> d <> space
-- | Conditionally enclose in parentheses
condParens b doc = if b then parens doc else doc

-- | Conditionally produce a doc
option b doc = if b then doc else empty

-- | Convert a 'Just' value to doc
optionMaybe mVal toDoc = case mVal of
  Nothing -> empty
  Just val -> toDoc val

entryDoc keyDoc valDoc (k, v) = nest 2 $ (keyDoc k  <+> text "->") <+> valDoc v

hMapDoc :: (k -> Doc) -> (v -> Doc) -> Map k v -> Doc
hMapDoc keyDoc valDoc m = brackets (commaSep (map (entryDoc keyDoc valDoc) (Map.toList m)))

vMapDoc :: (k -> Doc) -> (v -> Doc) -> Map k v -> Doc
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

{- Formulas -}

instance Pretty Sort where
  pretty IntS = text "Int"
  pretty BoolS = text "Bool"
  pretty (SetS el) = text "Set" <+> pretty el
  pretty (VarS name) = text name
  pretty (DataS name args) = text name <+> hsep (map (hlParens . pretty) args)
  pretty AnyS = operator "?"

instance Show Sort where
  show = show . plain . pretty

instance Pretty PredSig where
  pretty (PredSig p argSorts resSort) = hlAngles $ text p <+> text "::" <+> hsep (map (\s -> pretty s <+> text "->") argSorts) <+> pretty resSort

instance Pretty UnOp where
  pretty op = operator $ unOpTokens Map.! op

instance Show UnOp where
  show = show . plain . pretty

instance Pretty BinOp where
  pretty op = operator $ binOpTokens Map.! op

instance Show BinOp where
  show = show . plain . pretty

-- | Binding power of a formula
power :: Formula -> Int
power (Pred _ _ []) = 10
power (Cons _ _ []) = 10
power Pred {} = 9
power Cons {} = 9
power Unary {} = 8
power (Binary op _ _)
  | op `elem` [Times, Intersect] = 7
  | op `elem` [Plus, Minus, Union, Diff] = 6
  | op `elem` [Eq, Neq, Lt, Le, Gt, Ge, Member, Subset] = 5
  | op `elem` [And, Or] = 4
  | op `elem` [Implies] = 3
  | op `elem` [Iff] = 2
power All {} = 1
power Ite {} = 1
power _ = 10

-- | Pretty-printed formula
fmlDoc :: Formula -> Doc
fmlDoc = fmlDocAt 0

-- | 'fmlDocAt' @n fml@ : print @expr@ in a context with binding power @n@
fmlDocAt :: Int -> Formula -> Doc
fmlDocAt n fml = condHlParens (n' <= n) (
  case fml of
    BoolLit b -> pretty b
    IntLit i -> intLiteral i
    SetLit s elems -> withSort (SetS s) (hlBrackets $ commaSep $ map fmlDoc elems)
    SetComp (Var s x) e -> withSort (SetS s) (hlBrackets $ text x <> operator "|" <> pretty e)
    Var s name -> withSort s $ if name == valueVarName then special name else text name
    Unknown s name -> if Map.null s then text name else hMapDoc pretty pretty s <> text name
    Unary op e -> pretty op <> fmlDocAt n' e
    Binary op e1 e2 -> fmlDocAt n' e1 <+> pretty op <+> fmlDocAt n' e2
    Ite e0 e1 e2 -> keyword "if" <+> fmlDoc e0 <+> keyword "then" <+> fmlDoc e1 <+> keyword "else" <+> fmlDoc e2
    Pred b name args -> withSort b $ text name <+> hsep (map (fmlDocAt n') args)
    Cons b name args -> withSort b $ hlParens (text name <+> hsep (map (fmlDocAt n') args))
    All x e -> keyword "forall" <+> pretty x <+> operator "." <+> fmlDoc e
  )
  where
    n' = power fml
    withSort s doc = doc -- <> text ":" <> pretty s

instance Pretty Formula where pretty = fmlDoc

instance Show Formula where
  show = show . plain . pretty

instance Pretty a => Pretty (Set a) where
  pretty = pretty . Set.toList

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = pretty . Map.toList

instance (Pretty k, Pretty v) => Pretty (HashMap k v) where
  pretty = pretty . HashMap.toList

instance Pretty QSpace where
  pretty space = braces $ commaSep $ map pretty $ view qualifiers space

instance Show QSpace where
  show = show . plain . pretty

{- Types -}

instance Pretty TypeSkeleton where
  pretty = prettyType

instance Show TypeSkeleton where
 show = show . plain . pretty

-- | Pretty-printed refinement type
prettyType :: TypeSkeleton -> Doc
prettyType = prettyTypeAt 0

-- | Binding power of a type
typePower :: TypeSkeleton -> Int
typePower FunctionT {} = 1
typePower TyAppT {} = 2
typePower _ = 3

prettyTypeAt :: Int -> TypeSkeleton -> Doc
prettyTypeAt n t = condHlParens (n' <= n) (
    case t of
        TypeVarT v -> pretty v
        DatatypeT dt k -> pretty dt <+> operator "::" <+> prettyType k
        TyAppT (DatatypeT "List" _) tArg -> hlBrackets $ prettyType tArg
        TyAppT (TyAppT (DatatypeT "Pair" _) t1) t2 -> hlParens $ prettyType t1 <+> operator "," <+> prettyType t2
        TyAppT tFun tArg -> pretty tFun <+> pretty tArg
        TyFunT tArg tRes -> hlParens (pretty tArg <+> operator "->" <+> pretty tRes)
        AnyT -> text "_"
        FunctionT x t1 t2 -> hlParens (prettyTypeAt n' t1 <+> operator "->" <+> prettyTypeAt 0 t2)
        BotT -> text "⊥"
    )
    where
        n' = typePower t

prettySchema :: SchemaSkeleton -> Doc
prettySchema sch = case sch of
  Monotype t -> pretty t
  ForallT a sch' -> hlAngles (text a)  <+> operator "." <+> prettySchema sch'

instance Pretty SchemaSkeleton where
  pretty = prettySchema

instance Show SchemaSkeleton where
 show = show . plain . pretty

{- Programs -}

prettyProgram :: (Pretty t) => Program t -> Doc
prettyProgram (Program p typ) = case p of
    PSymbol "Nil" -> text "[]"
    PSymbol "Cons" -> text "(:)"
    PSymbol "Pair" -> text "(,)"
    PSymbol s -> case asInteger s of
                  Nothing -> if s == valueVarName then special s else text s
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
      prefix = hang tab $ text funName <+> hsep (map optParens x)
      in if f `elem` Map.elems unOpTokens
            then hang tab $ operator f <+> hsep (map optParens x)
            else prefix
    PFun x e -> nest 2 $ operator "\\" <> text x <+> operator "->" </> prettyProgram e
    PHole -> if show (pretty typ) == dontCare then operator "??" else hlParens $ operator "?? ::" <+> pretty typ
    PErr -> keyword "error"
  where
    withType doc t = doc -- <> text ":" <+> pretty t

instance (Pretty t) => Pretty (Program t) where
  pretty = prettyProgram

instance (Pretty t) => Show (Program t) where
  show = show . plain . pretty

prettyBinding (name, typ) = text name <+> operator "::" <+> pretty typ

prettyBindings env = commaSep (map pretty (Map.keys $ removeDomain (env ^. constants) (allSymbols env)))

instance Pretty Environment where
  pretty = prettyBindings

instance Pretty Goal where
  pretty (Goal name env spec impl depth _) = pretty env <+> operator "⊢" <+> text name <+> operator "::" <+> pretty spec $+$ text name <+> operator "=" <+> pretty impl $+$ parens (text "depth:" <+> pretty depth)

instance Show Goal where
  show = show. plain . pretty

prettySpec g@(Goal name _ _ _ _ _) = text name <+> operator "::" <+> pretty (unresolvedSpec g)
prettySolution (Goal name _ _ _ _ _) prog = text name <+> operator "=" </> pretty prog

{- Input language -}

instance Pretty ConstructorSig where
  pretty (ConstructorSig name t) = text name <+> text "::" <+> pretty t

prettyVarianceParam (predSig, contra) = pretty predSig <> (if contra then pretty Not else empty)

instance Pretty BareDeclaration where
  pretty (TypeDecl name tvs t) = keyword "type" <+> text name <+> hsep (map text tvs) <+> operator "=" <+> pretty t
  pretty (FuncDecl name t) = text name <+> operator "::" <+> pretty t
  pretty (DataDecl name tParams pParams ctors) = hang tab $
    keyword "data" <+> text name <+> hsep (map text tParams) <+> hsep (map prettyVarianceParam pParams) <+> keyword "where"
    $+$ vsep (map pretty ctors)
  pretty (SynthesisGoal name impl) = text name <+> operator "=" <+> pretty impl
  pretty (MutualDecl names) = keyword "mutual" <+> commaSep (map text names)

instance Show BareDeclaration where
  show = show . plain . pretty

instance Pretty a => Pretty (Pos a) where
  pretty (Pos _ x) = pretty x

instance Show a => Show (Pos a) where
  show (Pos _ x) = show x

prettyError (ErrorMessage ParseError pos descr) = align $ hang tab $
  errorDoc (hcat $ map (<> colon) [text (sourceName pos), pretty (sourceLine pos), pretty (sourceColumn pos), text " Parse Error"]) $+$
  pretty descr
prettyError (ErrorMessage ResolutionError pos descr) = hang tab $
  errorDoc (hcat $ map (<> colon) [text (sourceName pos), pretty (sourceLine pos), text " Resolution Error"]) $+$
  pretty descr
prettyError (ErrorMessage TypeError pos descr) = hang tab $
  errorDoc (hcat $ map (<> colon) [text (sourceName pos), pretty (sourceLine pos), text " Error"]) $+$
  pretty descr

instance Pretty ErrorMessage where
  pretty = prettyError

instance Show ErrorMessage where
  show = show . plain . pretty

{- AST node counting -}

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
  tab = text " &"
  nl = text " \\\\"

-- | Really? They didn't think I might want to right-align something?
-- This implementation only works for simple documents with a single string.
-- Implementing the general case seemed not worth it.
lfill :: Int -> Doc -> Doc
lfill w d        = case renderCompact d of
  SText l s _ -> spaces (w - l) <> d
 where
  spaces n | n <= 0    = empty
           | otherwise = text $ replicate n ' '

instance Pretty AbstractBase where
    pretty (ATypeVarT id) = text id
    pretty (ADatatypeT id args) = text id <+> hsep (map pretty args)

instance Show AbstractBase where
    show = show . plain . pretty

instance Pretty AbstractSkeleton where
    pretty (AScalar b) = pretty b
    pretty (AFunctionT tArg tRet) = hlParens (pretty tArg <+> operator "→" <+> pretty tRet)
    pretty ABottom = text "⊥"

instance Show AbstractSkeleton where
    show = show . plain . pretty

instance Pretty SplitInfo where
    pretty (SplitInfo p r tr) = text "Split places:" <+> text (show p)
                             $+$ text "Removed transitions:" <+> text (show r)
                             $+$ text "New transitions:" <+> text (show tr)

instance Pretty FunctionCode where
    pretty (FunctionCode name hop params rets) =
        text "function code:" <+> hlBraces
      ( text "function name:" <+> text name
        $+$ text "HO parameters:" <+> hlBrackets (commaSep (map pretty hop))
        $+$ text "paramters:" <+> hlBrackets (commaSep (map pretty params))
        $+$ text "return types:" <+> hlBrackets (commaSep (map pretty rets)))

instance Show FunctionCode where
    show = show . plain . pretty
