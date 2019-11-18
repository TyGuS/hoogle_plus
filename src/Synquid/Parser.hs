{-# LANGUAGE TupleSections #-}

-- | The parser for Synquid's program specification DSL.
module Synquid.Parser where

import Types.Program
import Types.Common
import Types.Type
import Synquid.Type
import Synquid.Program
import Synquid.Error
import Synquid.Tokens
import Synquid.Util
import Database.Util

import Data.List
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad.State
import Control.Applicative hiding ((<|>), many)

import Text.Parsec hiding (State)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Pos
import Text.Parsec.Expr
import Text.Parsec.Indent
import Text.Parsec.Error
import Text.PrettyPrint.ANSI.Leijen (text, vsep)
import Debug.Trace

{- Interface -}

type Parser a = IndentParserT String () (State SourcePos) a

parseProgram :: Parser [Declaration]
parseProgram = whiteSpace *> option [] (block parseDeclaration) <* eof

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile aParser fname = do
  input <- readFile fname
  return $ flip evalState (initialPos fname) $ runIndentParserT aParser () fname input

toErrorMessage :: ParseError -> ErrorMessage
toErrorMessage err = ErrorMessage ParseError (errorPos err)
  (vsep $ map text $ tail $ lines $ showErrorMessages "or" "unknown parse error" "expecting" "Unexpected" "end of input" (errorMessages err))

{- Lexical analysis -}

opNames :: [String]
opNames = otherOps

opStart :: [Char]
opStart = nub (map head opNames)

opLetter :: [Char]
opLetter = nub (concatMap tail opNames)

synquidDef :: Token.GenLanguageDef String st (IndentT (State SourcePos))
synquidDef = Token.LanguageDef
    commentStart
    commentEnd
    commentLine
    False
    (letter <|> oneOf identifierChars)
    (alphaNum <|> oneOf identifierChars)
    (oneOf opStart)
    (oneOf opLetter)
    keywords
    opNames
    True

lexer :: Token.GenTokenParser String st (IndentT (State SourcePos))
lexer = Token.makeTokenParser synquidDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
natural = Token.natural lexer
whiteSpace = Token.whiteSpace lexer
angles = Token.angles lexer
brackets = Token.brackets lexer
parens = Token.parens lexer
braces = Token.braces lexer
comma = Token.comma lexer
commaSep = Token.commaSep lexer
commaSep1 = Token.commaSep1 lexer
dot = Token.dot lexer

{- Declarations -}

parseDeclaration :: Parser Declaration
parseDeclaration = attachPosBefore $
    (choice [ parseTypeDecl
            , parseDataDecl
            , parseFuncDeclOrGoal] <?> "declaration")

parseTypeDecl :: Parser BareDeclaration
parseTypeDecl = do
    reserved "type"
    typeName <- parseType
    reservedOp "="
    typeDef <- parseType
    return $ TypeDecl typeName typeDef

fixArgName :: Int -> TypeSkeleton -> TypeSkeleton
fixArgName _ typ = let (_, res) = fixArgName' 0 typ in res
    where
        fixArgName' idx (FunctionT x tArg tRes)
            | "@@arg" `isPrefixOf` x = let
                (idx', tArg') = fixArgName' (idx + 1) tArg
                (idx'', tRes') = fixArgName' idx' tRes
                in (idx'', FunctionT ("arg"++show idx) tArg' tRes')
            | otherwise = let
                (idx', tArg') = fixArgName' idx tArg
                (idx'', tRes') = fixArgName' idx' tRes
                in (idx'', FunctionT x tArg' tRes')
        fixArgName' idx t = (idx, t)

parseDataDecl :: Parser BareDeclaration
parseDataDecl = do
    reserved "data"
    typeName <- parseTypeName
    tParams <- many (sameOrIndented >> parseIdentifier)
    constructors <- option [] (reserved "where" >> indented >> block parseConstructorSig)
    return $ DataDecl typeName tParams constructors

parseConstructorSig :: Parser ConstructorSig
parseConstructorSig = do
    ctorName <- parseTypeName
    reservedOp "::"
    ctorType <- parseType
    return $ ConstructorSig ctorName $ fixArgName 0 ctorType

parseFuncDeclOrGoal :: Parser BareDeclaration
parseFuncDeclOrGoal = do
    funcName <- parseIdentifier
    (reservedOp "::" >> FuncDecl funcName <$> parseSchema) <|>
        (reservedOp "=" >> SynthesisGoal funcName <$> parseImpl)

{- Types -}

parseSchema :: Parser SchemaSkeleton
parseSchema = Monotype <$> parseTypeMbTypeclasses

parseTypeMbTypeclasses :: Parser TypeSkeleton
parseTypeMbTypeclasses = do
    tcs <- try parseTypeclasses <|> (pure id)
    plainTypes <- parseType
    return (tcs $ fixArgName 0 plainTypes)

parseTCName :: Parser TypeSkeleton
parseTCName = do
    name <- parseTypeName
    typeArgs <- many (sameOrIndented >> parseArgType)
    let typeName = tyclassPrefix ++ name
    return $ foldl' TyAppT (DatatypeT typeName) typeArgs

parseTypeclasses :: Parser (TypeSkeleton -> TypeSkeleton)
parseTypeclasses = do
    tcs <- parens (commaSep1 parseTCName) <|> (parseTCName >>= (\x -> return [x]))
    let tcsAndNumbers = zip tcs [0 .. (length tcs)]
    reservedOp "=>"
    let combineTypeclasses (tc, num) next rest = FunctionT (tyclassArgBase ++ (show num)) tc (next rest)
    return $ foldr combineTypeclasses id tcsAndNumbers

parseType :: Parser TypeSkeleton
parseType = withPos (choice [try parseFunctionTypeWithArg, parseFunctionTypeMb] <?> "type")

-- | Parse top-level type that starts with an argument name, and thus must be a function type
parseFunctionTypeWithArg :: Parser TypeSkeleton
parseFunctionTypeWithArg = do
    argId <- parseArgName
    argType <- parseArgType
    reservedOp "->"
    returnType <- parseType
    return $ FunctionT argId argType returnType
    where
        parseArgName = parseIdentifier <* reservedOp ":"

-- | Parse top-level type that does not start with an argument name
-- and thus could be a scalar or a function type depending on whether followed by ->
parseFunctionTypeMb :: Parser TypeSkeleton
parseFunctionTypeMb = do
    argType <- parseArgType
    parseFunctionRest argType <|> return argType
    where
        parseFunctionRest argType = do
            reservedOp "->"
            returnType <- parseType
            return $ FunctionT ("@@arg" ++ show (arity returnType)) argType returnType

parseArgType :: Parser TypeSkeleton
parseArgType = choice [
    parseUnrefTypeWithArgs,
    try parseUnrefHkType,
    parseTypeAtom
    ]

parseTypeAtom :: Parser TypeSkeleton
parseTypeAtom = choice [
    try $ parens parseType,
    parseUnrefTypeNoArgs,
    parseListType,
    parsePairType
    ]

parseUnrefTypeNoArgs = choice [
    DatatypeT <$> parseTypeName,
    TypeVarT <$> parseIdentifier]

parseUnrefTypeWithArgs = do
    name <- parseTypeName
    typeArgs <- many (sameOrIndented >> parseTypeAtom)
    return $ foldl' TyAppT (DatatypeT name) typeArgs

parseUnrefHkType = do
    hkArg <- parseIdentifier
    args <- many1 (sameOrIndented >> parseTypeAtom)
    return $ foldl' TyAppT (TypeVarT hkArg) args

parseScalarUnrefType = parseUnrefTypeWithArgs <|> parseUnrefTypeNoArgs

parseListType = do
    elemType <- brackets parseType
    return $ TyAppT (DatatypeT "List") elemType

parsePairType = do
    elemType <- parens (commaSep1 parseType)
    return $ foldr mkPair (initial elemType) (drop 2 elemType)
    where
        mkPair t p = TyAppT (TyAppT (DatatypeT "Pair") p) t
        initial (x:y:_) = TyAppT (TyAppT (DatatypeT "Pair") x) y

{- Implementations -}

parseImpl :: Parser UProgram
parseImpl = withPos (parseFun <|> parseETerm)

parseFun = do
    reservedOp "\\"
    x <- parseIdentifierOrBlank
    reservedOp "."
    body <- parseImpl
    return $ untyped $ PFun x body

parseETerm = choice [parseAppTerm, parseAtomTerm] <?> "elimination term"
    where
        parseAppTerm = do
            f <- parseIdentifier
            args <- many (sameOrIndented >> (try parseAtomTerm <|> parens parseImpl))
            return $ untyped $ PApp f args
        parseAtomTerm = choice [
            parens (withOptionalType parseImpl)
            , parseHole
            , parseBoolLit
            , parseIntLit
            , parseSymbol
            , parseList
            ]
        parseBoolLit = (reserved "False" >> return (untyped $ PSymbol "False")) <|> (reserved "True" >> return (untyped $ PSymbol "True"))
        parseIntLit = natural >>= return . untyped . PSymbol . show
        parseHole = reserved "??" >> return (untyped PHole)
        parseSymbol = (parseIdentifier <|> parseTypeName) >>= (return . untyped . PSymbol)

parseList = do
    elems <- brackets (commaSep parseImpl)
    return $ foldr cons nil elems
    where
        cons x xs = untyped $ PApp "Cons" [x, xs]
        nil = untyped $ PSymbol "Nil"

withOptionalType p = do
    (Program content _) <- p
    typ <- option AnyT $ reserved "::" >> parseType
    return $ Program content typ

{- Misc -}

parseIdentifier :: Parser Id
parseIdentifier = try $ do
    name <- identifier
    if isTypeName name
        then unexpected ("capitalized " ++ show name)
        else return name

parseIdentifierOrBlank :: Parser Id
parseIdentifierOrBlank = try $ do
    name <- identifier
    if isTypeName name 
        then unexpected ("capitalized " ++ show name) 
        else return name

parseTypeName :: Parser Id
parseTypeName = try $ do
    name <- identifier
    if not (isTypeName name) then unexpected ("non-capitalized " ++ show name) else return name

-- | 'attachPosBefore' @p@ : parser that behaves like @p@, but also attaches the source position before the first token it parsed to the result
attachPosBefore :: Parser a -> Parser (Pos a)
attachPosBefore = liftM2 Pos getPosition
