module Compiler.Parser
  ( parseFromFile
  , parseType
  , parseTypeMbTypeclasses
  , parseSchema
  , toErrorMessage
  ) where

import           Control.Monad.State            ( State
                                                , evalState
                                                , liftM2
                                                )
import           Data.Char                      ( isDigit
                                                , isLower
                                                , isUpper
                                                )
import           Data.List                      ( isPrefixOf
                                                , nub
                                                )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Text.Parsec
import           Text.Parsec.Error              ( errorMessages
                                                , showErrorMessages
                                                )
import qualified Text.Parsec.Token             as Token
import           Text.PrettyPrint.ANSI.Leijen   ( text
                                                , vsep
                                                )

import           Compiler.Error
import           Types.Common
import           Types.Program
import           Types.Type

--------------------------------------------------------------------------------
------------------------------ High Level Interfaces ---------------------------
--------------------------------------------------------------------------------

type Parser a = Parsec String () a

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile aParser fname = do
  input <- readFile fname
  return $ runParser aParser () fname input

toErrorMessage :: ParseError -> ErrorMessage
toErrorMessage err = ErrorMessage
  ParseError
  (vsep $ map text $ tail $ lines $ showErrorMessages "or"
                                                      "unknown parse error"
                                                      "expecting"
                                                      "Unexpected"
                                                      "end of input"
                                                      (errorMessages err)
  )

--------------------------------------------------------------------------------
---------------------------------- Tokens --------------------------------------
--------------------------------------------------------------------------------
operators :: [String]
operators = ["::", ":", "->", "|", "=", "??", ",", ".", "\\"]

-- | Characters allowed in identifiers (in addition to letters and digits)
identifierChars :: String
identifierChars = "_'"

-- | Start of a multi-line comment
commentStart :: String
commentStart = "{-"

-- | End of a multi-line comment
commentEnd :: String
commentEnd = "-}"

-- | Start of a single-line comment
commentLine :: String
commentLine = "--"

isTypeName :: String -> Bool
isTypeName str = isUpper $ head str

isIdentifier :: String -> Bool
isIdentifier str = isLower $ head str

opNames :: [String]
opNames = operators

opStart :: String
opStart = nub (map head opNames)

opLetter :: String
opLetter = nub (concatMap tail opNames)

keywords :: [String]
keywords = []

hplusDef :: Token.LanguageDef st
hplusDef = Token.LanguageDef commentStart
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

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser hplusDef

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

natural :: Parser Integer
natural = Token.natural lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

angles :: Parser a -> Parser a
angles = Token.angles lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

comma :: Parser String
comma = Token.comma lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Token.commaSep1 lexer

dot :: Parser String
dot = Token.dot lexer

--------------------------------------------------------------------------------
---------------------------------- Parsers -------------------------------------
--------------------------------------------------------------------------------

----------- Parsing Declarations ------------

parseDeclaration :: Parser Declaration
parseDeclaration = choice [parseTypeDecl, parseFuncDecl] <?> "declaration"

parseTypeDecl :: Parser Declaration
parseTypeDecl = do
  reserved "type"
  typeName <- parseTypeName
  typeVars <- many parseIdentifier
  reservedOp "="
  TypeDecl typeName typeVars <$> parseType


parseFuncDecl :: Parser Declaration
parseFuncDecl = do
  funcName <- parseIdentifier
  reservedOp "::"
  FuncDecl funcName <$> parseSchema

----------- Parsing Types ------------

parseSchema :: Parser SchemaSkeleton
parseSchema = Monotype <$> parseTypeMbTypeclasses

parseTypeMbTypeclasses :: Parser TypeSkeleton
parseTypeMbTypeclasses = do
  tcs <- try parseTypeclasses <|> pure id
  tcs . fixArgName 0 <$> parseType

parseTCName :: Parser TypeSkeleton
parseTCName = do
  name     <- parseTypeName
  typeArgs <- many parseTypeAtom
  let typeName = tyclassPrefix `Text.append` name
  return $ DatatypeT typeName typeArgs

parseTypeclasses :: Parser (TypeSkeleton -> TypeSkeleton)
parseTypeclasses = do
  tcs <- parens (commaSep1 parseTCName) <|> (parseTCName >>= (\x -> return [x]))
  let tcsAndNumbers = zip tcs [0 .. (length tcs)]
  reservedOp "=>"
  let combineTypeclasses (tc, num) next rest =
        FunctionT (mkTyclassArg num) tc (next rest)
  return $ foldr combineTypeclasses id tcsAndNumbers
 where
  mkTyclassArg :: Int -> Text
  mkTyclassArg num = tyclassArgBase `Text.append` Text.pack (show num)

parseType :: Parser TypeSkeleton
parseType =
  choice [try parseFunctionTypeWithArg, parseFunctionTypeMb] <?> "type"

-- | Parse top-level type that starts with an argument name, and thus must be a function type
parseFunctionTypeWithArg :: Parser TypeSkeleton
parseFunctionTypeWithArg = do
  argId   <- parseIdentifier <* reservedOp ":"
  argType <- parseTypeAtom
  reservedOp "->"
  FunctionT argId argType <$> parseType

-- | Parse top-level type that does not start with an argument name
-- and thus could be a scalar or a function type depending on whether followed by ->
parseFunctionTypeMb :: Parser TypeSkeleton
parseFunctionTypeMb = do
  argType <- parseTypeAtom
  parseFunctionRest argType <|> return argType
 where
  parseFunctionRest argType = do
    reservedOp "->"
    FunctionT "" argType <$> parseType

parseTypeAtom :: Parser TypeSkeleton
parseTypeAtom = choice
  [ try $ parens parseType
  , parseDatatype <|> parseTypeNoArgs
  , parseListType
  , parsePairType
  ]

parseTypeNoArgs :: Parser TypeSkeleton
parseTypeNoArgs =
  choice [(`DatatypeT` []) <$> parseTypeName, vart <$> parseIdentifier]

parseDatatype :: Parser TypeSkeleton
parseDatatype = do
  name     <- parseTypeName
  typeArgs <- many parseTypeAtom
  return $ DatatypeT name typeArgs

parseListType :: Parser TypeSkeleton
parseListType = do
  elemType <- brackets parseType
  return $ listType elemType

parsePairType :: Parser TypeSkeleton
parsePairType = do
  elemType <- parens (commaSep1 parseType)
  return $ foldr pairType (initial elemType) (drop 2 elemType)
 where
  initial (x : y : _) = pairType x y
  initial _ = error "parsePairType: pair type with less than two elements"

-------------- Miscellaneous --------------

parseIdentifier :: Parser Id
parseIdentifier = try $ do
  name <- identifier
  if isTypeName name
    then unexpected ("capitalized " ++ show name)
    else return (Text.pack name)

parseIdentifierOrBlank :: Parser Id
parseIdentifierOrBlank = try $ do
  name <- identifier
  if isTypeName name
    then unexpected ("capitalized " ++ show name)
    else return (Text.pack name)

parseTypeName :: Parser Id
parseTypeName = try $ do
  name <- identifier
  if not (isTypeName name)
    then unexpected ("non-capitalized " ++ show name)
    else return (Text.pack name)

--------------------------------------------------------------------------------
----------------------------- Parsing Utilities --------------------------------
--------------------------------------------------------------------------------

checkNum :: Text -> Bool
checkNum t = foldr ((&&) . isDigit) True (Text.unpack t)

fixIndex :: TypeSkeleton -> Int
fixIndex (FunctionT n a r) =
  let num      = Text.drop 3 n
      maxIndex = max (fixIndex a) (fixIndex r)
      argNum   = if "arg" `Text.isPrefixOf` n && checkNum num
        then read (Text.unpack num) :: Int
        else -1
  in  max argNum maxIndex
fixIndex _ = -1

fixArgName :: Int -> TypeSkeleton -> TypeSkeleton
fixArgName _ typ = res
 where
  (_, res) = fixArgName' (fixIndex typ + 1) typ

  fixArgName' :: Int -> TypeSkeleton -> (Int, TypeSkeleton)
  fixArgName' idx (FunctionT x tArg tRes) =
    let (idx', tRes') = fixArgName' (if Text.null x then idx + 1 else idx) tRes
        x'            = Text.pack $ "arg" ++ show idx
    in  (idx', FunctionT (if Text.null x then x' else x) tArg tRes')
  fixArgName' idx t = (idx, t)
