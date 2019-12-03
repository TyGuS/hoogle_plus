-- | Lexems of the Synquid language
module Synquid.Tokens where

import Types.Common
import Synquid.Util
import Data.Maybe
import Data.Map.Strict (Map, fromList)
import Data.Char

-- | Keywords
keywords :: [String]
keywords = ["data", "else", "error", "False", "if", "in", "inline",
  "let", "match", "measure", "predicate", "qualifier", "termination",
  "then", "True", "type", "with", "where"
  ]
  
-- | Other operators
otherOps :: [String]
otherOps = ["::", ":", "->", "|", "=", "??", ",", ".", "\\"]

-- | Characters allowed in identifiers (in addition to letters and digits)
identifierChars = "_'"
-- | Start of a multi-line comment
commentStart = "{-"
-- | End of a multi-line comment
commentEnd = "-}"
-- | Start of a single-line comment
commentLine = "--"

-- | 'isLiteral' @str@ : Is string @str@ a literal of a primitive type?
isLiteral str = isJust (asInteger str) || str == "True" || str == "False"

isTypeName str = isUpper $ head str
isIdentifier str = isLower $ head str
