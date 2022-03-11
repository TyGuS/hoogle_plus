-- | Lexems of the Synquid language
module Synquid.Tokens where

import Types.Common
import Synquid.Util
import Data.Maybe
import Data.Map (Map, fromList)
import Data.Char

-- | Other operators
operators :: [String]
operators = ["::", ":", "->", "|", "=", "??", ",", ".", "\\"]

-- | Characters allowed in identifiers (in addition to letters and digits)
identifierChars = "_'"
-- | Start of a multi-line comment
commentStart = "{-"
-- | End of a multi-line comment
commentEnd = "-}"
-- | Start of a single-line comment
commentLine = "--"

isTypeName str = isUpper $ head str
isIdentifier str = isLower $ head str
