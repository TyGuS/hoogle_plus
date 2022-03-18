module Compiler.Error
  ( Pos(..)
  , SourcePos
  , sourceLine
  , sourceColumn
  , sourceName
  , ErrorKind(..)
  , ErrorMessage(..)
  ) where

import           Text.Parsec.Pos                ( SourcePos
                                                , sourceColumn
                                                , sourceLine
                                                , sourceName
                                                )
import           Text.PrettyPrint.ANSI.Leijen   ( Doc )

-- | Anything with a source position attached 
data Pos a = Pos
  { position :: SourcePos
  , node     :: a
  }
  deriving (Eq, Ord)

data ErrorKind = ParseError | ResolutionError | TypeError

data ErrorMessage = ErrorMessage
  { emKind        :: ErrorKind
  , emDescription :: Doc
  }
