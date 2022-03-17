module Compiler.Error (
  Pos(..)
  ,SourcePos
  ,sourceLine
  ,sourceColumn
  ,sourceName  
  ,noPos
  ,ErrorKind(..)
  ,ErrorMessage(..)
  ) where

import Text.PrettyPrint.ANSI.Leijen ( Doc )
import Text.Parsec.Pos

-- | Anything with a source position attached 
data Pos a = Pos {
      position :: SourcePos,
      node :: a
    } deriving(Eq, Ord)
    
-- | Dummy source position
noPos = (initialPos "<no file name>")    

data ErrorKind = ParseError | ResolutionError | TypeError

data ErrorMessage = ErrorMessage {
  emKind :: ErrorKind,
  emPosition :: SourcePos,
  emDescription :: Doc
}