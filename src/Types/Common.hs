module Types.Common where

{-
This is a place for any top-level types that will be used by many modules.
This should have no dependencies on any other files in this project. This should be
the Top Level module.
-}
import           Data.Text                      ( Text )

type Id = Text
type GroupId = Text

varName :: Text
varName = "_v"
