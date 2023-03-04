{-# LANGUAGE UndecidableInstances #-}

module Data.Text.Extended.Pretty (
    Pretty(..)
  ) where

import           Data.Text ( Text )
import qualified Data.Text as Text

----------------------------------------------------------------------

class Pretty a where
  pretty :: a -> Text

instance {-# OVERLAPPABLE #-} (Show a) => Pretty a where
  pretty = Text.pack . show