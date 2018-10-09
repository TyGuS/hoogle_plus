module TypeOf where

import Control.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

getStaticType :: Name -> Q Exp
getStaticType = lift <=< fmap pprint . reify



-- https://stackoverflow.com/questions/10399685/haskell-getting-the-static-type-of-an-expression
