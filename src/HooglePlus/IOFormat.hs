module HooglePlus.IOFormat(
    decodeInput,
    encodeOutput
    ) where

import Types.IOFormat

import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson.Encode.Pretty as AP

-- parse the input json string
-- includes: type query, examples
decodeInput :: ByteString -> QueryInput
decodeInput bs = case mbInput of
                   Just i -> i
                   Nothing -> error "error parsing input json string"
    where 
        mbInput = A.decode bs :: Maybe QueryInput

-- output the result into a json string
-- includes: solutions, each solution is an ast
-- accompanied with several examples
encodeOutput :: QueryOutput -> ByteString
encodeOutput out = A.encode out

instance Show QueryOutput where
    show = show . encodeOutput 
