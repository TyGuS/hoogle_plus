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
decodeInput :: ByteString -> Input
decodeInput bs = case mbInput of
                   Just i -> i
                   Nothing -> error "error parsing input json string"
    where 
        mbInput = A.decode bs :: Maybe Input

-- output the result into a json string
-- includes: solutions, each solution is an ast
-- accompanied with several examples
encodeOutput :: Output -> ByteString
encodeOutput out = AP.encodePretty out
