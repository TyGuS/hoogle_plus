#!/bin/bash

cp libraries/{ghc-prim,base,bytestring,deepseq}.txt $TMPDIR

stack exec -- synquid generate \
      -p base \
      -m "Data.Bits" \
      -m "Data.Bool" \
      -m "Data.Char" \
      -m "Data.Data" \
      -m "Data.Either" \
      -m "Data.Eq" \
      -m "Data.Int" \
      -m "Data.List" \
      -m "Data.List.NonEmpty" \
      -m "Data.Maybe" \
      -m "Data.Ratio" \
      -m "Data.Tuple" \
      -m "Data.Word" \
      -m "Data.String" \
      -m "Numeric" \
      -m "Text.Read" \
      -m "Text.Show" \
      -p bytestring \
      -m "Data.ByteString.Lazy" \
      -m "Data.ByteString.Lazy.Builder" \
      -m "Data.ByteString.Lazy.Builder.ASCII" \
      -m "Data.ByteString.Lazy.Builder.Extras" \
      -m "Data.ByteString.Lazy.Char8"
