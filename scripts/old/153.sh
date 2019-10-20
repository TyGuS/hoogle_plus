#!/bin/bash

cp libraries/{ghc-prim,base,bytestring,deepseq}.txt $TMPDIR

stack exec -- synquid generate \
      -p base \
      -m "Data.Int" \
    -m "Data.Bool" \
    -m "Data.Maybe" \
    -m "Data.Tuple" \
    -m "GHC.Char" \
    -m "Text.Show" \
    -p bytestring \
    -m "Data.ByteString.Lazy" \
    -m "Data.ByteString.Builder"
