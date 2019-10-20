#!/bin/bash

cp libraries/tier1/{ghc-prim,base,bytestring,deepseq}.txt $TMPDIR

stack exec -- hplus generate \
      -p base \
      -m "Data.Int" \
    -m "Data.Bool" \
    -m "Data.Maybe" \
    -m "GHC.Char" \
    -m "Text.Show" \
    -p bytestring \
    -m "Data.ByteString.Lazy" \
    -m "Data.ByteString.Builder"
