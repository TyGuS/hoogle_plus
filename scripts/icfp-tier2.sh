#!/bin/bash

cp libraries/{ghc-prim,base,bytestring,deepseq}.txt $TMPDIR

stack exec -- synquid generate -h\
      -p base \
      -m "Data.Int" \
    -m "Data.Maybe" \
    -m "Data.Either" \
    -m "GHC.List" \
    -m "GHC.Char" \
    -m "Text.Show" \
    -p bytestring \
    -m "Data.ByteString.Lazy" \
    -m "Data.ByteString.Builder"
