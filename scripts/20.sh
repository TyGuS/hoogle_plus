#!/bin/bash
cd "$(dirname "$0")"
rm $TMPDIR/base.txt
./generate_test.sh test20.txt
