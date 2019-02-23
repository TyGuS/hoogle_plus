#!/bin/bash
cd "$(dirname "$0")"
rm /tmp/base.txt
./generate_test.sh test50.txt
