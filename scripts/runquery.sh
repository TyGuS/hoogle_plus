#!/bin/bash

MODE=$1
SOLUTIONS=$2
USE_HOF=$3
QUERY=$4

stack exec -- synquid synthesis --path="PetriNet" "$USE_HOF" --use-refine=$MODE --sol-num=$SOLUTIONS "$QUERY"
