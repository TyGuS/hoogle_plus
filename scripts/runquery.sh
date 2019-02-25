#!/bin/bash

MODE=$1
SOLUTIONS=$2
QUERY=$3

stack exec -- synquid synthesis --path="PetriNet" --use-refine=$MODE --sol-num=$SOLUTIONS "$QUERY"
