#!/bin/bash

STACK=stack

$STACK build
$STACK install hoogle
hoogle generate
