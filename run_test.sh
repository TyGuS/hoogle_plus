#!/bin/bash

query_list=("Int64 -> ByteString" "List (Maybe Bool) -> Bool" "a -> List (Maybe a) -> a" "(a -> b) -> Pair a a -> Pair b b")


for i in 10 20 50 100 155
do
    testPath="./test/test$i.txt"
    echo $testPath
    if [ $i -eq 10 ]
    then
        cp $testPath /tmp/base.txt
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.Tuple -m GHC.List
    elif [ $i -eq 20 ]
    then
        cp $testPath /tmp/base.txt
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.Tuple -m GHC.List
    elif [ $i -eq 50 ]
    then
        cp $testPath /tmp/base.txt
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.List -m Data.Tuple -m GHC.List
    elif [ $i -eq 100 ]
    then
        cp $testPath /tmp/base.txt
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.List -m Data.Tuple -m GHC.List -m Prelude -m GHC.Char -m Data.Char -m Text.Show
    else
        rm /tmp/base.txt
        stack exec -- synquid generate -p base -m Data.Bool    -m Data.Maybe    -m GHC.Char    -m Text.Show    -p bytestring -m Data.ByteString    -m Data.ByteString.Builder -m Data.Tuple
    fi
    for q in "${query_list[@]}"    
    do
        for t in 1
        do
            time stack exec -- synquid synthesis "$q" --path=PetriNet --use-refine=$1
        done
    done
done
