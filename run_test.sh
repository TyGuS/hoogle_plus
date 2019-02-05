#!/bin/bash

query_list=("Int64 -> ByteString" "List (Maybe Bool) -> Bool" "a -> List (Maybe a) -> a" "(a -> b) -> Pair a a -> Pair b b")


for i in 100
do
    testPath="./test/test$i.txt"
    cp $testPath /tmp/base.txt
    if [ $i -eq 10 ]
    then
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.Tuple
    elif [ $i -eq 20 ]
    then
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.Tuple
    elif [ $i -eq 50 ]
    then
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.Map.Strict -m Data.List -m Data.Tuple
    else
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.Map.Strict -m Data.List -m Data.Tuple -m Data.Set -m GHC.List -m GHC.Char
    fi
    for q in "${query_list[@]}"    
    do
        for t in {1..5}
        do
            time stack exec -- synquid synthesis "$q" --path=PetriNet --use-refine=AbstractRefinement >> time.log
        done
    done
done
