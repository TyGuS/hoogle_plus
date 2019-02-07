#!/bin/bash

query_list=("Int64 -> ByteString" "List (Maybe Bool) -> Bool" "a -> List (Maybe a) -> a")


for i in 100
do
    testPath="./test/test$i.txt"
    echo $testPath
    cp $testPath /tmp/base.txt
    if [ $i -eq 10 ]
    then
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.Tuple -m GHC.List
    elif [ $i -eq 20 ]
    then
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.Tuple -m GHC.List
    elif [ $i -eq 50 ]
    then
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.List -m Data.Tuple -m GHC.List
    else
        stack exec -- synquid generate -p base -m Data.Word -m Data.Int -m Data.String -m Data.Maybe -m Data.ByteString.Builder -m Data.ByteString.Lazy -m Data.List -m Data.Tuple -m GHC.List -m Prelude -m GHC.Char -m Data.Char -m Text.Show
    fi
    for q in "${query_list[@]}"    
    do
        for t in {1..5}
        do
            time stack exec -- synquid synthesis "$q" --path=PetriNet --use-refine=AbstractRefinement
        done
    done
done
