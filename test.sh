# clear the log file
echo > log

echo "SAT encoding without refinement" >> log
for i in {10,20,30,40,50,60}
do
    SUM=0
    for j in {1..5}
    do
        START=$(($(date +%s%N)/1000000))
        # SECONDS=0
        timeout 100 stack exec -- synquid synthesis "a -> List (Maybe a) -> a" --path=PetriNet --cnt=$i --path-solver=SMTSolver --use-refine=$1 >/dev/null
        END=$(($(date +%s%N)/1000000))
        SUM=$((SUM+END-START))
    done
    DURATION=$(awk -v s=$SUM 'BEGIN { print s / 5000 }')
    echo -e "$i \t $DURATION"
    echo -e "$i \t $DURATION" >> log
done
