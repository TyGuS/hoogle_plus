# clear the log file
echo > log

echo "SAT encoding without refinement" >> log
for i in {1..9}
do
    SUM=0
    for j in {1..5}
    do
        START=$(($(date +%s%N)/1000000))
        # SECONDS=0
        timeout 100 stack exec -- synquid synthesis "a -> List (Maybe a) -> a" --path=$1 --cnt=$i -p $2 --encoder=$3 --use-refine=$4 > /dev/null 2> /dev/null
        END=$(($(date +%s%N)/1000000))
        SUM=$((SUM+END-START))
    done
    DURATION=$(awk -v s=$SUM 'BEGIN { print s / 5000 }')
    echo -e "$i \t $DURATION"
    echo -e "$i \t $DURATION" >> log
done
