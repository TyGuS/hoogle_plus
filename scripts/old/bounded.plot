set datafile separator ","
set xlabel "Seconds"
set ylabel "Benchmarks passed"
set yr [0:30]
set term png
set output "intialcover.png"
plot "tmp/plot.csv" using 2:1 with lp title "Tygar-Q", "tmp/plot.csv" using 3:1 with lp title "Tygar - 0", "tmp/plot.csv" using 4:1 with lp title "Tygar - HOF"
