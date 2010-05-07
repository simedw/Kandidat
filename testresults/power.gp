set terminal postscript eps
set output "power.eps"
set title "Power function"
set xlabel "Exponent"
set ylabel "Time (s)"
set grid
set log x
set log y
plot "< sh extract.sh power.csv False"  title "Not Optimised" with linespoints ,\
     "< sh extract.sh power.csv True"   title "Optimised"     with linespoints
