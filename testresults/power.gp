set terminal postscript eps
set output "power.eps"
set title "Powerfunktionen"
set xlabel "Exponent"
set ylabel "Tid (s)"
set grid
set log x
set log y
set key right bottom
plot "< sh extract.sh power.csv False"  title "Icke optimerad" with linespoints ,\
     "< sh extract.sh power.csv True"   title "Optimerad"     with linespoints
