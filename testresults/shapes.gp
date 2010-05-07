set terminal postscript eps
set output "shapes.eps"
set title "Raytracer"
set xlabel "Pixlar"
set ylabel "Tid (s)"
set grid
set log y
plot "< sh extract.sh shapes.csv False"  using ($1*$1):2 title "Icke optimerad" with lines ,\
     "< sh extract.sh shapes.csv True"   using ($1*$1):2 title "Optimerad"     with lines ,\
     "< sh extract.sh shapescb.csv True" using ($1*$1):2 title "Optimerad med CB"  with lines
