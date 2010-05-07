set terminal postscript eps
set output "shapes.eps"
set title "Raytracer"
set xlabel "Bildstorlek (pixlar)"
set ylabel "Tid (s)"
set grid
set log y
set log x
set key right bottom
plot "< sh extract.sh shapes.csv False"  using ($1*$1):2 title "Icke optimerad" with linespoints ,\
     "< sh extract.sh shapes.csv True"   using ($1*$1):2 title "Optimerad"      with linespoints ,\
     "< sh extract.sh shapescb.csv True" using ($1*$1):2 title "Optimerad med CB"  with linespoints
