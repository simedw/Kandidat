set terminal postscript eps
set output "shapes.eps"
set title "Raytracer"
set xlabel "Square root of resolution"
set ylabel "Time (s)"
set grid
set log y
plot "< sh extract.sh shapes.csv False"  title "Not Optimised" with lines ,\
     "< sh extract.sh shapes.csv True"   title "Optimised"      with lines ,\
     "< sh extract.sh shapescb.csv True" title "CB Optimised" with lines
