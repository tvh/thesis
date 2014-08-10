# vim: filetype=gnuplot

set title "Ray tracer"

set terminal pdf size 4,3
set output "ray-scale.pdf"

set key on
set key left

set xlabel "Number of CPUs"
set logscale x
set xtics  ("1" 1, "2" 2, "4" 4, "8" 8, "16" 16)

set ylabel "speedup/CPU"
set yrange [0.6:1.2]

plot    'ray-scale.dat' using ($1):($2)                                    \
                title "Repa"                                            \
                ls 7  lw 4 with linespoints,                            \
        'ray-scale.dat' using ($1):($3)                                    \
                title "Accelerate"                                      \
                ls 3  lw 4 with linespoints

