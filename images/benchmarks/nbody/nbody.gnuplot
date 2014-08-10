# vim: filetype=gnuplot

set title "N-Body"

set terminal pdf size 4,3
set output "nbody.pdf"

set key on
set key at 3500,8000

set xlabel "Bodies"
set logscale x
set xrange [500:32000]

set xtics (500, 1000, 2000, 4000, 8000,16000, 32000)

set ylabel "Run Time (ms)"
set logscale y
set yrange [0.1:10000]

plot    'nbody.dat' using ($1):($2)                                     \
                title "... -N16"                                     \
                ls 1  lw 4 with linespoints,                            \
        'nbody.dat' using ($1):($3)                                     \
                title "... -N8"                                     \
                ls 2  lw 4 with linespoints,                            \
        'nbody.dat' using ($1):($4)                                     \
                title "... -N1"                                  \
                ls 3  lw 4 with linespoints,                           \
        'nbody.dat' using ($1):($5)                                     \
                title "... -N1, no vect."                                  \
                ls 4  lw 4 with linespoints,                           \
        'nbody.dat' using ($1):($6)                                     \
                title "Data.Vector"                                     \
                ls 8  lw 4 with linespoints

