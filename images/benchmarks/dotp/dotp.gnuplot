# vim: filetype=gnuplot

set title "Dot product"

set terminal pdf size 4,3
set output "dotp.pdf"

set key on
set key bottom

set xlabel "Elements (millions)"
set logscale x
set xrange [1.5:25]

set xtics (2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

set ylabel "Run Time (ms)"
set logscale y
#set yrange [0.05:100]

plot    'dotp.dat' using ($1):($2)                                              \
                title "Data.Vector"        ls 1  lw 4 with linespoints,         \
        'dotp.dat' using ($1):($3)                                              \
                title "Repa"               ls 2  lw 4 with linespoints,         \
        'dotp.dat' using ($1):($4)                                              \
                title "Accelerate (compile)"         ls 3  lw 4 with linespoints,         \
        'dotp.dat' using ($1):($5)                                              \
                title "Accelerate (execute)"        ls 8  lw 4 with linespoints,        \
        'dotp.dat' using ($1):($4+$5)                                              \
                title "Accelerate (complete)"             ls 16  lw 4 with linespoints

#        'dotp.dat' using ($1):($7)                                              \
#                title "NDP2GPU"            ls 4  lw 4 with linespoints,         \
