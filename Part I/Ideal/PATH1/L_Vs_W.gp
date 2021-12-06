reset
set terminal qt
set title "Path 1 for Th0=0.6 (rad)"
set xlabel "t (s)"
set ylabel "theta (rad)"
set yrange [-1:1]


plot 'P1L(10.)T(0.6).txt' using 1:2 with lines linewidth 2 title "l=10 (m)" ,\
 'P1L( 1.)T(0.6).txt' using 1:2 with lines linewidth 2 title "l=1 (m)" ,\
 'P1L(50.)T(0.6).txt' using 1:2 with lines linewidth 2 title "l=50 (m)"