reset
set terminal qt
set title "Path 1"
set xlabel "t (s)"
set ylabel "theta (rad)"
set yrange [-3.5:3.5]


plot 'P1L( 1.)T(0.1).txt' using 1:2 with lines linewidth 2 title "l=1 (m), Th_0=0.1 (rad)" ,\
 'P1L(50.)T(0.1).txt' using 1:2 with lines linewidth 2 title "l=50 (m), Th_0=0.1 (rad)",\
 'P1L( 1.)T(1.6).txt' using 1:2 with lines linewidth 2 title "l=1 (m), Th_0=1.6 (rad)" ,\
 'P1L(50.)T(1.6).txt' using 1:2 with lines linewidth 2 title "l=50 (m), Th_0=1.6 (rad)",\
 'P1L( 1.)T(3.1).txt' using 1:2 with lines linewidth 2 title "l=1 (m), Th_0=3.1 (rad)" ,\
 'P1L(50.)T(3.1).txt' using 1:2 with lines linewidth 2 title "l=50 (m), Th_0=3.1 (rad)"