reset
set terminal qt
set title "Path 1 For l=10 (m)"
set xlabel "t (s)"
set ylabel "theta (rad)"

plot 'P1L(10.)T(0.1).txt' using 1:2 with lines title "Th_0=0.1 (rad)",\
'P1L(10.)T(0.6).txt' using 1:2 with lines title "Th_0=0.6 (rad)",\
'P1L(10.)T(1.1).txt' using 1:2 with lines title "Th_0=1.1 (rad)",\
'P1L(10.)T(1.6).txt' using 1:2 with lines title "Th_0=1.6 (rad)",\
'P1L(10.)T(2.1).txt' using 1:2 with lines title "Th_0=2.1 (rad)",\
'P1L(10.)T(2.6).txt' using 1:2 with lines title "Th_0=2.6 (rad)",\
'P1L(10.)T(3.1).txt' using 1:2 with lines title "Th_0=3.1 (rad)"