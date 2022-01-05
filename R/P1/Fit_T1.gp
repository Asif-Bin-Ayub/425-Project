reset
set terminal qt

set title "T vs Theta0"
set xlabel "Theta0 (rad)"
set ylabel "T (s)"


plot "T_vs_theta0_Path1_filter.txt" using 1:2 with points pointtype 7 pointsize 0.6 lt -1 title "Data points" 