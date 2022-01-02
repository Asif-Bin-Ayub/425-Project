reset
set terminal qt

plot "trajectory_path1.txt" using 1:2 with lines title "P1",\
     "trajectory_path2.txt" using 1:2 with lines title "P2"
