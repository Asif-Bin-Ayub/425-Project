reset
# theta0=135deg
# l=5

set terminal gif animate delay 1 size 1920, 1080 
set output "A.gif"
set title "Theta0=135 (deg) l=5 q=0 Fd=0"


set size square
set xrange [-25:25]
set yrange [-10:40]


do for [i = 0:6001:5]{

    plot "P1A.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'red' title "PATH 1",\
    "" using 1:2 with lines lc 'red' title "",\
    "P2A.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'blue' title "PATH 2",\
    "" using 1:2 with lines lc 'blue' title ""

}