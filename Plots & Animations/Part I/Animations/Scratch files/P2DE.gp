reset
# theta0=135deg
# l=5

set terminal gif animate delay 1 size 1920, 1080 
set output "2DE.gif"
set title "Theta0=135 (deg) l=5 Fd=0"


set size square
set xrange [-25:25]
set yrange [-10:40]


do for [i = 0:6001:5]{

    plot "P2E1.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'red' title "q=1",\
    "P2D3.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'blue' title "q=3",\
    "P2E03.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'green' title "q=0.3",\
    "P2A.txt" using 1:2 with lines lc 'black' title ""

}