reset
# theta0=135deg


set terminal gif animate delay 1 size 1920, 1080 
set output "2BT135.gif"
set title "Path 2 Theta0=135 (deg) q=0 Fd=0"


set size square
set xrange [-25:25]
set yrange [-10:40]


do for [i = 0:6001:5]{

    plot "P2BL3.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'red' title "l=3",\
    "" using 1:2 with lines lc 'red' title "",\
    "P2BL5.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'blue' title "l=5",\
    "" using 1:2 with lines lc 'blue' title "",\
    "P2BL7.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'green' title "l=7",\
    "" using 1:2 with lines lc 'green' title ""

}