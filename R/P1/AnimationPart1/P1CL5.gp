reset
# l=5


set terminal gif animate delay 1 size 1920, 1080 
set output "1CL5.gif"
set title "Path 1 l=5 q=0 Fd=0"


set size square
set xrange [-25:25]
set yrange [-10:40]


do for [i = 0:6001:5]{

    plot "P1CTHETA30.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'red' title "theta0=30",\
    "" using 1:2 with lines lc 'red' title "",\
    "P1CTHETA90.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'blue' title "theta0=90",\
    "" using 1:2 with lines lc 'blue' title "",\
    "P1CTHETA150.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'green' title "theta0=150",\
    "" using 1:2 with lines lc 'green' title ""

}