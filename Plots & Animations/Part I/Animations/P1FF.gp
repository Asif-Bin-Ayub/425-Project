reset
# theta0=135deg
# l=5
# q=0.3
# wd=1.5

set terminal gif animate delay 1 size 1920, 1080 
set output "1FF.gif"
set title "Theta0=135 (deg) l=5 q=0.3 omega_d=1.5"


set size square
set xrange [-25:25]
set yrange [-10:40]


do for [i = 0:6001:5]{

    plot "P1FF1.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'red' title "F=1",\
    "P1FF2.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'blue' title "F=2",\
    "P1FF3.txt" using 1:2 every::i::i with points ps 5 pt 7 lc 'green' title "F=3",\
    "P1A.txt" using 1:2 with lines lc 'black' title ""

}