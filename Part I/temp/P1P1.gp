reset
# l=1 and theta0=45deg

set terminal gif animate delay 1
set output "Path1.gif"

set xrange [-1.6 to 1.6]
set yrange [-2.1 to 1.1]

set size square

set title "Path 1 ideal"
set xlabel "x(m)"
set ylabel "y(m)"

do for [i = 0:15000:10]{

    plot "trajectory_Path1.txt" using 4:5 every::i::i with points ps 2 pt 7 lc 'red' title "",\
    "" using 4:5 with lines lc -1 title ""

}