reset

set terminal gif animate delay 1
set output "Path2.gif"

set xrange [-1.6 to 1.6]
set yrange [-1.6 to 1.6]

set size square

set title "Path 2 ideal"
set xlabel "x(m)"
set ylabel "y(m)"

do for [i = 0:15000:10]{

    plot "trajectory_Path2.txt" using 4:5 every::i::i with points ps 2 pt 7 lc 'blue' title "",\
    "" using 4:5 with lines lc -1 title ""

}