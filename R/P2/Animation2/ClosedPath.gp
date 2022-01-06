reset
# l=9.8
# tmax= 60.0d0 
# theta = pi/2 !rad
# phi   = pi/4 !rad
# omega = 0.0d0 !rad/s
# alpha=0d0

reset

# set terminal qt
set terminal gif animate delay 1 size 1080, 1080 
set output "ClosedPath3.gif"
set title "Closed Path: Lissajous-like"

set view equal

set size square
set xrange [-10:10]
set yrange [-10:10]
set zrange [-10:10]



do for [i = 0:6001:25]{

    splot "trajectory.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 2 title "",\
    "" using 2:3:4 every::0::i with lines lc 2 title ""

}

set output