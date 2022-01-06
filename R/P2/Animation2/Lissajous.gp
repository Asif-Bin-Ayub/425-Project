reset
# l=9.8
# tmax= 60.0d0 
# theta = pi/4d0 !rad
# phi   = pi/3d0 !rad
# omega = 0.1d0 !rad/s
# alpha = theta !rad/s


# set terminal qt
set terminal gif animate delay 1 size 1920, 1080 
set output "Lissajous.gif"
set title "Lissajous Figure l=9.8 (m) theta = pi/4d0 (rad) phi = pi/3d0 (rad) omega = 0.1d0 (rad/s) alpha = theta (rad/s)"

set view equal


set size square
set xrange [-10:10]
set yrange [-10:10]
set zrange [-10:10]



do for [i = 0:6001:25]{

    splot "trajectory.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 'gray40' title "",\
    "" using 2:3:4 every::0::i with lines lc 'black' title ""

}