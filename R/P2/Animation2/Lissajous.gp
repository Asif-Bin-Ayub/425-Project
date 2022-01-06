reset
# l=9.8
# tmax= 60.0d0 


############1111111111###############
# theta = pi/4d0 !rad
# phi   = pi/3d0 !rad
# omega = 0.1d0 !rad/s
# alpha = theta !rad/s

############2222222222###############
# theta = pi/3d0 !rad
# phi   = pi/2d0 !rad
# omega = 0d0 !rad/s
# alpha = 0.5d0*theta !rad/s

############3333333333###############
# theta = pi/5d0 !rad
# phi   = pi/3d0 !rad
# omega = 0d0 !rad/s
# alpha = 0.25d0*theta !rad/s


# set terminal qt
set terminal gif animate delay 1 size 1920, 1080 
set output "Lissajous.gif"
set title "Lissajous Figure l=9.8 (m) tmax=60 (s)"

set view equal


set size square
set xrange [-10:10]
set yrange [-10:10]
set zrange [-10:10]



do for [i = 0:6001:25]{

    splot "LAS1.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc '#BC000E' title "theta=pi/4, phi=pi/3, omega=0.1, alpha=theta",\
    "" using 2:3:4 every::0::i with lines lc '#BC000E' title "",\
    "LAS2.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 'steelblue' title "theta=pi/3, phi=pi/2, omega=0, alpha=0.5theta",\
    "" using 2:3:4 every::0::i with lines lc 'steelblue' title "",\
    "LAS3.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 'dark-green' title "theta=pi/5, phi=pi/3, omega=0, alpha=0.25theta",\
    "" using 2:3:4 every::0::i with lines lc 'dark-green' title ""

}