reset
# l=9.8
# tmax= 60.0d0 
# theta = pi/5d0 !rad
# phi   = pi/4d0 !rad
# omega = pi/3d0 !rad/s
# alpha = theta/2d0 !rad/s


############1111111111###############
#q=0.25
############2222222222###############
# q=0.5
############3333333333###############
# q=1
###########0000000000###############


# set terminal qt
set terminal gif animate delay 1 size 1920, 1080 
set output "q.gif"
set title "Different damping coofeicents (theta=pi/5 | phi=pi/4 | omega = pi/3 | alpha = theta/2 | tmax=60 | l=9.8)"

set view equal


set size square
set xrange [-10:10]
set yrange [-10:10]
set zrange [-10:10]



do for [i = 0:6001:25]{

    splot "q005.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc '#BC000E' title "q=0.05",\
    "" using 2:3:4 every::0::i with lines lc '#BC000E' title "",\
    "q0075.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 'steelblue' title "q=0.075",\
    "" using 2:3:4 every::0::i with lines lc 'steelblue' title "",\
    "q01.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 'dark-green' title "q=0.1",\
    "" using 2:3:4 every::0::i with lines lc 'dark-green' title "",\
    "q0.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 'black' title "q=0",\
    "" using 2:3:4 every::0::i with lines lc 'black' title ""

}