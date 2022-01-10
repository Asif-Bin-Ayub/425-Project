reset
# l=9.8
# tmax= 60.0d0 
# theta = pi/3d0 
# phi   = 0.0d0 
# omega = 0.0d0 
# alpha = sqrt(1/cos(theta)) 


############1111111111###############
#q=0.05
############2222222222###############
# q=0.075
############3333333333###############
# q=0.1
###########0000000000###############


# set terminal qt
set terminal gif animate delay 1 size 1080, 1080 
set output "Cq.gif"
set title "Different damping coofeicents (theta=pi/3 | phi=0 | omega = 0 | alpha = sqrt(1/cos(theta)) | tmax=60 | l=9.8)"

set view equal


set size square
set xrange [-10:10]
set yrange [-10:10]
set zrange [-10:10]



do for [i = 0:6001:25]{

    splot "CQ5.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc '#BC000E' title "q=0.05",\
    "" using 2:3:4 every::0::i with lines lc '#BC000E' title "",\
    "CQ75.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 'steelblue' title "q=0.075",\
    "" using 2:3:4 every::0::i with lines lc 'steelblue' title "",\
    "CQ1.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 'dark-green' title "q=0.1",\
    "" using 2:3:4 every::0::i with lines lc 'dark-green' title "",\
    "CQ0.txt" using 2:3:4 every::i::i with points ps 5 pt 7 lc 'black' title "q=0",\
    "" using 2:3:4 every::0::i with lines lc 'black' title ""

}