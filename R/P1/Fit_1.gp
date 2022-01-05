reset
set terminal qt

set title "T vs l PATH1"
set xlabel "l"
set ylabel "T"

f(x)=a1*log(x+b1)-a1*log(b1)
g(x)=a2/(x+b2)-a2/b2
h(x)=a3*sqrt(x)


fit f(x) "T_vs_L_Path1_filter.txt" using 1:2 via a1,b1
fit g(x) "T_vs_L_Path1_filter.txt" using 1:2 via a2,b2
fit h(x) "T_vs_L_Path1_filter.txt" using 1:2 via a3

plot "T_vs_L_Path1_filter.txt" using 1:2 with points pointtype 7 pointsize 0.6 lt -1 title "Data points",\
h(x) lc rgb 'green' title "h(x)=a*sqrt(x+b)+c",\
f(x) lc rgb 'blue' title "f(x)=a*log(x+b)+c",\
g(x) lc rgb 'red' title "g(x)=a/(x+b)+c"