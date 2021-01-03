#!/usr/bin/env gnuplot
reset
set terminal qt enhanced lw 4
#set terminal dumb
set style data lines
set yrange [15.0:25.0]
plot 'cactus.log' u 1:2:3 linecolor variable
pause mouse close
