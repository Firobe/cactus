#!/usr/bin/env gnuplot
reset

# Plot smoothed data
set xdata time
set timefmt '%s'
set format x '%s'
set table '/tmp/data-smoothed'
set samples 10000
plot '/home/pi/Projects/cactus/cactus.log' u 1:2 smooth bezier
unset table

# Terminal
set terminal qt enhanced lw 3
set style data lines

set xrange [time(0) - 24*60*60:]
set yrange [18.0:24.0]

# Time format
set xdata time
set timefmt '%s'
set format x "%d/%m %H:%M"

# Grid
set style line 100 lt 1 lc rgb "gray" lw 0.5
set style line 101 lt 0 lc rgb "gray" lw 0.5
set grid xtics ytics mytics ls 100, ls 101
set mytics 10
set grid  

# Plot colored smoothed data
plot "< awk \
    'BEGIN { current = 0 } \
    NR == FNR { timestamps[FNR] = $1; color[$1] = $3; next } \
    (current + 1) in timestamps && $1 > timestamps[current + 1] { current = current + 1} \
    { print $1 FS $2 FS color[timestamps[current]] }' \
    /home/pi/Projects/cactus/cactus.log /tmp/data-smoothed" \
    u 1:2:3 linecolor variable title 'Temperature', \
    '/home/pi/Projects/cactus/cactus.log' u 1:4 lc rgb "red" lw 0.5 dashtype 2 \
    title 'Goal'
pause 30
reread
