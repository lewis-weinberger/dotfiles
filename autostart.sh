#!/bin/bash

# Wallpaper
feh --bg-fill ~/Pictures/enso.png

# Start emacs
emacs --daemon

# Start xscreensaver
xscreensaver &

# Status bar
while true; do
    WIFI="$(iw wlp3s0 station dump | awk '/signal:/{ print $2"dBm" }')"
    CPU="$(mpstat | awk '$12 ~ /[0-9.]+/{ printf("%.1f%"), 100 - $12 }')"
    TIME="$(date +"%H:%M:%S")"
    DATE="$(date +"%d/%m/%Y")"
    MEM="$(free | awk '/Mem/{ printf("%.1f%"), $3/$2 * 100.0 }')"
    xsetroot -name "[CPU $CPU] [MEM $MEM] [NET $WIFI] [$TIME $DATE]"
    sleep 1
done
