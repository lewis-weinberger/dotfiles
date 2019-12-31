#!/bin/bash

# Wallpaper
feh --bg-fill ~/Pictures/enso.png

# Start emacs
emacs --daemon

# Start xscreensaver
xscreensaver &

# Status bar
while true; do
    WIFI="$(iw wlp3s0 station dump | grep "signal:" | awk '$2 { print $2 "dBm" }')"
    CPU="$(mpstat | awk '$12 ~ /[0-9.]+/ { print 100 - $12"%" }')"
    TIME="$(date +"%H:%M:%S")"
    DATE="$(date +"%d/%m/%Y")"
    xsetroot -name "[📶 $WIFI] [🖳 $CPU] [🕑 $TIME] [📅 $DATE]"
    sleep 1
done
