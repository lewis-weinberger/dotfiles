#!/bin/bash

# Wallpaper
feh --bg-fill ~/Pictures/enso.png

# Start emacs
emacs --daemon

# Show date and time
while true; do
    xsetroot -name "$(date +"%H:%M:%S %d/%m/%Y")"
    sleep 1
done
