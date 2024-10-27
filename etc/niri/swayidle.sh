#!/bin/sh
WALLPAPER=${HOME}/media/tokyo_gentoo.jpg
swayidle -w \
    timeout 300 "swaylock -f -i ${WALLPAPER}" \
    timeout 600 'niri msg action power-off-monitors' \
    before-sleep "swaylock -f -i ${WALLPAPER}"
