#!/usr/bin/env bash

# Add this script to your wm startup file.

DIR="$HOME/.config/polybar/shapes"
~/scripts/walpaper
# Launch the bar
polybar -q main -c "$DIR"/config.ini &
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
polybar -q main -c "$DIR"/config.ini &

