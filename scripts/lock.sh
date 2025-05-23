#!/usr/bin/env bash

# Only exported variables can be used within the timer's command.
export PRIMARY_DISPLAY="$(xrandr | awk '/ primary/{print $1}')"
pkill xidlehook

# Run xidlehook
xidlehook \
  --socket "/tmp/xidlehook.socket" \
  `# Don't lock when there's a fullscreen application` \
  --not-when-fullscreen \
  `# Don't lock when there's audio playing` \
  --not-when-audio \
  `# Dim the screen after 5 minutes, undim if user becomes active` \
  --timer 240 \
  'xrandr --output "$PRIMARY_DISPLAY" --brightness .1' \
  'xrandr --output "$PRIMARY_DISPLAY" --brightness 1' \
  `# Undim & lock after 60 more seconds` \
  --timer 10 \
  'xrandr --output "$PRIMARY_DISPLAY" --brightness 1;betterlockscreen --lock dimblur' \
  '' \
  `# Finally, suspend 3 minutes after it locks` \
  --timer 180 \
  'xrandr --output "$PRIMARY_DISPLAY" --brightness 1 && systemctl suspend' \
  ''
