#!/usr/bin/env bash

# Only exported variables can be used within the timer's command.
export PRIMARY_DISPLAY="$(xrandr | awk '/ primary/{print $1}')"
pkill xidlehook
# Run xidlehook
xidlehook \
  `# Don't lock when there's a fullscreen application` \
  --not-when-fullscreen \
  `# Don't lock when there's audio playing` \
  --not-when-audio \
  `# Dim the screen after 5 minutes, undim if user becomes active` \
  --timer 360 \
    'xrandr --output "$PRIMARY_DISPLAY" --brightness .1' \
    'xrandr --output "$PRIMARY_DISPLAY" --brightness 1' \
  `# Undim & lock after 10 more seconds` \
  --timer 10 \
    'xrandr --output "$PRIMARY_DISPLAY" --brightness 1;betterlockscreen --lock dimblur' \
    '' \
  `# Finally, suspend 5 minutes after it locks` \
  --timer 360 \
    'xrandr --output "$PRIMARY_DISPLAY" --brightness 1 && systemctl suspend' \
    ''
