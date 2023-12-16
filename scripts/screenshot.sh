#!/bin/bash
 
SAVEDIR=~/Pictures
mkdir -p -- "$SAVEDIR"
FILENAME="$SAVEDIR/$(date +'%Y-%m-%d-%H%M%S_screenshot.png')"
EXPENDED_FILENAME="${FILENAME/#\~/$HOME}"
selected_region="$(slurp)"
if [ -z "$selected_region" ]; then
    # No region was selected, so exit the script
    exit 1
fi
grim -g "$selected_region" "$EXPENDED_FILENAME"
swappy -f "$EXPENDED_FILENAME" -o "$EXPENDED_FILENAME"
 
wl-copy < "$EXPENDED_FILENAME"
notify-send "Screenshot" "File saved as <i>'$FILENAME'</i> and copied to the clipboard." -i "$EXPENDED_FILENAME"
