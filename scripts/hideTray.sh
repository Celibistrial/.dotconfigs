#!/bin/bash

TRAY_NAME="stalonetray"

while true; do
    # Get the current workspace
    CURRENT_WORKSPACE=$(i3-msg -t get_workspaces | jq -r '.[] | select(.focused == true) | .num')

    # Get the number of windows on the current workspace
    NUM_WINDOWS=$(i3-msg -t get_tree | jq -r --arg ws "$CURRENT_WORKSPACE" \
        '.nodes[] | select(.type == "workspace" and .num == ($ws | tonumber)) | .nodes[].nodes | length')

    # Debugging output
    echo "CURRENT_WORKSPACE: $CURRENT_WORKSPACE"
    echo "NUM_WINDOWS: $NUM_WINDOWS"

    # Check if $NUM_WINDOWS is not empty and is greater than 0
    if [ -n "$NUM_WINDOWS" ] && [ "$NUM_WINDOWS" -gt 0 ]; then
        # If there are windows, hide the tray
        xdotool search --class "$TRAY_NAME" windowunmap
    else
        # If no windows, unhide the tray
        xdotool search --class "$TRAY_NAME" windowmap
    fi

    # Sleep for a short interval to avoid high CPU usage
    sleep 1
done

