#!/bin/bash

radio="$(nmcli radio all | awk 'FNR == 2 {print $2}')"

if [ "$radio" = "enabled" ]; then
    nmcli radio all off
else
    nmcli radio all on
fi

if rfkill list bluetooth | grep -q 'yes$'; then
    rfkill unblock bluetooth
else
    rfkill block bluetooth
fi
