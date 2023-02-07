#!/bin/sh

if [ "$DESKTOP_SESSION" = "i3-gnome" ]; then 
   sleep 20s
   killall conky
   cd "$HOME/.conky/nordcore"
   conky -c "$HOME/.conky/nordcore/conkyrc2core" &
   exit 0
fi
