#!/bin/sh

lock='~/.dotconfigs/scripts/swaylock.sh'


swayidle -w \
  timeout 300 $lock \
  timeout 900 'systemctl suspend' \
  before-sleep $lock
