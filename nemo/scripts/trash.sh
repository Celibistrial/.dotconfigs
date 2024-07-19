#!/bin/bash
for item in "$@"
  do
    #notify-send "$item"
    trash-put $item
  done
