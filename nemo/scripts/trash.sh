#!/bin/bash
# for item in "$@"
#   do
#     #notify-send "$item"
#     trash-put $item
#   done
# Loop through each argument
for item in "$@"; do
  # Remove any backslashes before spaces
  item="${item//\\ / }"
  trash-put "$item"
done
