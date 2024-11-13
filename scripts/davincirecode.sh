#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash zenity ffmpeg
file=$(zenity --file-selection)
extension="${file##*.}"
ffmpeg -y -i "$file" -stats -hide_banner -loglevel panic -acodec pcm_s16le -vcodec copy "${file/%$extension/mov}"

