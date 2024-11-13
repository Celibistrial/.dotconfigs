#!/usr/bin/env bash
#
nix-shell -p yt-dlp ffmpeg --run '
read -p "Enter the URL: " url

yt-dlp --merge-output-format mp4 -f "bestvideo+bestaudio[ext=m4a]/best" "$url"
'
