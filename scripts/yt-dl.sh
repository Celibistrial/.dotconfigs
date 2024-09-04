#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg yt-dlp

# Check if the user provided an argument
if [ -z "$1" ]; then
    echo "Usage: $0 <URL>"
    exit 1
fi

# Use yt-dlp to download the video and audio, merge them, and save as mp4
yt-dlp --merge-output-format mp4 -f "bestvideo+bestaudio[ext=m4a]/best" "$1"
