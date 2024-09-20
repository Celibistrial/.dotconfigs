#!/usr/bin/env bash

SRC_FOLDER=~/data/
DST_FOLDER=~/hp-pav/

LOCAL_IP=deb1
REMOTE_IP=10.21.103.1

EXCLUDED_FOLDERS=(AI Music target .cache .direnv .Trash)

if ping -c 1 $LOCAL_IP &>/dev/null; then
  DST_IP=$LOCAL_IP
else
  DST_IP=$REMOTE_IP
fi
EXCLUDE_ARGS=""
for folder in "${EXCLUDED_FOLDERS[@]}"; do
  EXCLUDE_ARGS+="--exclude=$folder "
done

rsync -avz -e ssh $EXCLUDE_ARGS $SRC_FOLDER/ $DST_IP:$DST_FOLDER/
