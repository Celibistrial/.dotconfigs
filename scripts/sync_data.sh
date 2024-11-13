#!/usr/bin/env bash

SRC_FOLDER=~/data/
DST_FOLDER=~/hp-pav/

#LOCAL_IP=deb1
#REMOTE_IP=10.21.103.1

EXCLUDED_FOLDERS=(AI Music target .cache .direnv .Trash .Trash-1000)

#if ping -c 1 $LOCAL_IP &>/dev/null; then
#  DST_IP=$LOCAL_IP
#else
#  DST_IP=$REMOTE_IP
#fi
DST_IP=deb1
EXCLUDE_ARGS=""
for folder in "${EXCLUDED_FOLDERS[@]}"; do
  EXCLUDE_ARGS+="--exclude=$folder "
done

rsync -avz --delete -e ssh $EXCLUDE_ARGS $SRC_FOLDER/ $DST_IP:$DST_FOLDER/
