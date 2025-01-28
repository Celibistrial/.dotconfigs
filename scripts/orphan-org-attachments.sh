#!/usr/bin/env sh

#!/bin/sh

## Location where org-mode stores attachments
datadir="$HOME/org/data";
#datadir="$HOME/org/roam/data";
orgdir="$HOME/org/"
#orgdir="$HOME/org/roam/"

echo "The following files appear orphaned:";

files=$(find "$datadir" -type f|perl -ne 'print "$1\n" if /([^\/]*)\/[^\/]*$/'|uniq|while read id; do grep -qiR --include "*.org" --include "*.org_archive" "$id" "$orgdir" || find "$datadir" -ipath "*$id*" -type f; done)

echo "$files"

if [ "" == "$files" ]; then
   echo "Nothing to do!"
   exit
fi

echo "Delete? y/[n]"
read delete
case $delete in
    y)
        echo "$files" |
        while read fn; do
        rm "$fn";
        done
    echo "Done."
        ;;
    *)
        echo "Not deleting anything!"
        ;;
esac
find $datadir -depth -type d -empty -delete

