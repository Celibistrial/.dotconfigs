#!/bin/bash
while true
do
  nbfc status -a | grep "Temperature" | sed 's/.*: //' && sleep 10
done
