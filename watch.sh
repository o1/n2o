#!/bin/sh
while true; do
  mlton -output n2o.exe n2o.mlb
  inotifywait --exclude '\.\#.*$' ./src n2o.mlb
done 
