#!/bin/sh
while true; do
  mlton -output n2o.exe n2o.mlb
  inotifywait -ecreate -edelete -emodify --exclude '\.\#.*$' -q ./src n2o.mlb
done 
