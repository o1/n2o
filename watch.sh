#!/bin/sh
while true; do
  mlton n2o.mlb
  inotifywait --exclude '\.\#.*$' ./src n2o.mlb
done 
