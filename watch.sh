#!/bin/sh
while true; do
    mlton -output n2o.exe -default-ann 'allowVectorExps true' n2o.mlb
    ./n2o.exe &
    PID=$!
    inotifywait -ecreate -edelete -emodify --exclude '\.\#.*$' -q ./src n2o.mlb
    kill $PID
done
