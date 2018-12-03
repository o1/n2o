#!/bin/sh
while true; do
    # mlton -output n2o.exe -default-ann 'allowVectorExps true' n2o.mlb
    ml-build n2o.cm Main.main n2o.nj
    sml @SMLload=n2o.nj &
    # ./n2o.exe &
    PID=$!
    inotifywait -ecreate -edelete -emodify --exclude '\.\#.*$' -q ./src n2o.mlb n2o.cm
    kill $PID
done
