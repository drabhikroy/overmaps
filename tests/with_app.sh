#!/bin/bash
# Usage (from the project root): tests/with_app.sh <command...>
# Starts the offline demo app on port 8765, runs the command, then stops the app.
# Set RSCRIPT to use a particular R; the default is the first Rscript on the path.
RSCRIPT="${RSCRIPT:-Rscript}"
pkill -f demo_app.R 2>/dev/null; sleep 1
"$RSCRIPT" tests/demo_app.R > tests/demo_app.log 2>&1 &
APP=$!
for i in $(seq 1 40); do curl -s -o /dev/null http://127.0.0.1:8765/ && break; sleep 1; done
"$@"; RC=$?
kill $APP 2>/dev/null; exit $RC
