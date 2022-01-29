#!/usr/bin/env bash
export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

"$SCRIPT_DIR/bonsai.sh" & 
"$SCRIPT_DIR/async.sh" & 
"$SCRIPT_DIR/incremental.sh" &
"$SCRIPT_DIR/core.sh" &
"$SCRIPT_DIR/patdiff.sh" &
"$SCRIPT_DIR/ppx_jane.sh" &
"$SCRIPT_DIR/cohttp.sh" &
"$SCRIPT_DIR/pandoc.sh" &
"$SCRIPT_DIR/notty.sh" &

wait

