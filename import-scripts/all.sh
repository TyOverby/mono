#!/usr/bin/env bash
export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

"$SCRIPT_DIR/bonsai.sh" & 
"$SCRIPT_DIR/incremental.sh" &
"$SCRIPT_DIR/core.sh" &
"$SCRIPT_DIR/patdiff.sh" &
"$SCRIPT_DIR/ppx_jane.sh" &

wait

