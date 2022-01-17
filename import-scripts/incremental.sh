#!/usr/bin/env bash

export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
source "$SCRIPT_DIR/util.sh"

get janestreet incremental master
rm -rf test src-debug  test-debug step_function/test

get janestreet incr_map master
rm -rf test collate bench

get janestreet incr_select master
rm -rf test
