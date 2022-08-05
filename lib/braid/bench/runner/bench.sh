#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

export X_LIBRARY_INLINING=true
export BENCHMARKS_RUNNER=TRUE
export BENCH_LIB="braid_benchmarks"
"$SCRIPT_DIR/../../../../_build/default/lib/braid/bench/runner/main.exe" \
  -run-without-cross-library-inlining \
  -width 80 \
  -clear-columns time percentage \
  -quota 0.5s \
  -stabilize-gc \
  "$@"
