#!/usr/bin/env bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR="$SCRIPT_DIR/.."

export PATH="$ROOT_DIR/_tools/_opam/bin:$PATH"
cd "$ROOT_DIR"
opam env
