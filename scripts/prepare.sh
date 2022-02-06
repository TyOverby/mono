#!/usr/bin/env bash

export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
set -euo pipefail

PROJECT_DIR="$SCRIPT_DIR/../"
TOOLS_DIR="$PROJECT_DIR/_tools"
VER="4.12.1"
JS_BLEED_VER="cf2164a995a0d0577cd54beef5d2471e1a02f61f"

function make_switch() {
    root="$1"
    mkdir -p "$root"
    cd "$root"

    echo "making switch in ${pwd}"
    ls

    if [ -d "./_opam" ]; then
        if cat "./_opam/.opam-switch/switch-config" | grep "$VER" > /dev/null; then 
            echo "opam switch for $VER already created"
        else 
            echo "old switch detected, removing..."
            rm -rf "./_opam"
        fi
    fi 

    if [ -d "./_opam" ]; then
        true
    else 
        opam switch create "./" "ocaml-base-compiler.$VER" --no-install
    fi
}

make_switch "$TOOLS_DIR"
eval $(opam env)
opam install -y ocamlformat ocamlformat-rpc ocaml-lsp-server

make_switch "$PROJECT_DIR"
eval $(opam env)
opam repo add "janestreet-bleeding-$JS_BLEED_VER" "https://github.com/janestreet/opam-repository.git#cf2164a995a0d0577cd54beef5d2471e1a02f61f"
opam install -y dune re js_of_ocaml js_of_ocaml-ppx gen_js_api pcre lambdasoup sedlex fmt cryptokit ctypes ctypes-foreign ctypes-build angstrom stringext

