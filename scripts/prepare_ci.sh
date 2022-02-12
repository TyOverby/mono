#!/usr/bin/env bash

ls
opam switch create "./" "ocaml-base-compiler.$VER" --no-install || true
eval $(opam env)
opam repo add "janestreet-bleeding-$JS_BLEED_VER" "https://github.com/janestreet/opam-repository.git#cf2164a995a0d0577cd54beef5d2471e1a02f61f" || true
opam install -y dune re js_of_ocaml js_of_ocaml-ppx gen_js_api pcre lambdasoup sedlex fmt cryptokit ctypes ctypes-foreign ctypes-build angstrom stringext || true
