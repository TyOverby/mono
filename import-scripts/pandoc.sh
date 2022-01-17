#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get smimram ocaml-pandoc master
mv pandoc.opam pandoc_ast.opam
mv src/pandoc.ml src/pandoc_ast.ml
mv src/pandoc.mli src/pandoc_ast.mli
apply_patches smimram ocaml-pandoc
