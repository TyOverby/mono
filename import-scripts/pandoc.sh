#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get smimram ocaml-pandoc master
mv pandoc.opam pandoc_ast.opam
