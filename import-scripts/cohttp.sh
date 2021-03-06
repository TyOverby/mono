#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get mirage ocaml-magic-mime master

get mirage ocaml-base64 master
rm -rf test bench fuzz

get mirage ocaml-conduit master
rm -rf tests src/conduit-lwt-unix src/conduit-lwt src/conduit-mirage

get hannesm domain-name main

get dbuenzli jsonm master
rm -rf test pkg doc
mv opam jsonm.opam
apply_patches dbuenzli jsonm

get dbuenzli logs master
rm -rf test pkg doc
mv opam logs.opam
apply_patches dbuenzli logs

get dbuenzli astring master
rm -rf test pkg doc
mv opam astring.opam
apply_patches dbuenzli astring 

get mirage ocaml-cstruct master
rm -rf lib_test fuzz ppx_test lwt async

get mirage ocaml-ipaddr master
rm -rf "lib_test"


get mirage ocaml-cohttp a9bb82a16cad32725be56e06267871bebb0d972b
rm -rf \
  ./http/test \
  ./http/fuzz \
  ./cohttp_async_test \
  ./cohttp/test \
  ./cohttp-async/test \
  ./cohttp-async/bin \
  ./bench \
  ./cohttp-lwt-jsoo \
  ./cohttp-lwt \
  ./cohttp-mirage \
  ./cohttp-lwt-unix \
  ./cohttp_test \
  ./examples \
  ./cohttp_lwt_jsoo_test \
  ./cohttp-top \
  ./.github \
  ./cohttp_lwt_unix_test \

