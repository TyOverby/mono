#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get janestreet ocaml-embed-file master

get janestreet incr_dom master
rm -rf example

get janestreet virtual_dom master
rm -rf tyxml 

get janestreet bonsai master
rm -rf experimental
