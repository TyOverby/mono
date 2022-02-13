#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get janestreet ocaml-embed-file master

get janestreet incr_dom master
apply_patches janestreet incr_dom
rm -rf example

get janestreet virtual_dom master
rm -rf tyxml 

get janestreet bonsai master
apply_patches janestreet bonsai
rm -rf experimental
rm -rf web_ui/url_var
rm -rf web_ui/partial_render_table/bench
