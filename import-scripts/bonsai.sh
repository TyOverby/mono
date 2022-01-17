#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get janestreet incr_dom master
rm -rf example testing

get janestreet async_js master
rm -rf test example-server example-client

get janestreet virtual_dom master
rm -rf test tyxml example
apply_patches janestreet virtual_dom 

get janestreet bonsai master
rm -rf docs examples/open_source experimental test web_test web_ui bench
