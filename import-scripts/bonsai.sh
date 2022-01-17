#!/usr/bin/env bash

export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
source "$SCRIPT_DIR/util.sh"

get janestreet incr_dom master
rm -rf example testing

get janestreet async_js master
rm -rf test example-server example-client

get janestreet virtual_dom master
rm -rf test tyxml example
apply_patches janestreet virtual_dom 

get janestreet bonsai master
rm -rf bindings docs examples experimental extra test web web_test web_ui bench
