#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get janestreet textutils master
get janestreet async_extra master
get janestreet async_kernel master
get janestreet async_rpc_kernel master
get janestreet async_rpc_websocket master
get janestreet async_unix master
get janestreet async master
get janestreet async_js master
