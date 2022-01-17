#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get janestreet textutils master
get janestreet async_extra master
get janestreet async_kernel master
get janestreet async_rpc_kernel master
get janestreet async_rpc_websocket master
get janestreet cohttp_async_websocket master
get janestreet async_unix master
get janestreet async master
get janestreet async_kernel master
get janestreet async_js master

get janestreet async_ssl master
rm bindings/ctypes_foreign_flat.ml

get janestreet async_websocket master
echo 'module Cryptokit = Cryptokit' > src/crypto.ml
