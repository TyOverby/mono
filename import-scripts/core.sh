#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get janestreet base master
apply_patches janestreet base

get janestreet bignum master

get janestreet core master
get janestreet core_kernel master
get janestreet core_unix master
apply_patches janestreet core_unix

# misc
get janestreet abstract_algebra master
get janestreet profunctor master
get janestreet fuzzy_match master

