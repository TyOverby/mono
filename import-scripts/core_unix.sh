#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"

get janestreet core_unix master
apply_patches janestreet core_unix
