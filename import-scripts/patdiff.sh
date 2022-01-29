#!/usr/bin/env bash
source "$(git rev-parse --show-toplevel)/import-scripts/util.sh"
get janestreet patience_diff master
get janestreet patdiff master
rm -rf test

