#!/usr/bin/env bash

export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
VENDOR_DIR="$SCRIPT_DIR/../vendor"

for dir in $(ls "$VENDOR_DIR" | grep "-"); do 
  cd "$dir"
  git diff .
done
