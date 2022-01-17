#!/usr/bin/env bash
set -euo pipefail 

export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_DIR="$SCRIPT_DIR/../"
VENDOR_DIR="$PROJECT_DIR/vendor"
PATCHES_DIR="$PROJECT_DIR/_patches"

function rev_with_content() {
  git add -A  > /dev/null
  git commit -m "_" > /dev/null
  git rev-parse HEAD
}

AFTER=$(rev_with_content)

"$VENDOR_DIR/all.sh"
BEFORE=$(rev_with_content)



for dir in $(ls "$VENDOR_DIR" | grep "-"); do 
  cd "$VENDOR_DIR/$dir"
  git diff "$BEFORE" "$AFTER" .
done
