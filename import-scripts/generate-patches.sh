#!/usr/bin/env bash
set -euo pipefail 

export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_DIR="$SCRIPT_DIR/../"
VENDOR_DIR="$PROJECT_DIR/vendor"
PATCHES_DIR="$PROJECT_DIR/_patches"

function rev_with_content() {
  git add -A  > /dev/null
  git commit -m "_" > /dev/null
  git log -1 --pretty=format:%H
}

AFTER=$(rev_with_content)

"$SCRIPT_DIR/all.sh" > /dev/null
BEFORE=$(rev_with_content)

rm -rf "$PATCHES_DIR"
mkdir -p "$PATCHES_DIR"

for dir in $(ls "$VENDOR_DIR" | grep "-"); do 
  cd "$VENDOR_DIR/$dir"
  git diff "$BEFORE" "$AFTER" . > "$PATCHES_DIR/$dir"
done

#git checkout "$AFTER"
