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

rm -rf "$PATCHES_DIR"
mkdir -p "$PATCHES_DIR"
for dir in $(ls "$VENDOR_DIR" | grep "-"); do 
  patchfile="$PATCHES_DIR/$dir"
  cd "$VENDOR_DIR/$dir"
  git diff -R "$AFTER" . | tee "$patchfile"

  if [ -s "$patchfile" ]; then
    true
  else 
    rm "$patchfile"
  fi
done

git checkout -f master 2> /dev/null

for file in $(ls "$PATCHES_DIR"); do 
  max=$(ls "$SCRIPT_DIR" | grep ".patch" | grep "$file" | cut -f 1 -d '-' | sort -n | tail -1 || echo '0')
  next=$(($max + 1))
  mv "$PATCHES_DIR/$file" "$SCRIPT_DIR/$next-$file.patch"
done
