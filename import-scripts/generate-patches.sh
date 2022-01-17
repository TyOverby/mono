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
  git diff -R "$AFTER" . > "$patchfile"

  if [ -s "$patchfile" ]; then
    echo "$dir has patches"
  else 
    rm "$patchfile"
  fi
done

git checkout -f master

for file in $(ls "$PATCHES_DIR"); do 
  ls "$SCRIPT_DIR" | grep ".patch" | grep "$file" | sort -t '-' -n
  max=$(ls "$SCRIPT_DIR" | grep ".patch" | grep "virtual" | cut -f 3 -d '-' | sed 's/.patch//' | sort -n | tail -1 || echo '0')
  echo $max
done
