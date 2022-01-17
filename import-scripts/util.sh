set -euo pipefail 
export SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

VENDOR_DIR="$SCRIPT_DIR/../vendor"
mkdir -p "$VENDOR_DIR"
cd "$VENDOR_DIR"

CACHE_DIR="$SCRIPT_DIR/../_cache"
mkdir -p "$CACHE_DIR"

function get() {
  user=$1
  repo=$2
  rev=$3

  tarball="$CACHE_DIR/$user-$repo-$rev.tar.gz"
  dest="$VENDOR_DIR/$user-$repo"

  rm -rf "$dest"
  mkdir -p "$dest"
  cd "$dest"

  echo "for $repo" 

  if [ -f "$tarball" ]; then
      true
  else
      curl -Ls "https://github.com/$user/$repo/archive/$rev.tar.gz" > "$tarball"
  fi
  tar -xzf "$tarball" --strip-components 1 
}

function apply_patches() {
  user=$1
  repo=$2

  for patch in $(ls "$SCRIPT_DIR" | grep ".patch" | grep "$user-$repo"); do 
    git apply "$SCRIPT_DIR/$patch"
  done
}

function remove_opam_deterius() {
  rm *.opam
}
