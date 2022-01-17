set -euo pipefail 

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

  if [ -f "$tarball" ]; then
      echo "found tarball in cache for $user/$repo@$rev"
  else
      curl -Ls "https://github.com/$user/$repo/archive/$rev.tar.gz" > "$tarball"
  fi
  tar -xzf "$tarball" --strip-components 1 
}

function remove_opam_deterius() {
  rm *.opam
}
