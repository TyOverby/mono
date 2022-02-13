#!/usr/bin/env bash

rsync \
   -avzh \
   --prune-empty-dirs \
   --include "*/"  \
   --include="*.html" \
   --include="*.js" \
   --include="*.css" \
   --include="*.svg" \
   --include="*.png" \
   --exclude="*" \
   ./_build/default/app/ \
   ./_public

find _public \
  | grep index.html \
  | sed 's:_public/\(\(.*\)/index.html\):<a href="\1"> \2 </a>:' \
  > _public/index.html

