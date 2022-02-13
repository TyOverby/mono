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
  | sed 's:_public/\(\(.*\)/index.html\):<li><a href="./\1">\2</a></li>:' \
  > _public/out.html

mv _public/out.html _public/index.html
