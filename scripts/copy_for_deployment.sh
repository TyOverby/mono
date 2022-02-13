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
