#!/usr/bin/env bash


## Built Resources
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


echo '<title>Ty Overby - Monorepo</title>' > _public/out.html
echo '<li><a href="./ocaml-docs/index.html">OCaml Docs</a></li>' >> _public/out.html

find _public \
  | grep index.html \
  | sed 's:_public/\(\(.*\)/index.html\):<li><a href="./\1">\2</a></li>:' \
  >> _public/out.html

mv _public/out.html _public/index.html

## Documentation
mv _build/default/_doc/_html _public/ocaml-docs
