#! /usr/bin/env nix-shell
#! nix-shell -p sbcl -i bash

mkdir "$PWD/temp"
curl -o "$PWD/temp/quicklisp.lisp" "https://beta.quicklisp.org/quicklisp.lisp"

sbcl --non-interactive \
     --load temp/quicklisp.lisp \
     --eval "(quicklisp-quickstart:install :path #p\"$PWD/temp/quicklisp/\")" \
     --load "$PWD/deps.lisp" \
     --eval "(main)"

rm -rf "$PWD/temp"
