#! /usr/bin/env nix-shell
#! nix-shell -i bash -p sbcl curl

CACHE_DIR="${PWD}/.cache"

mkdir -p "${CACHE_DIR}" || exit 1

QUICKSTART="${CACHE_DIR}/quicklisp.lisp"
QUICKLISP="${CACHE_DIR}/quicklisp"
GENERATE_LISP="./generate-dependency-urls.lisp"

curl -o "${QUICKSTART}" "https://beta.quicklisp.org/quicklisp.lisp"

export CL_SOURCE_REGISTRY="${PWD}/swm-config/:"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration (t \"${CACHE_DIR}/common-lisp/\"))"

sbcl --non-interactive \
     --load "${QUICKSTART}" \
     --eval "(quicklisp-quickstart:install :path #p\"${QUICKLISP}/\")" \
     --load "${GENERATE_LISP}"

rm -rf "${CACHE_DIR}"
