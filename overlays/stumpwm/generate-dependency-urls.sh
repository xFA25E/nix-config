#! /usr/bin/env nix-shell
#! nix-shell -i bash -p sbcl curl

CACHE_DIR="${PWD}/.cache"

mkdir -p "${CACHE_DIR}" || exit 1

QUICKSTART="${CACHE_DIR}/quicklisp.lisp"
QUICKLISP="${CACHE_DIR}/quicklisp"
GENERATE_LISP="${CACHE_DIR}/generate.lisp"

curl -o "${QUICKSTART}" "https://beta.quicklisp.org/quicklisp.lisp"

cat >"${GENERATE_LISP}" <<EOF
(in-package :cl-user)

(ql:quickload "uiop")
(ql:quickload "alexandria")
(ql:quickload "jonathan")
(ql:quickload "arrow-macros")

(labels ((dependencies ()
           (jojo:parse (uiop:read-file-string #p"dependencies.json")))

         (url-hash (url)
           (uiop:run-program (list "nix-prefetch-url" "--unpack" url) :output '(:string :stripped t)))

         (archive-url (dependency)
           (ql-dist:archive-url (ql-dist:release dependency)))

         (make-nix-dependency (dependency)
           (let ((url (archive-url dependency)))
             (list :|name| (ql-dist:name dependency)
                   :|url| url
                   :|sha256| (url-hash url)))))

  (with-open-file (file #p"dependency-urls.json" :direction :output
                                                 :if-exists :supersede
                                                 :if-does-not-exist :create)
    (arrow-macros:-> (list (cdr (ql-dist:dependency-tree "stumpwm"))
                           (mapcar #'ql-dist:dependency-tree (dependencies)))
      alexandria:flatten
      (delete-duplicates :key #'archive-url :test #'string=)
      (arrow-macros:->> (mapcar #'make-nix-dependency))
      jojo:to-json
      (write-string file))))
EOF

export ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration (t \"${CACHE_DIR}/common-lisp/\"))"

sbcl --non-interactive \
     --load "${QUICKSTART}" \
     --eval "(quicklisp-quickstart:install :path #p\"${QUICKLISP}/\")" \
     --load "${GENERATE_LISP}"

rm -rf "${CACHE_DIR}"
