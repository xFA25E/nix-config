#! /usr/bin/env -S nix shell nixpkgs#sbcl nixpkgs#curl -c sbcl --script

(require "asdf")

(defpackage #:update-urls.setup
  (:use #:cl)
  (:import-from #:uiop #:delete-directory-tree))
(in-package #:update-urls.setup)

(defconstant +cache-dir+ (merge-pathnames ".cache/"))
(defconstant +quicklisp-file+ (merge-pathnames "quicklisp.lisp" +cache-dir+))
(defconstant +quicklisp-dir+ (merge-pathnames "quicklisp" +cache-dir+))

(delete-directory-tree +cache-dir+ :validate t :if-does-not-exist :ignore)

(asdf:initialize-source-registry
 `(:source-registry
   :ignore-inherited-configuration
   (:directory ,(merge-pathnames "swm-config/"))))

(asdf:initialize-output-translations
 `(:output-translations
   :ignore-inherited-configuration
   (t ,(merge-pathnames "common-lisp/" +cache-dir+))))

(ensure-directories-exist +cache-dir+)
(uiop:run-program `("curl" "https://beta.quicklisp.org/quicklisp.lisp" "-o"
                           ,(namestring +quicklisp-file+)))
(load +quicklisp-file+)

(quicklisp-quickstart:install :path +quicklisp-dir+)
(ql:quickload "alexandria")
(ql:quickload "arrow-macros")
(ql:quickload "cl-ppcre")
(ql:quickload "jonathan")

(defpackage #:update-urls.generate
  (:use #:cl)
  (:import-from #:alexandria #:flatten)
  (:import-from #:arrow-macros #:->>))
(in-package #:update-urls.generate)

(defun archive-url (dist)
  (ql-dist:archive-url (ql-dist:release dist)))

(defun dependencies (system &key (without "stumpwm"))
  (format t ">>> Getting ~A's list of dependencies...~%" system)
  (flet ((delete-duplicate-dists (dists)
           (delete-duplicates dists :key #'archive-url :test #'string=))
         (delete-dist (name dists)
           (delete name dists :test #'string= :key #'ql-dist:name)))
    (->> system
      asdf:find-system
      asdf:system-depends-on
      (mapcar #'ql-dist:dependency-tree)
      flatten
      delete-duplicate-dists
      (delete-dist without))))

(defclass nix-dependency ()
  ((name :initarg :name)
   (url :initarg :url)
   (sha256 :initarg :sha256)))

(defun make-nix-dependency (dist)
  (flet ((url-hash (url)
           (uiop:run-program `("nix-prefetch-url" "--unpack" ,url)
                             :output '(:string :stripped t))))
    (let ((url (archive-url dist)))
      (format t "Fetching ~A's hash...~%" (ql-dist:name dist))
      (make-instance 'nix-dependency :name (ql-dist:name dist)
                                     :url url
                                     :sha256 (url-hash url)))))

(defmethod print-object ((object nix-dependency) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (slot-value object 'name))))

(defmethod jojo:%to-json ((nix-dependency nix-dependency))
  (with-slots (name url sha256) nix-dependency
    (jojo:with-object
      (jojo:write-key-value "name" name)
      (jojo:write-key-value "url" url)
      (jojo:write-key-value "sha256" sha256))))

(let ((json (jojo:to-json (mapcar #'make-nix-dependency
                                  (dependencies "swm-config")))))
  (with-open-file (file #p"urls.json" :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
    (format t "Writing to file...~%")
    (write-string json file)))

(in-package #:update-urls.setup)

(delete-directory-tree +cache-dir+ :validate t)
