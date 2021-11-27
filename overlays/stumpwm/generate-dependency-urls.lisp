(require "asdf")
(ql:quickload "alexandria")
(ql:quickload "arrow-macros")
(ql:quickload "jonathan")

(defpackage #:generate-dependency-urls
  (:use #:cl)
  (:import-from #:alexandria #:flatten)
  (:import-from #:arrow-macros #:->>)
  (:import-from #:uiop #:run-program))
(in-package #:generate-dependency-urls)

(defun archive-url (dist)
  (ql-dist:archive-url (ql-dist:release dist)))

(defun dependencies (system &key (without "stumpwm"))
  (flet ((delete-duplicate-dists (dists) (delete-duplicates dists :key #'archive-url :test #'string=))
         (delete-dist (name dists) (delete name dists :test #'string= :key #'ql-dist:name)))
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
           (run-program `("nix-prefetch-url" "--unpack" ,url) :output '(:string :stripped t))))
    (let ((url (archive-url dist)))
      (make-instance 'nix-dependency :name (ql-dist:name dist) :url url :sha256 (url-hash url)))))

(defmethod print-object ((object nix-dependency) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (slot-value object 'name))))

(defmethod jojo:%to-json ((nix-dependency nix-dependency))
  (with-slots (name url sha256) nix-dependency
    (jojo:with-object
      (jojo:write-key-value "name" name)
      (jojo:write-key-value "url" url)
      (jojo:write-key-value "sha256" sha256))))

(let ((json (jojo:to-json (mapcar #'make-nix-dependency (dependencies "swm-config")))))
  (with-open-file (file #p"dependency-urls.json" :direction :output
                                                 :if-exists :supersede
                                                 :if-does-not-exist :create)
    (write-string json file)))
