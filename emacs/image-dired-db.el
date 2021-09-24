;;; image-dired-db.el --- Better image-dired tag database  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <val@nixos>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO:

;; when image-dired-db-enabled is t:
;; if the current db is not in hash-table form:
;; convert it to hash-table form
;; add advices

;; when image-dired-db-enabled is nil:
;; convert current db in old format
;; remove advices

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image-dired)

(cl-defstruct (image-dired-db-data (:constructor image-dired-db-data--create)
                                   (:copier nil))
  tags comment)

(defvar image-dired-db
  nil
  "Image-dired database.")

(defun image-dired-db-load ()
  "Load `image-dired' tags database."
  (or image-dired-db
      (setq image-dired-db
            (if (file-exists-p image-dired-db-file)
                (with-temp-buffer
                  (insert-file-contents image-dired-db-file)
                  (read (current-buffer)))
              (make-hash-table :test 'equal)))))

(defun image-dired-db-save (data)
  "Save `image-dired' tags DATA."
  (let ((directory (file-name-directory image-dired-db-file)))
    (unless (file-directory-p directory)
      (make-directory directory t)))

  (with-temp-file image-dired-db-file
    (let ((standard-output (current-buffer))
          (print-circle t))
      (prin1 data))))

(defmacro image-dired-db-with-db (variable &rest body)
  "Run BODY with VARIABLE bounded to database.
Save database afterwards.."
  (declare (indent 1) (debug t))
  `(let ((,variable (image-dired-db-load)))
     (prog1 (progn ,@body)
       (image-dired-db-save ,variable))))

(defun image-dired-db-write-tags (file-tags)
  (image-dired-db-with-db db
    (dolist (file-tag file-tags)
      (cl-destructuring-bind (file . tag) file-tag
        (unless (string-empty-p tag)
          (if-let ((data (gethash file db)))
              (cl-pushnew tag (image-dired-db-data-tags data) :test 'equal)
            (puthash file (image-dired-db-data--create :tags (list tag)) db)))))))

(defun image-dired-db-comment-full-p (comment)
  (and (stringp comment) (not (string-empty-p comment))))

(defun image-dired-db-data-comment-full-p (data)
  (image-dired-db-comment-full-p (image-dired-db-data-comment data)))

(defun image-dired-db-remove-tag (files tag)
  (image-dired-db-with-db db
    (dolist (file (if (listp files) files (list files)))
      (when-let ((data (gethash file db)))
        (unless (or (cl-callf2 cl-delete tag (image-dired-db-data-tags data) :test 'equal)
                    (image-dired-db-data-comment-full-p data))
          (remhash file db))))))

(defun image-dired-db-write-comments (file-comments)
  (image-dired-db-with-db db
    (dolist (file-comment file-comments)
      (cl-destructuring-bind (file . comment) file-comment
        (if (image-dired-db-comment-full-p comment)
            (if-let ((data (gethash file db)))
                (setf (image-dired-db-data-comment data) comment)
              (puthash file (image-dired-db-data--create :comment comment) db))
          (when-let ((data (gethash file db)))
            (if (image-dired-db-data-tags data)
                (setf (image-dired-db-data-comment data) nil)
              (remhash file db))))))))

(defun image-dired-db-list-tags (file)
  (when-let ((data (gethash file (image-dired-db-load))))
    (image-dired-db-data-tags data)))

(defun image-dired-db-get-comment (file)
  (when-let ((data (gethash file (image-dired-db-load))))
    (image-dired-db-data-comment data)))

(defun image-dired-db-mark-tagged-files ()
  (interactive)
  (let* ((tag (read-string "Mark tagged files (regexp): "))
         (tag-match-p (apply-partially 'string-match-p tag))
         (files nil))
    (maphash
     (lambda (file data)
       (when (cl-some tag-match-p (image-dired-db-data-tags data))
         (push file files)))
     (image-dired-db-load))
    (dired-mark-if
     (and (not (looking-at-p dired-re-dot))
          (not (eolp))                ; empty line
          (when-let ((fn (dired-get-filename nil t)))
            (member fn files)))
     "matching tag file")))

(defun image-dired-db-create-gallery-lists ()
  (image-dired-db-with-db db
    (setq image-dired-tag-file-list nil)
    (setq image-dired-file-tag-list nil)
    (setq image-dired-file-comment-list nil)
    (maphash
     (lambda (file data)
       (dolist (tag (image-dired-db-data-tags data))
         (image-dired-add-to-tag-file-lists tag file))
       (when-let ((comment (image-dired-db-data-comment data)))
         (image-dired-add-to-file-comment-list file comment)))
     db))
  ;; Sort tag-file list
  (setq image-dired-tag-file-list
        (sort image-dired-tag-file-list
              (lambda (x y)
                (string< (car x) (car y))))))

(defun image-dired-db-save-information-from-widgets ()
  (cl-flet ((tags-diff (a b) (cl-set-difference a b :test 'equal)))
    (image-dired-write-comments
     (mapcar
      (lambda (widget)
        (cons (car widget) (widget-value (cadr widget))))
      image-dired-widget-list))

    (image-dired-write-tags
     (mapcan
      (lambda (widget)
        (let* ((file (car widget))
               (tags-string (widget-value (car (cddr widget))))
               (new-tags (split-string tags-string ","))
               (current-tags (image-dired-list-tags file))
               (tags-to-remove (tags-diff current-tags new-tags))
               (tags-to-add (tags-diff new-tags current-tags)))
          (mapc (apply-partially 'image-dired-remove-tag file) tags-to-remove)
          (mapcar (apply-partially 'cons file) tags-to-add)))
      image-dired-widget-list))))

(defun image-dired-db-old-db-p ()
  (when (file-exists-p image-dired-db-file)
    (with-temp-buffer
      (insert-file-contents image-dired-db-file)
      (not (hash-table-p (ignore-errors (read (current-buffer))))))))

(defun image-dired-db-parse-old-db-tags-and-comment (tags-and-comment)
  (let ((tags nil)
        (comment nil))
    (dolist (tag-or-comment tags-and-comment)
      (if (string-prefix-p "comment:" tag-or-comment)
          (setq comment (substring tag-or-comment 8))
        (cl-pushnew tag-or-comment tags)))
    (image-dired-db-data--create :tags tags :comment comment)))

(defun image-dired-db-parse-old-db ()
  (with-temp-buffer
    (insert-file-contents image-dired-db-file)
    (cl-loop with db = (make-hash-table :test 'equal)
             for line in (split-string (buffer-string) "\n")
             for (file . tags-and-comment) = (split-string line ";")
             for data = (image-dired-db-parse-old-db-tags-and-comment tags-and-comment)
             unless (string-empty-p file)
             do (puthash file data db)
             finally return db)))

(defun image-dired-db-convert-to-old-db ()
  (with-temp-file image-dired-db-file
    (maphash
     (lambda (file data)
       (pcase data
         ((cl-struct image-dired-db-data tags comment)
          (insert file)
          (dolist (tag tags)
            (insert ";" tag))
          (when comment
            (insert ";comment:" comment))
          (insert "\n"))))
     (with-temp-buffer
       (insert-file-contents image-dired-db-file)
       (read (current-buffer))))))

(defun image-dired-db-enable (variable value)
  (if value
      (progn
        (when (image-dired-db-old-db-p)
          (image-dired-db-save (image-dired-db-parse-old-db)))

        (advice-add 'image-dired-write-tags :override 'image-dired-db-write-tags)
        (advice-add 'image-dired-remove-tag :override 'image-dired-db-remove-tag)
        (advice-add 'image-dired-write-comments :override 'image-dired-db-write-comments)
        (advice-add 'image-dired-list-tags :override 'image-dired-db-list-tags)
        (advice-add 'image-dired-get-comment :override 'image-dired-db-get-comment)
        (advice-add 'image-dired-mark-tagged-files :override 'image-dired-db-mark-tagged-files)
        (advice-add 'image-dired-create-gallery-lists :override 'image-dired-db-create-gallery-lists)
        (advice-add 'image-dired-save-information-from-widgets :override 'image-dired-db-save-information-from-widgets))

    (unless (image-dired-db-old-db-p)
      (image-dired-db-convert-to-old-db))

    (advice-remove 'image-dired-write-tags 'image-dired-db-write-tags)
    (advice-remove 'image-dired-remove-tag 'image-dired-db-remove-tag)
    (advice-remove 'image-dired-write-comments 'image-dired-db-write-comments)
    (advice-remove 'image-dired-list-tags 'image-dired-db-list-tags)
    (advice-remove 'image-dired-get-comment 'image-dired-db-get-comment)
    (advice-remove 'image-dired-mark-tagged-files 'image-dired-db-mark-tagged-files)
    (advice-remove 'image-dired-create-gallery-lists 'image-dired-db-create-gallery-lists)
    (advice-remove 'image-dired-save-information-from-widgets 'image-dired-db-save-information-from-widgets))

  (set-default variable value))

(defcustom image-dired-db-enabled
  nil
  ""
  :type 'boolean
  :set 'image-dired-db-enable
  :group 'image-dired-db)

(provide 'image-dired-db)
;;; image-dired-db.el ends here
