;;; tempo-ext.el --- Extensions for tempo            -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'derived)
(require 'subr-x)
(require 'tempo)

(defun te-deduplicate-marks ()
  "Delete duplicate marks in `tempo-marks'."
  (let ((marks tempo-marks))
    (while (cdr marks)
      (if (/= (car marks) (cadr marks))
          (setq marks (cdr marks))
        (when (markerp (cadr marks))
          (set-marker (cadr marks) nil))
        (setcdr marks (cddr marks))))))

(defun te-abbrevs ()
  (let ((abbrevs (make-hash-table :test 'equal))
        (tables (abbrev--active-tables)))
    (while tables
      (let ((table (pop tables)))
        (when (abbrev-table-get table :tempo-ext)
          (mapatoms
           (lambda (abbrev)
             (when (symbol-value abbrev)
               (let ((name (symbol-name abbrev)))
                 (unless (gethash name abbrevs)
                   (puthash name (symbol-function abbrev) abbrevs)))))
           table))
        (setq tables (append (abbrev-table-get table :parents) tables))))
    abbrevs))

(defun te-call ()
  (interactive)
  (let ((abbrevs (te-abbrevs)))
    (if (hash-table-empty-p abbrevs)
        (message "No tempo-ext abbrevs defined for current mode.")
      (let ((abbrev (completing-read "Abbrev: " abbrevs nil t)))
        (funcall (gethash abbrev abbrevs))))))

(defun te-mode-abbrev-table (mode)
  "Get abbrev table for MODE or `global-abbrev-table' if nil."
  (if mode
      (derived-mode-abbrev-table-name mode)
    'global-abbrev-table))

(defun te-abbrev-table (mode)
  "Get tempo-ext abbrev table for MODE."
  (intern (concat "tempo-ext-" (symbol-name (te-mode-abbrev-table mode)))))

(defun te-abbrev-table-names (table)
  "Return abbrev TABLE names."
  (let ((names nil))
    (mapatoms (lambda (abbrev)
                (when (symbol-value abbrev)
                  (push (symbol-name abbrev) names)))
              (symbol-value table))
    names))

(defun te-define-function (name mode hook)
  (let ((mode-abbrev-table (te-mode-abbrev-table mode))
        (abbrev-table (te-abbrev-table mode)))
    (put hook 'no-self-insert t)
    (define-abbrev-table mode-abbrev-table nil)
    (define-abbrev-table abbrev-table nil :case-fixed t :tempo-ext t)
    (define-abbrev (symbol-value abbrev-table) name "" hook
      :case-fixed t :system t :tempo-ext t)

    (let* ((names (te-abbrev-table-names abbrev-table))
           (regexp (concat (regexp-opt names "\\_<\\(") " *")))
      (abbrev-table-put (symbol-value abbrev-table) :regexp regexp))

    (let ((parents (abbrev-table-get (symbol-value mode-abbrev-table) :parents)))
      (cl-pushnew (symbol-value abbrev-table) parents :test #'eq)
      (abbrev-table-put (symbol-value mode-abbrev-table) :parents parents))))

(defvar-local te-insert-count 0)
(defun te-make-hook (name body)
  (let* ((body-symbol (gensym (concat "body-" name)))
         (hook-symbol (gensym (concat "hook-" name)))
         (hook (lambda (&optional arg)
                 (interactive "*P")
                 (when (zerop (mod (cl-incf te-insert-count) 50))
                   (te-deduplicate-marks)
                   (setq-local te-insert-count 0))
                 (let ((on-region (xor tempo-insert-region arg)))
                   (tempo-insert-template body-symbol on-region)))))
    (set body-symbol body)
    (fset hook-symbol hook)
    hook-symbol))

(defun te-define (name mode body)
  (te-define-function name mode (te-make-hook name body)))

(defun te-user-elements (element)
  "Support for conditional and looping tempo elements.
The following forms are supported for ELEMENT:

\(:if (PROMPT VAR) THEN ELSE)

\(:when (PROMPT VAR) BODY...)

\(:while (PROMPT VAR) BODY...)

PROMPT is a string used to read value for VAR.  VAR is a tempo
variable symbol.  Its value can be read with s, as usual.  BODY,
THEN and ELSE are tempo elements.  To abort the execution of
these elements, user must enter empty string.

The main purpose of this extension is to mimic skeleton
conditionals and iterative templats.  Skeleton becomes pretty
much obsolete with this extension."
  (pcase element
    (`(:if (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) ,then ,else)
     (let ((input (read-from-minibuffer prompt)))
       (if (string-empty-p input)
           else
         (tempo-save-named var (read-from-minibuffer prompt))
         then)))
    (`(:when (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:if (,prompt ,var) (l ,@body) (l)))
    (`(:while (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:when (,prompt ,var) ,@body ,element))))

(add-hook 'tempo-user-elements 'te-user-elements)

(provide 'tempo-ext)
;;; tempo-ext.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("te-" . "tempo-ext-"))
;; End:
