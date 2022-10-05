;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'skempo))

(add-to-list 'tempo-user-elements 'tempo-emacs-lisp-user-elements)

(defun tempo-emacs-lisp-user-elements (arg)
  (pcase arg
    (:elisp-group
     (string-trim-right (or (file-name-nondirectory (buffer-file-name))
                            (buffer-name))
                        (rx (? "-mode") ".el" eos)))
    (:elisp-namespace
     (string-trim
      (replace-regexp-in-string
       (rx (+ (not (any "a-z")))) "-"
       (string-trim-right
        (downcase (or (file-name-nondirectory (buffer-file-name))
                      (buffer-name)))
        (rx ".el" eos)))
      "-" "-"))
    (`(:elisp-with-parens . ,body)
     (if (or (not (eql (char-before) ?\()) (use-region-p))
         `(l "(" ,@body ")")
       `(l ,@body)))))

(declare-function skempo--abbrev-table "skempo")
(let ((enable-fn (lambda () (or (eq this-command 'expand-abbrev)
                                (eql ?\s last-command-event))))
      (table (skempo--abbrev-table 'emacs-lisp-mode)))
  (define-abbrev-table table nil :enable-function enable-fn))

(declare-function skempo-define "skempo")
(declare-function skempo--define-tempo "skempo")

(skempo-define-tempo lambda (:mode emacs-lisp-mode :tag t :abbrev t)
  (:elisp-with-parens
   "lambda (" p ") " r>))

(skempo-define-tempo let (:mode emacs-lisp-mode :tag t :abbrev t)
  (:elisp-with-parens
   "let ((" p "))" n>
   r>))

(skempo-define-tempo defvar (:mode emacs-lisp-mode :tag t :abbrev t)
  (:elisp-with-parens
   "defvar " :elisp-namespace "-" p n>
   r> n>
   "\"" p "\""))

(skempo-define-tempo defun (:mode emacs-lisp-mode :tag t :abbrev t)
  (:elisp-with-parens
   "defun " :elisp-namespace "-" p " (" p ")" n>
   "\"" p "\"" n>
   r>))

(skempo-define-tempo defgroup (:mode emacs-lisp-mode :tag t :abbrev t)
  (:elisp-with-parens
   "defgroup " :elisp-group " nil" n>
   "\"" p "\"" n>
   ":group " p "nil"))

(skempo-define-tempo defcustom (:mode emacs-lisp-mode :tag t :abbrev t)
  (:elisp-with-parens
   "defcustom " :elisp-namespace "-" p n>
   r> n>
   "\"" p "\"" n>
   ":type " p "nil" n>
   ":group '" :elisp-group))

(skempo-define-tempo defface (:mode emacs-lisp-mode :tag t :abbrev t)
  (:elisp-with-parens
   "defface " :elisp-namespace "-" p n>
   "'((t :inherit " p "nil))" n>
   "\"" p "\"" n>
   ":group '" :elisp-group))
