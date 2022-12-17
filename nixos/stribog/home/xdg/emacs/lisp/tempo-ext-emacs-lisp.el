;; -*- lexical-binding: t; -*-

(require 'tempo-ext)

(defun tempo-emacs-lisp-user-elements (arg)
  (pcase arg
    (:elisp-group
     (string-trim-right (if-let ((bfn (buffer-file-name)))
                            (file-name-nondirectory bfn)
                          (buffer-name))
                        (rx (? "-mode") ".el" eos)))
    (:elisp-namespace
     (string-trim
      (replace-regexp-in-string
       (rx (+ (not (any "a-z")))) "-"
       (string-trim-right
        (downcase (if-let ((bfn (buffer-file-name)))
                      (file-name-nondirectory bfn)
                    (buffer-name)))
        (rx ".el" eos)))
      "-" "-"))
    (`(:elisp-with-parens . ,body)
     (if (or (not (eql (char-before) ?\()) (use-region-p))
         `(l "(" ,@body ")")
       `(l ,@body)))))

(add-to-list 'tempo-user-elements 'tempo-emacs-lisp-user-elements)

(let ((enable-fn (lambda () (or (eq this-command 'expand-abbrev)
                                (eql ?\s last-command-event))))
      (table (tempo-ext-abbrev-table 'emacs-lisp-mode)))
  (define-abbrev-table table nil :enable-function enable-fn))

(tempo-ext-define
 "lambda" 'emacs-lisp-mode
 '((:elisp-with-parens
    "lambda (" p ") " r>)))

(tempo-ext-define
 "let" 'emacs-lisp-mode
 '((:elisp-with-parens
    "let ((" p "))" n>
    r>)))

(tempo-ext-define
 "defvar" 'emacs-lisp-mode
 '((:elisp-with-parens
    "defvar " :elisp-namespace "-" p n>
    r> n>
    "\"" p "\"")))

(tempo-ext-define
 "defun" 'emacs-lisp-mode
 '((:elisp-with-parens
    "defun " :elisp-namespace "-" p " (" p ")" n>
    "\"" p "\"" n>
    r>)))

(tempo-ext-define
 "defgroup" 'emacs-lisp-mode
 '((:elisp-with-parens
    "defgroup " :elisp-group " nil" n>
    "\"" p "\"" n>
    ":group " p "nil")))

(tempo-ext-define
 "defcustom" 'emacs-lisp-mode
 '((:elisp-with-parens
    "defcustom " :elisp-namespace "-" p n>
    r> n>
    "\"" p "\"" n>
    ":type " p "nil" n>
    ":group '" :elisp-group)))

(tempo-ext-define
 "defface" 'emacs-lisp-mode
 '((:elisp-with-parens
    "defface " :elisp-namespace "-" p n>
    "'((t :inherit " p "nil))" n>
    "\"" p "\"" n>
    ":group '" :elisp-group)))

(provide 'tempo-ext-emacs-lisp)
;;; tempo-ext-emacs-lisp.el ends here
