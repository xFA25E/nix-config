;; -*- lexical-binding: t; -*-

(require 'tempo-ext)

(defun tempo-lisp-user-elements (arg)
  (pcase arg
    (`(:lisp-with-parens . ,body)
     (if (or (not (eql (char-before) ?\()) (use-region-p))
         `(l "(" ,@body ")")
       `(l ,@body)))))

(add-to-list 'tempo-user-elements 'tempo-lisp-user-elements)

(let ((enable-fn (lambda () (or (eq this-command 'expand-abbrev)
                                (eql ?\s last-command-event))))
      (table (tempo-ext-abbrev-table 'lisp-mode)))
  (define-abbrev-table table nil :enable-function enable-fn))

(tempo-ext-define
 "lambda" 'lisp-mode
 '((:lisp-with-parens
    "lambda (" p ") " r>)))

(tempo-ext-define
 "let" 'lisp-mode
 '((:lisp-with-parens
    "let ((" p "))" n>
    r>)))

(tempo-ext-define
 "defvar" 'lisp-mode
 '((:lisp-with-parens
    "defvar " p n>
    r> n>
    "\"" p "\"")))

(tempo-ext-define
 "defun" 'lisp-mode
 '((:lisp-with-parens
    "defun " p " (" p ")" n>
    "\"" p "\"" n>
    r>)))

(tempo-ext-define
 "defpackage" 'lisp-mode
 '((:lisp-with-parens
    "defpackage #:" (P "Package name: " package) n>
    "(:use #:cl)" n>
    (:when ("Nickname: " nickname)
           "(:nicknames #:" (s nickname)
           (:while ("Nickname: " nickname) " #:" (s nickname))
           ")" n>)
    (:when ("Local nickname: " local-nickname)
           (:when ("For package: " local-package)
                  "(:local-nicknames (#:" (s local-nickname) " #:" (s local-package) ")"
                  (:while ("Local nickname: " local-nickname)
                          (:when ("For package: " local-package)
                                 " (#:" (s local-nickname) " #:" (s local-package) ")"))
                  ")" n>))
    (:while ("Import from: " import-package)
            (:when ("Import symbol: " import-symbol)
                   "(:import-from #:" (s import-package) " #:" (s import-symbol)
                   (:while ("Import symbol: " import-symbol) " #:" (s import-symbol))
                   ")" n>))
    (:when ("Export: " export)
           "(:export #:" (s export)
           (:while ("Export: " export) " #:" (s export))
           ")" n>)
    "(:documentation \"" (P "Documentation: ") "\"))" n>
    "(in-package #:" (s package) ")" n>)))

(tempo-ext-define
 "defsystem" 'lisp-mode
 '((:lisp-with-parens
    "defsystem \"" (P "System: " system) "\"" n>
    (:when ("Long name: " long-name) ":long-name \"" (s long-name) "\"" n>)
    (:when ("Version: " version) ":version \""  (s version) "\"" n>)
    (:when ("Author: " author) ":author \"" (s author) "\"" n>)
    (:when ("Maintainer: " maintainer) ":maintainer \"" (s maintainer) "\"" n>)
    (:when ("Mailto: " mailto) ":mailto \"" (s mailto) "\"" n>)
    (:when ("License (ex: GPL3): " license) ":license \"" (s license) "\"" n>)
    (:when ("Homepage: " homepage) ":homepage \"" (s homepage) "\"" n>)
    (:when ("Bug tracker: " bug-tracker) ":bug-tracker \"" (s bug-tracker) "\"" n>)
    (:when ("Source control (ex: git): " source-control)
           (:when ("Link: " link) ":source-control (:" (s source-control) " \"" (s link) "\")" n>))
    (:when ("Description: " description) ":description \"" (s description) "\"" n>)
    ":long-description #.(let ((file (probe-file* (subpathname *load-pathname* \"README.md\")))) (when file (read-file-string file)))" n>
    (:when ("Dependency: " dependency)
           ":depends-on (" "\"" (s dependency) "\""
           (:while ("Dependency: " dependency) " \"" (s dependency) "\"")
           ")" n>)
    ":components ((:module \"src\" :components ((:file \"" (s system) "\"))))" n>
    ":in-order-to ((test-op (test-op \"" (s system) "/tests\"))))" n>
    n>
    "(defsystem \"" (s system) "/tests\"" n>
    ":depends-on (\"" (s system) "\" \"fiveam\")" n>
    ":components ((:module \"tests\" :components ((:file \"" (s system) "\"))))" n>
    ":perform (test-op (op c) (symbol-call '#:fiveam '#:run! (find-symbol* '#:" (s system) " '#:" (s system) ".tests)))")))

(provide 'tempo-ext-lisp)
;;; tempo-ext-lisp.el ends here
