;;; skempo-lisp.el --- Lisp skempo templates -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'skempo)

(skempo-define-tempo (lambda :mode lisp-mode)
  "(lambda (" p ") " n> r> ")")

(skempo-define-tempo (let :mode lisp-mode)
  "(let ((" p "))" n> r> ")")

(skempo-define-tempo (defvar :mode lisp-mode)
  "(defvar " p n> r> n> "\"" p "\")")

(skempo-define-tempo (defun :mode lisp-mode)
  "(defun " p " (" p ")" n> "\"" p "\"" n> r> ")")

(provide 'skempo-lisp)
