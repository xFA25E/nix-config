;;; skempo-emacs-lisp.el --- Emacs lisp skempo templates -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'skempo)

(defun skempo-elisp-namespace ()
  "Return elisp file prefix without .el."
  (string-trim-right (buffer-name) (rx ".el" eos)))

(defun skempo-elisp-group ()
  "Return elisp file prefix without -mode.el."
  (string-trim-right (buffer-name) (rx (? "-mode") ".el" eos)))

(skempo-define-tempo (lambda :mode emacs-lisp-mode)
  "(lambda (" p ") " n> r> ")")

(skempo-define-tempo (let :mode emacs-lisp-mode)
  "(let ((" p "))" n> r> ")")

(skempo-define-tempo (defvar :mode emacs-lisp-mode)
  "(defvar " (skempo-elisp-namespace) "-" p n>
  r> n>
  "\"" p "\")")

(skempo-define-tempo (defun :mode emacs-lisp-mode)
  "(defun " (skempo-elisp-namespace) "-" p " (" p ")" n>
  "\"" p "\"" n>
  r> ")")

(skempo-define-tempo (defgroup :mode emacs-lisp-mode)
  "(defgroup " (skempo-elisp-group) " nil" n>
  "\"" p "\"" n>
  ":group " p "nil)")

(skempo-define-tempo (defcustom :mode emacs-lisp-mode)
  "(defcustom " (skempo-elisp-namespace) "-" p n>
  r> n>
  "\"" p "\"" n>
  ":type nil" n>
  ":group '" (skempo-elisp-group) ")" n>)

(skempo-define-tempo (defface :mode emacs-lisp-mode)
  "(defface " (skempo-elisp-namespace) "-" p n>
  "'((t :inherit " p "nil))" n>
  "\"" p "\"" n>
  ":group '" (skempo-elisp-group) ")")

(provide 'skempo-emacs-lisp)
