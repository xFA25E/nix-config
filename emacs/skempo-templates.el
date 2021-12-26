;; -*- lexical-binding: t; -*-
(require 'skempo)

(defun skempo-user-element (arg)
  (pcase arg
    ('nix-hash (make-string 52 ?1))
    ('elisp-namespace (string-trim-right (buffer-name) (rx ".el" eos)))
    ('elisp-group (string-trim-right (buffer-name) (rx (? "-mode") ".el" eos)))
    ('delete-parens
     (when (and (eql (char-before) ?\() (eql (char-after) ?\)) (not (use-region-p)))
       (delete-char -1)
       (delete-char 1))
     '(l nil))))

(add-to-list 'tempo-user-elements 'skempo-user-element)

(defun skempo-lisp-mode-further-elements ()
  (setq skeleton-further-elements
        '((delete-parens ''(when (and (eql (char-before) ?\()
                                      (eql (char-after) ?\))
                                      (not (use-region-p)))
                             (delete-char -1)
                             (delete-char 1))))))

(with-eval-after-load 'lisp-mode
  (add-hook 'lisp-mode-hook 'skempo-lisp-mode-further-elements))

(skempo-define-tempo (lambda :mode (emacs-lisp-mode lisp-mode))
  delete-parens
  "(lambda (" p ") " r> ")")

(skempo-define-tempo (let :mode (emacs-lisp-mode lisp-mode))
  delete-parens
  "(let ((" p "))" n>
  r> ")")

(skempo-define-tempo (defvar :mode lisp-mode)
  delete-parens
  "(defvar " p n>
  r> n>
  "\"" p "\")")

(skempo-define-tempo (defun :mode lisp-mode)
  delete-parens
  "(defun " p " (" p ")" n>
  "\"" p "\"" n>
  r> ")")

(skempo-define-skeleton (defpackage :mode lisp-mode)
  "Package name: " delete-parens
  "(defpackage #:" str > "\n"
  "(:use #:cl)" > "\n"
  "(:nicknames" ("Nickname: " " #:" str) & ")" | -12 > "\n"
  "(:local-nicknames" ("Local nickname: " " (#:" str " #:" (skeleton-read "Nickname of package: " nil t) ")") & ")" | -18 > "\n"
  ("Import from: " "(:import-from #:" str ("Import: " " #:" str) ")" > "\n")
  "(:export" ("Export: " " #:" str) & ")" | -9 > "\n"
  "(:documentation \"" (skeleton-read "Documentation: ") "\"))" > "\n"
  "(in-package #:" str ")" > "\n")

(skempo-define-skeleton (defsystem :mode lisp-mode)
  "System name: " delete-parens
  "(defsystem \"" str "\"" > "\n"
  ":long-name \"" (skeleton-read "Long name: ") "\"" | -13 > "\n"
  ":version \"" (skeleton-read "Version: " "0.0.1") & "\"" | -11 > "\n"
  ":author \"" (setq v1 (skeleton-read "Author: ")) & "\"" | -10 > "\n"
  ":maintainer \"" (skeleton-read "Maintainer: " v1) & "\"" | -14 > "\n"
  ":license \"" (skeleton-read "License: " "GPL3") & "\"" | -11 > "\n"
  ":homepage \"" (setq v2 (skeleton-read "Homepage: ")) & "\"" | -12 > "\n"
  ":bug-tracker \"" (skeleton-read "Bug tracker: " (when (and v2 (not (string-empty-p v2))) (concat v2 "/issues"))) & "\"" | -15 > "\n"
  ":description \"" (skeleton-read "Description: ") "\"" > "\n"
  ":mailto \"" (skeleton-read "Mailto: " (string-trim-right (string-trim-left (or v1 "") ".*?<") ">")) & "\"" | -10 > "\n"
  (nil ":source-control (:" (skeleton-read "Source control: " "git" t) " \"" (skeleton-read "Link: " (when (and v2 (not (string-empty-p v2))) (concat v2 ".git")) t) "\")" > "\n")
  ":long-description #.(let ((file (probe-file* (subpathname *load-pathname* \"README.md\")))) (when file (read-file-string file)))" > "\n"
  ":depends-on (" ("Dependency: " "\"" str "\" ") & -1 & ")" | -14 > "\n"
  ":components ((:module \"src\" :components ((:file \"" str "\"))))" > "\n"
  ":in-order-to ((test-op (test-op \"" str "/tests\")))" > "\n"
  -1 ")" "\n"
  "\n"
  "(defsystem \"" str "/tests\"" > "\n"
  ":depends-on (\"" str "\" \"fiveam\")" > "\n"
  ":components ((:module \"tests\" :components ((:file \"" str "\"))))" > "\n"
  ":perform (test-op (op c) (symbol-call '#:fiveam '#:run! (find-symbol* '#:" str " '#:" str ".tests)))" > "\n"
  -1 ")")

(skempo-define-tempo (defvar :mode emacs-lisp-mode)
  delete-parens
  "(defvar " elisp-namespace "-" p n>
  r> n>
  "\"" p "\")")

(skempo-define-tempo (defun :mode emacs-lisp-mode)
  delete-parens
  "(defun " elisp-namespace "-" p " (" p ")" n>
  "\"" p "\"" n>
  r> ")")

(skempo-define-tempo (defgroup :mode emacs-lisp-mode)
  delete-parens
  "(defgroup " elisp-group " nil" n>
  "\"" p "\"" n>
  ":group " p "nil)")

(skempo-define-tempo (defcustom :mode emacs-lisp-mode)
  delete-parens
  "(defcustom " elisp-namespace "-" p n>
  r> n>
  "\"" p "\"" n>
  ":type " p "nil" n>
  ":group '" elisp-group ")")

(skempo-define-tempo (defface :mode emacs-lisp-mode)
  delete-parens
  "(defface " elisp-namespace "-" p n>
  "'((t :inherit " p "nil))" n>
  "\"" p "\"" n>
  ":group '" elisp-group ")")

(skempo-define-skeleton (switch :mode js-mode)
  "Expression: "
  "switch (" str ") {" > \n
  ("Pattern: "
   "case " str ":" > \n
   @ \n
   "break;" > \n \n)
  "default:" > \n
  @ \n
  "}" >)

(skempo-define-tempo (function :mode js-mode)
  "function " p "(" p ") {" n>
  p n>
  "}" >)

(skempo-define-tempo (if :mode js-mode)
  "if (" p ") {" n>
  p n>
  "}" >)

(skempo-define-tempo (for :mode js-mode)
  "for (" p ") {" n>
  p n>
  "}" >)

(skempo-define-tempo (try :mode js-mode)
  "try {" n>
  p n>
  "} catch (" p "error) {" > n>
  p n>
  "}" >)

(skempo-define-tempo (github :mode nix-mode)
  "fetchFromGitHub {" n>
  "owner = \"" p "\";" n>
  "repo = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo (url :mode nix-mode)
  "fetchurl {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo (zip :mode nix-mode)
  "fetchzip {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo (git :mode nix-mode)
  "fetchgit {" n>
  "url = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo (vd :mode php-mode)
  "echo '<pre>'; var_dump(" r "); echo '</pre>';")

(skempo-define-tempo (readmeorg :mode org-mode)
  "#+TITLE: " (P "Project title: ") n
  (P "A short, one-line description of the project: ") n
  n
  "* Overview" n
  p "# A longer description of the project" n
  n
  "** Features" n
  "** History" n
  "** Motivation" n
  "* Usage" n
  p "# Examples of usage" n
  n
  "* Documentation" n
  "* License" n
  "Copyright (c) " (format-time-string "%Y") " " (P "Authors: ") n
  "Licensed under the " p "GPL3 License." n
  n
  "* COMMENT Local Variables" n
  "# Local Variables:" n
  "# eval: (add-hook 'after-save-hook #'org-md-export-to-markdown nil t)" p n
  "# End:")
