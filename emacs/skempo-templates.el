;; -*- lexical-binding: t; -*-

(skempo-define-tempo lambda (:mode (emacs-lisp-mode lisp-mode))
  (:lisp-with-parens
   "lambda (" p ") " r>))

(skempo-define-tempo let (:mode (emacs-lisp-mode lisp-mode))
  (:lisp-with-parens
   "let ((" p "))" n>
   r>))

(skempo-define-tempo defvar (:mode lisp-mode)
  (:lisp-with-parens
   "defvar " p n>
   r> n>
   "\"" p "\""))

(skempo-define-tempo defun (:mode lisp-mode)
  (:lisp-with-parens
   "defun " p " (" p ")" n>
   "\"" p "\"" n>
   r>))

(skempo-define-tempo defpackage (:mode lisp-mode)
  (:lisp-with-parens
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
                  (:while ("Import symbol: " import-symbol)
                          " #:" (s import-symbol))
                  ")" n>))
   (:when ("Export: " export)
          "(:export #:" (s export)
          (:while ("Export: " export) " #:" (s export))
          ")" n>)
   "(:documentation \"" (P "Documentation: ") "\"))" n>
   "(in-package #:" (s package) ")" n>))

(skempo-define-tempo defsystem (:mode lisp-mode)
  (:lisp-with-parens
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
   ":perform (test-op (op c) (symbol-call '#:fiveam '#:run! (find-symbol* '#:" (s system) " '#:" (s system) ".tests)))"))

(skempo-define-tempo defvar (:mode emacs-lisp-mode)
  (:lisp-with-parens
   "defvar " :elisp-namespace "-" p n>
   r> n>
   "\"" p "\""))

(skempo-define-tempo defun (:mode emacs-lisp-mode)
  (:lisp-with-parens
   "defun " :elisp-namespace "-" p " (" p ")" n>
   "\"" p "\"" n>
   r>))

(skempo-define-tempo defgroup (:mode emacs-lisp-mode)
  (:lisp-with-parens
   "defgroup " :elisp-group " nil" n>
   "\"" p "\"" n>
   ":group " p "nil"))

(skempo-define-tempo defcustom (:mode emacs-lisp-mode)
  (:lisp-with-parens
   "defcustom " :elisp-namespace "-" p n>
   r> n>
   "\"" p "\"" n>
   ":type " p "nil" n>
   ":group '" :elisp-group))

(skempo-define-tempo defface (:mode emacs-lisp-mode)
  (:lisp-with-parens
   "defface " :elisp-namespace "-" p n>
   "'((t :inherit " p "nil))" n>
   "\"" p "\"" n>
   ":group '" :elisp-group))

(skempo-define-tempo switch (:mode js-mode)
  "switch (" p ") {" n>
  (:while ("Pattern: " pat)
          "case " (s pat) ":" > n>
          p n>
          "break;" n>)
  "default:" > n>
  p n>
  "break;" n>
  "}" >)

(skempo-define-tempo function (:mode js-mode)
  "function " p "(" p ") {" n>
  p n>
  "}" >)

(skempo-define-tempo if (:mode js-mode)
  "if (" p ") {" n>
  p n>
  "}" >)

(skempo-define-tempo for (:mode js-mode)
  "for (" p ") {" n>
  p n>
  "}" >)

(skempo-define-tempo try (:mode js-mode)
  "try {" n>
  p n>
  "} catch (" p "error) {" > n>
  p n>
  "}" >)

(skempo-define-tempo github (:mode nix-mode)
  "fetchFromGitHub {" n>
  "owner = \"" p "\";" n>
  "repo = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo url (:mode nix-mode)
  "fetchurl {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo zip (:mode nix-mode)
  "fetchzip {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo git (:mode nix-mode)
  "fetchgit {" n>
  "url = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo vd (:mode php-mode)
  "echo '<pre>'; var_dump(" r "); echo '</pre>';")

(skempo-define-tempo readmeorg (:mode org-mode)
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
