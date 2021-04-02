;;; skempo-templates.el --- Skempo templates         -*- lexical-binding: t; eval: (progn (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t)); -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
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

;; My skempo templates

;;; Code:

(require 'skempo)

(defun skempo-elisp-namespace ()
  "Return elisp file prefix without .el."
  (string-trim-right (buffer-name) (rx ".el" eos)))

(defun skempo-elisp-group ()
  "Return elisp file prefix without -mode.el."
  (string-trim-right (buffer-name) (rx (? "-mode") ".el" eos)))

(defun skempo-nix-hash ()
  "Return temporary hash with 52 ones."
  (make-string 52 ?1))

(skempo-define-tempo (vd :tag t :mode php-mode)
  "echo '<pre>'; var_dump(" p "); echo '</pre>';")

(skempo-define-tempo (github :tag t :mode nix-mode)
  "fetchFromGitHub {" n>
  "owner = \"" p "\";" n>
  "repo = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p (skempo-nix-hash) "\";" n>
  "}" p >)

(skempo-define-tempo (url :tag t :mode nix-mode)
  "fetchurl {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p (skempo-nix-hash) "\";" n>
  "}" p >)

(skempo-define-tempo (zip :tag t :mode nix-mode)
  "fetchzip {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p (skempo-nix-hash) "\";" n>
  "}" p >)

(skempo-define-tempo (git :tag t :mode nix-mode)
  "fetchGit {" n>
  "url = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "}" p >)

(skempo-define-tempo (lambda :tag t :mode (emacs-lisp-mode lisp-mode))
  "(lambda (" p ") " n> r> ")")

(skempo-define-tempo (let :tag t :mode (emacs-lisp-mode lisp-mode))
  "(let ((" p "))" n> r> ")")

(skempo-define-tempo (defvar :tag t :mode lisp-mode)
  "(defvar " p n> r> n> "\"" p "\")")

(skempo-define-tempo (defun :tag t :mode lisp-mode)
  "(defun " p " (" p ")" n> "\"" p "\"" n> r> ")")

(skempo-define-tempo (defvar :tag t :mode emacs-lisp-mode)
  "(defvar " (skempo-elisp-namespace) "-" p n>
  r> n>
  "\"" p "\")")

(skempo-define-tempo (defun :tag t :mode emacs-lisp-mode)
  "(defun " (skempo-elisp-namespace) "-" p " (" p ")" n>
  "\"" p "\"" n>
  r> ")")

(skempo-define-tempo (defgroup :tag t :mode emacs-lisp-mode)
  "(defgroup " (skempo-elisp-group) " nil" n>
  "\"" p "\"" n>
  ":group " p "nil)")

(skempo-define-tempo (defcustom :tag t :mode emacs-lisp-mode)
  "(defcustom " (skempo-elisp-namespace) "-" p n>
  r> n>
  "\"" p "\"" n>
  ":type nil" n>
  ":group '" (skempo-elisp-group) ")" n>)

(skempo-define-tempo (defface :tag t :mode emacs-lisp-mode)
  "(defface " (skempo-elisp-namespace) "-" p n>
  "'((t :inherit " p "nil))" n>
  "\"" p "\"" n>
  ":group '" (skempo-elisp-group) ")")

(provide 'skempo-templates)
;;; skempo-templates.el ends here
