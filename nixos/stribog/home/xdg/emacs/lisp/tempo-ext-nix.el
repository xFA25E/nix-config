;; -*- lexical-binding: t; -*-

(require 'tempo-ext)

(defun tempo-nix-user-elements (arg)
  (pcase arg
    (:nix-hash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=")))

(add-to-list 'tempo-user-elements 'tempo-nix-user-elements)

(tempo-ext-define
 "fetchurl" 'nix-mode
 '("fetchurl {" n>
   "url = \"" p "\";" n>
   "hash = \"" p :nix-hash "\";" n>
   "}" p >))

(tempo-ext-define
 "fetchzip" 'nix-mode
 '("fetchzip {" n>
   "url = \"" p "\";" n>
   "hash = \"" p :nix-hash "\";" n>
   "}" p >))

(tempo-ext-define
 "fetchgit" 'nix-mode
 '("fetchgit {" n>
   "url = \"" p "\";" n>
   "rev = \"" p "\";" n>
   "hash = \"" p :nix-hash "\";" n>
   "}" p >))

(tempo-ext-define
 "fetchFromGitHub" 'nix-mode
 '("fetchFromGitHub {" n>
   "owner = \"" p "\";" n>
   "repo = \"" p "\";" n>
   "rev = \"" p "\";" n>
   "hash = \"" p :nix-hash "\";" n>
   "}" p >))

(provide 'tempo-ext-nix)
;;; tempo-ext-nix.el ends here
