;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'skempo))

(declare-function skempo-define "skempo")
(declare-function skempo--define-tempo "skempo")

(skempo-define-tempo github (:mode nix-mode :tag t :abbrev t)
  "fetchFromGitHub {" n>
  "owner = \"" p "\";" n>
  "repo = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo url (:mode nix-mode :tag t :abbrev t)
  "fetchurl {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo zip (:mode nix-mode :tag t :abbrev t)
  "fetchzip {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo git (:mode nix-mode :tag t :abbrev t)
  "fetchgit {" n>
  "url = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p :nix-hash "\";" n>
  "}" p >)
