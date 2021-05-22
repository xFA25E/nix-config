;;; skempo-nix.el --- Nix skempo templates -*- lexical-binding: t; eval: (add-hook (quote after-save-hook) (lambda () (byte-recompile-file (buffer-file-name))) nil t); -*-

(require 'skempo)

(defun skempo-nix-hash ()
  "Return temporary hash with 52 ones."
  (make-string 52 ?1))

(skempo-define-tempo (github :mode nix-mode)
  "fetchFromGitHub {" n>
  "owner = \"" p "\";" n>
  "repo = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p (skempo-nix-hash) "\";" n>
  "}" p >)

(skempo-define-tempo (url :mode nix-mode)
  "fetchurl {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p (skempo-nix-hash) "\";" n>
  "}" p >)

(skempo-define-tempo (zip :mode nix-mode)
  "fetchzip {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p (skempo-nix-hash) "\";" n>
  "}" p >)

(skempo-define-tempo (git :mode nix-mode)
  "fetchgit {" n>
  "url = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p (skempo-nix-hash) "\";" n>
  "}" p >)

(provide 'skempo-nix)
