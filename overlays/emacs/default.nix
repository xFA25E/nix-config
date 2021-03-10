self: super: let
  myEmacsWithPackages = (super.emacsPackagesGen super.emacs).emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    (super.runCommand "default.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${./init.el} $out/share/emacs/site-lisp/default.el
      ${super.lib.strings.optionalString (builtins.pathExists ./init.elc) ''
        cp ${./init.elc} $out/share/emacs/site-lisp/default.elc
      ''}
    '')

    bui

    ace-link acme-theme apache-mode async avy bash-completion bicycle cargo
    cider clipmon clojure-mode consult diff-hl diminish dired-rsync
    dumb-jump edit-indirect eglot embark emmet-mode fd-dired flycheck
    flycheck-checkbashisms form-feed format-all free-keys gcmh geiser
    gitconfig-mode gitignore-mode htmlize insert-char-preview ipretty
    json-mode leaf ledger-mode magit marginalia mingus mu4e-alert
    neato-graph-bar nix-mode nov orderless org-mime outline-minor-faces
    pdf-tools php-mode quelpa restclient reverse-im rg robots-txt-mode
    rust-mode sdcv shr-tag-pre-highlight sly sly-asdf sly-quicklisp
    smartparens sqlup-mode sudo-edit transmission vlf web-mode wgrep
    which-key
  ]) ++ (with epkgs.elpaPackages; [
    csv-mode dired-git-info modus-operandi-theme rainbow-mode sql-indent
  ]) ++ (with epkgs.orgPackages; [ org-plus-contrib ]));
in {
  myEmacs = super.symlinkJoin {
    name = "emacs";
    paths = [ myEmacsWithPackages ];
    nativeBuildInputs = [ super.makeWrapper ];
    postBuild = ''
      wrapProgram "$out/bin/emacs" --add-flags '--no-splash-screen'
      makeWrapper "$out/bin/emacsclient" "$out/bin/emacseditor" --add-flags "--create-frame --alternate-editor=$out/bin/emacs"
    '';
  };
}
