{
  enable = true;
  extraPackages = epkgs: with epkgs; [
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

    csv-mode dired-git-info modus-operandi-theme rainbow-mode sql-indent

    org-plus-contrib
  ];
  overrides = self: super: {
    csv-mode = super.elpaPackages.csv-mode.override (oldAttrs: {
      elpaBuild = attrs: oldAttrs.elpaBuild (attrs // {
        version = "1.15";
        src = oldAttrs.fetchurl {
          url = "https://elpa.gnu.org/packages/csv-mode-1.15.tar";
          sha256 = "0pigqhqg5mfza6jdskcr9yvrzdxnd68iyp3vyb8p8wskdacmbiyx";
        };
      });
    });
  };
}
