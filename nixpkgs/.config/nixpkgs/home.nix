{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.htop
  ];

  # programs.emacs = {
  #   enable = true;
  #   extraPackages = epkgs: with epkgs; [
  #     bui

  #     ace-link acme-theme apache-mode async avy bash-completion bicycle cargo
  #     cider clipmon clojure-mode consult diff-hl diminish dired-rsync dumb-jump
  #     edit-indirect eglot embark emmet-mode fd-dired flycheck
  #     flycheck-checkbashisms form-feed format-all free-keys gcmh geiser
  #     gitconfig-mode gitignore-mode htmlize insert-char-preview ipretty
  #     json-mode leaf ledger-mode magit marginalia mingus mu4e-alert
  #     neato-graph-bar nix-mode nov orderless org-mime outline-minor-faces
  #     pdf-tools php-mode quelpa restclient reverse-im rg robots-txt-mode
  #     rust-mode sdcv shr-tag-pre-highlight sly sly-asdf sly-quicklisp
  #     smartparens sqlup-mode sudo-edit transmission vlf web-mode wgrep which-key

  #     (csv-mode.override (oldAttrs: {
  #       elpaBuild = (attrs: oldAttrs.elpaBuild (attrs // {
  #         version = "1.15";
  #         src = oldAttrs.fetchurl {
  #           url = "https://elpa.gnu.org/packages/csv-mode-1.15.tar";
  #           sha256 = "0pigqhqg5mfza6jdskcr9yvrzdxnd68iyp3vyb8p8wskdacmbiyx";
  #         };
  #       }));
  #     }))

  #     dired-git-info modus-operandi-theme rainbow-mode sql-indent

  #     org-plus-contrib
  #   ];
  # };

  # programs.firefox = {
  #   enable = true;
  #   profiles = {
  #     myprofile = {
  #       settings = {
  #         "general.smoothScroll" = false;
  #       };
  #     };
  #   };
  # };

  # services.gpg-agent = {
  #   enable = true;
  #   defaultCacheTtl = 1800;
  #   enableSshSupport = true;
  # };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "val";
  home.homeDirectory = "/home/val";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
