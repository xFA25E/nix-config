self: super: let
  emacs-overlay = import (builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz) self super;

  overrides = eself: esuper: {
    cyrillic-dvorak-im = esuper.melpaBuild {
      pname = "cyrillic-dvorak-im";
      ename = "cyrillic-dvorak-im";
      version = "20191017.2111";
      recipe = super.writeText "recipe" ''
        (cyrillic-dvorak-im
          :fetcher github
          :repo "xFA25E/cyrillic-dvorak-im"
          :commit "09edcbc420ebae3ec069df95a768f79e6edcc76f")
      '';
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "cyrillic-dvorak-im";
        rev = "09edcbc420ebae3ec069df95a768f79e6edcc76f";
        sha256 = "1h3y8xj3zcnhxim7g78snsx01a2p0mq0lhg954ly6snna8m3dzvf";
      };
    };
    shell-pwd = esuper.melpaBuild {
      pname = "shell-pwd";
      ename = "shell-pwd";
      version = "20210306";
      recipe = super.writeText "recipe" ''
        (shell-pwd
          :fetcher github
          :repo "xFA25E/shell-pwd"
          :commit "dbb3a1a35fbd8fbfe9592e1529b649a99d015cd2")
      '';
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "shell-pwd";
        rev = "dbb3a1a35fbd8fbfe9592e1529b649a99d015cd2";
        sha256 = "0zq1pn5lk57c7sdrqk1ccy05dm1h9vqbp77h74gpp73xmwj3avbh";
      };
    };
    skempo = esuper.melpaBuild {
      pname = "skempo";
      ename = "skempo";
      version = "20210308";
      recipe = super.writeText "recipe" ''
        (skempo
          :fetcher github
          :repo "xFA25E/skempo"
          :commit "ff11999e1a4cc034b399a5fa685da1f76f93d5b2")
      '';
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "skempo";
        rev = "ff11999e1a4cc034b399a5fa685da1f76f93d5b2";
        sha256 = "0qm3zsz5kivvxrrn26f6a5nsvmany9629jd3dz2k7ahz02mky6rr";
      };
      packageRequires = [ eself.parent-mode ];
    };
    readelf-mode = esuper.melpaBuild {
      pname = "readelf-mode";
      ename = "readelf-mode";
      version = "20201125";
      recipe = super.writeText "recipe" ''
        (readelf-mode
          :fetcher github
          :repo "sirikid/readelf-mode"
          :commit "d43ec8e6c34b53c1d86157d5b380759a40076b53")
      '';
      src = super.fetchFromGitHub {
        owner = "sirikid";
        repo = "readelf-mode";
        rev = "d43ec8e6c34b53c1d86157d5b380759a40076b53";
        sha256 = "05rsky6wvgpaidky2cs4xw0ma0j3z6zqdl3djnkp795rr1a9gi0n";
      };
    };
    mediainfo-mode = esuper.melpaBuild {
      pname = "mediainfo-mode";
      ename = "mediainfo-mode";
      version = "20210203";
      recipe = super.writeText "recipe" ''
        (mediainfo-mode
          :fetcher github
          :repo "xFA25E/mediainfo-mode"
          :commit "b2793ae0f5f8029d296544715e92326bada463ae")
      '';
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "mediainfo-mode";
        rev = "b2793ae0f5f8029d296544715e92326bada463ae";
        sha256 = "1420n3vfjpby3cd6hbzdxvirk3kfik0hdpivgddcdgln6dwji95m";
      };
    };
    youtube-comments = esuper.melpaBuild {
      pname = "youtube-comments";
      ename = "youtube-comments";
      version = "20210222";
      recipe = super.writeText "recipe" ''
        (youtube-comments
          :fetcher github
          :repo "xFA25E/youtube-comments"
          :commit "621f9e9677241c06bb02a1b7eee46541b2d9c2c2")
      '';
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "youtube-comments";
        rev = "621f9e9677241c06bb02a1b7eee46541b2d9c2c2";
        sha256 = "1h4rn7mpj9y22dq8a13rxzm3bbgh4qap4pwvc9mnr7bl6adc94pi";
      };
    };
    pueue = esuper.melpaBuild {
      pname = "pueue";
      ename = "pueue";
      version = "20210311";
      recipe = super.writeText "recipe" ''
        (pueue
          :fetcher github
          :repo "xFA25E/pueue"
          :commit "a4467da565833e83c650740719d1c51fba6658eb")
      '';
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "pueue";
        rev = "a4467da565833e83c650740719d1c51fba6658eb";
        sha256 = "1k68mnxbsc8k450y0yfnan2vqa86lrb176mxv4pvqfkz4icpxyqf";
      };
      packageRequires = [ eself.bui ];
    };
    torrent-mode = esuper.melpaBuild {
      pname = "torrent-mode";
      ename = "torrent-mode";
      version = "20201109";
      recipe = super.writeText "recipe" ''
        (torrent-mode
          :fetcher github
          :repo "xFA25E/torrent-mode"
          :commit "f90ec549ac307e8222f9a30a8e0fd5c6115142f4")
      '';
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "torrent-mode";
        rev = "f90ec549ac307e8222f9a30a8e0fd5c6115142f4";
        sha256 = "112cyrpi741h2rjyg9w8a1kj4yd3xbr4vjigjgwc19fbnnckhb99";
      };
    };
  };
  emacsWithPackages = ((emacs-overlay.emacsPackagesFor super.emacs).overrideScope' overrides).emacsWithPackages;
in {
  myEmacs = emacsWithPackages (epkgs: with epkgs; [
    # my

    cyrillic-dvorak-im mediainfo-mode pueue readelf-mode shell-pwd skempo
    torrent-mode youtube-comments

    # melpa

    ace-link apache-mode async avy bash-completion bicycle cargo cider
    clojure-mode consult diff-hl dired-rsync direnv dumb-jump edit-indirect
    eglot emmet-mode fd-dired flycheck flycheck-checkbashisms form-feed
    format-all geiser gitconfig-mode gitignore-mode htmlize insert-char-preview
    ipretty json-mode ledger-mode magit marginalia nix-mode nov orderless
    org-mime outline-minor-faces pdf-tools php-mode restclient reverse-im rg
    robots-txt-mode rust-mode sdcv sly sly-asdf sly-quicklisp smartparens
    sqlup-mode sudo-edit transmission vlf web-mode wgrep

    # elpa
    csv-mode modus-themes rainbow-mode sql-indent

    # org
    org-plus-contrib
  ]);

  emacsEditor = super.writeShellScriptBin "emacseditor" ''
    "${self.myEmacs}/bin/emacsclient" --create-frame --alternate-editor=${self.myEmacs}/bin/emacs "$@"
  '';
}
