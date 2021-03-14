self: super: let
  overrides = eself: esuper: {
    # my
    cyrillic-dvorak-im = esuper.trivialBuild {
      pname = "cyrillic-dvorak-im";
      ename = "cyrillic-dvorak-im";
      version = "20191017.2111";
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "cyrillic-dvorak-im";
        rev = "09edcbc420ebae3ec069df95a768f79e6edcc76f";
        sha256 = "1h3y8xj3zcnhxim7g78snsx01a2p0mq0lhg954ly6snna8m3dzvf";
      };
      packageRequires = [ eself.emacs ];
      meta = {
        homepage = "https://github.com/xFA25E/cyrillic-dvorak-im";
        license = super.lib.licenses.free;
      };
    };
    shell-pwd = esuper.trivialBuild {
      pname = "shell-pwd";
      ename = "shell-pwd";
      version = "20210306.1333";
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "shell-pwd";
        rev = "dbb3a1a35fbd8fbfe9592e1529b649a99d015cd2";
        sha256 = "0zq1pn5lk57c7sdrqk1ccy05dm1h9vqbp77h74gpp73xmwj3avbh";
      };
      packageRequires = [
        eself.emacs
        # (require 'tramp)
        # (require 'files)
      ];
      meta = {
        homepage = "https://github.com/xFA25E/shell-pwd";
        license = super.lib.licenses.free;
      };
    };
    skempo = esuper.trivialBuild {
      pname = "skempo";
      ename = "skempo";
      version = "0.1.0";
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "skempo";
        rev = "ff11999e1a4cc034b399a5fa685da1f76f93d5b2";
        sha256 = "0qm3zsz5kivvxrrn26f6a5nsvmany9629jd3dz2k7ahz02mky6rr";
      };
      packageRequires = [ eself.emacs eself.parent-mode ];
      meta = {
        homepage = "https://github.com/xFA25E/skempo";
        license = super.lib.licenses.free;
      };
    };
    readelf-mode = esuper.trivialBuild {
      pname = "readelf-mode";
      ename = "readelf-mode";
      version = "1.0.0";
      src = super.fetchFromGitHub {
        owner = "sirikid";
        repo = "readelf-mode";
        rev = "1.0.0";
        sha256 = "05rsky6wvgpaidky2cs4xw0ma0j3z6zqdl3djnkp795rr1a9gi0n";
      };
      packageRequires = [ eself.emacs ];
      meta = {
        homepage = "https://github.com/sirikid/readelf-mode";
        license = super.lib.licenses.free;
      };
    };
    mediainfo-mode = esuper.trivialBuild {
      pname = "mediainfo-mode";
      ename = "mediainfo-mode";
      version = "0.2.0";
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "mediainfo-mode";
        rev = "96aed2e3f0f5bd8959a71f983f5f87b12ec9057c";
        sha256 = "1gmmlvrlpwpsdn764l0lwhabhc6ilmiq264ln0x9w3vvrfxy1mvl";
      };
      packageRequires = [ eself.emacs ];
      meta = {
        homepage = "https://github.com/xFA25E/mediainfo-mode";
        license = super.lib.licenses.free;
      };
    };
    youtube-comments = esuper.trivialBuild {
      pname = "youtube-comments";
      ename = "youtube-comments";
      version = "20210222.2247";
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "youtube-comments";
        rev = "621f9e9677241c06bb02a1b7eee46541b2d9c2c2";
        sha256 = "1h4rn7mpj9y22dq8a13rxzm3bbgh4qap4pwvc9mnr7bl6adc94pi";
      };
      packageRequires = [ eself.emacs ];
      meta = {
        homepage = "https://github.com/xFA25E/youtube-comments";
        license = super.lib.licenses.free;
      };
    };
    pueue = esuper.trivialBuild {
      pname = "pueue";
      ename = "pueue";
      version = "0.1.0";
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "pueue";
        rev = "a4467da565833e83c650740719d1c51fba6658eb";
        sha256 = "1k68mnxbsc8k450y0yfnan2vqa86lrb176mxv4pvqfkz4icpxyqf";
      };
      packageRequires = [ eself.emacs eself.bui ];
      meta = {
        homepage = "https://github.com/xFA25E/pueue";
        license = super.lib.licenses.free;
      };
    };
    torrent-mode = esuper.trivialBuild {
      pname = "torrent-mode";
      ename = "torrent-mode";
      version = "20201109.910";
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "torrent-mode";
        rev = "211f4f6ed8759e3817c636a39b1b26e40375aad9";
        sha256 = "1m5q9zdcgx7kvaybm6jgn0p5sqkjdrbrqqfhcnywfirh146xi2hx";
      };
      packageRequires = [ eself.emacs ];
      meta = {
        homepage = "https://github.com/xFA25E/torrent-mode";
        license = super.lib.licenses.free;
      };
    };
    fb2-mode = esuper.trivialBuild {
      pname = "fb2-mode";
      ename = "fb2-mode";
      version = "20201109.910";
      src = super.fetchFromGitHub {
        owner = "spline1986";
        repo = "fb2-mode";
        rev = "edd56bfa3966eb6f7a9a9ed513c6463907122b3d";
        sha256 = "0c8vrljm566pks14bi4zaw3qpwpl27052gq1rm1zwk4qa23cb2mp";
      };
      packageRequires = [ eself.emacs ];
      meta = {
        homepage = "https://github.com/spline1986/fb2-mode";
        license = super.lib.licenses.free;
      };
    };
    # elpa
    flymake = esuper.elpaBuild {
      pname = "flymake";
      ename = "flymake";
      version = "1.1.1";
      src = super.fetchurl {
        url = "https://elpa.gnu.org/packages/flymake-1.1.1.tar";
        sha256 = "0lk2v34b59b24j3hsmi8d0v7fgpwcipv7ka9i88cdgjmjjmzgz5q";
      };
      packageRequires = [ eself.eldoc eself.emacs ];
      meta = {
        homepage = "https://elpa.gnu.org/packages/flymake.html";
        license = super.lib.licenses.free;
      };
    };
    csv-mode = esuper.elpaBuild {
      pname = "csv-mode";
      ename = "csv-mode";
      version = "1.15";

      src = super.fetchurl {
        url = "https://elpa.gnu.org/packages/csv-mode-1.15.tar";
        sha256 = "0pigqhqg5mfza6jdskcr9yvrzdxnd68iyp3vyb8p8wskdacmbiyx";
      };

      packageRequires = [ eself.cl-lib eself.emacs ];
      meta = {
        homepage = "https://elpa.gnu.org/packages/csv-mode.html";
        license = super.lib.licenses.free;
      };
    };
    project = esuper.elpaBuild {
      pname = "project";
      ename = "project";
      version = "0.5.4";
      src = super.fetchurl {
        url = "https://elpa.gnu.org/packages/project-0.5.4.tar";
        sha256 = "0arjvhzzcf8b80w94yvpgfdlhsjwf5jk1r7vcai5a4dg3bi9cxyb";
      };
      packageRequires = [ eself.emacs eself.xref ];
      meta = {
        homepage = "https://elpa.gnu.org/packages/project.html";
        license = super.lib.licenses.free;
      };
    };
    # melpa
    marginalia = esuper.melpaBuild {
      pname = "marginalia";
      ename = "marginalia";
      version = "20210310.1539";
      recipe = super.fetchurl {
        url = "https://github.com/melpa/melpa/raw/f89731cc7ea7ea5436ef080110bba22146632102/recipes/marginalia";
        sha256 = "0wi9fv0xhpm7wz42x7gybqmbvfilmgwkh15rcns53x37zyk6kpxc";
      };
      src = super.fetchFromGitHub {
        owner = "minad";
        repo = "marginalia";
        rev = "0.3";
        sha256 = "1qihw1vq9sysrl6ha23ggycp0n2n1dx1ajkaqfm5vmv8480al07i";
      };
      packageRequires = [ eself.emacs ];
      meta = {
        homepage = "https://github.com/minad/marginalia";
        license = super.lib.licenses.free;
      };
    };
    consult = esuper.melpaBuild {
      pname = "consult";
      ename = "consult";
      version = "20210312.1143";
      recipe = super.fetchurl {
        url = "https://github.com/melpa/melpa/raw/84410868459e976e3c61e6ca5caac311683f706d/recipes/consult";
        sha256 = "17zriam6hgz19ms78c9zh0hvb4b6h5hinrinbmbb2jcwi4cykxs3";
      };
      src = super.fetchFromGitHub {
        owner = "minad";
        repo = "consult";
        rev = "0.6";
        sha256 = "09n3q3dyi83s4fk4z7csnjicbxd69ws4zp4371c1lbxcvvq2fdnd";
      };
      packageRequires = [ eself.emacs ];
      meta = {
        homepage = "https://github.com/minad/consult";
        license = super.lib.licenses.free;
      };
    };
    insert-char-preview = esuper.trivialBuild {
      pname = "insert-char-preview";
      ename = "insert-char-preview";
      version = "20201023.2108";
      recipe = super.fetchurl {
        url = "https://github.com/melpa/melpa/raw/d8f3ae2c4ed25ee4ba28b787270f7abd35d7392c/recipes/insert-char-preview";
        sha256 = "1zs4rv310ry0a5fhji9b0r6xmyh6lr799c49adg3ghy5y6kgp7ry";
      };
      src = super.fetchFromGitLab {
        owner = "matsievskiysv";
        repo = "insert-char-preview";
        rev = "0e4a62b5407fb1bed8920a4c13cf9a91065e15ad";
        sha256 = "0cqc23y9n63a7kl2p1zrfcsxnclfxcszfmbh2hmbrs6q05ys0kzg";
      };
      packageRequires = [ eself.emacs ];
      meta = {
        homepage = "https://gitlab.com/matsievskiysv/insert-char-preview";
        license = super.lib.licenses.free;
      };
    };
  };
  emacsWithPackages = ((super.emacsPackagesGen super.emacs).overrideScope' overrides).emacsWithPackages;
in {
  myEmacs = emacsWithPackages (epkgs: with epkgs; [
    # my
    cyrillic-dvorak-im fb2-mode mediainfo-mode pueue readelf-mode shell-pwd
    skempo torrent-mode youtube-comments

    # melpa
    ace-link acme-theme apache-mode async avy bash-completion bicycle cargo
    cider clipmon clojure-mode consult diff-hl diminish dired-rsync dumb-jump
    edit-indirect eglot emmet-mode fd-dired flycheck flycheck-checkbashisms
    form-feed format-all free-keys gcmh geiser gitconfig-mode gitignore-mode
    htmlize insert-char-preview ipretty json-mode leaf ledger-mode magit
    marginalia mingus mu4e-alert neato-graph-bar nix-mode nov orderless org-mime
    outline-minor-faces pdf-tools php-mode quelpa restclient reverse-im rg
    robots-txt-mode rust-mode sdcv shr-tag-pre-highlight sly sly-asdf
    sly-quicklisp smartparens sqlup-mode sudo-edit transmission vlf web-mode
    wgrep which-key


    # elpa
    csv-mode dired-git-info modus-operandi-theme rainbow-mode sql-indent

    # org
    org-plus-contrib
  ]);

  emacsEditor = super.writeShellScriptBin "emacseditor" ''
    "${self.myEmacs}/bin/emacsclient" --create-frame --alternate-editor=${self.myEmacs}/bin/emacs
  '';
}
