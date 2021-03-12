self: super: let
  overrides = eself: esuper: {
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
    consult marginalia insert-char-preview

    # melpa
    ace-link acme-theme apache-mode async avy bash-completion bicycle cargo
    cider clipmon clojure-mode  diff-hl diminish dired-rsync dumb-jump
    edit-indirect eglot emmet-mode fd-dired flycheck flycheck-checkbashisms
    form-feed format-all free-keys gcmh geiser gitconfig-mode gitignore-mode
    htmlize  ipretty json-mode leaf ledger-mode magit
    mingus mu4e-alert neato-graph-bar nix-mode nov orderless org-mime
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
