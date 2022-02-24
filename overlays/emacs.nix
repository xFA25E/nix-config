self: super: let

  emacs-overlay = import (super.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "4faef893814812fda93cc04081d40a09b2064dd9";
    sha256 = "09hcdrxyhvavajjf8klym2lg81gskz6qxv0p5y2c68nlhqk2a2bs";
  }) self super;

  hyperspec = super.fetchzip {
    name = "common-lisp-hyperspec";
    url = "http://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz";
    sha256 = "1zsi35245m5sfb862ibzy0pzlph48wvlggnqanymhgqkpa1v20ak";
    stripRoot = false;
  };

  emacs-default = super.writeTextDir "share/emacs/site-lisp/default.el" ''
    (setq common-lisp-hyperspec-root "file://${hyperspec}/HyperSpec/")
  '';

  overrides = eself: esuper: let

    githubPackageBuild = {
      owner, repo, version, rev, sha256
      , packageRequires ? []
      , extraRecipe ? ""
      , extraAttributes ? {}
    }: esuper.melpaBuild ({
      inherit version packageRequires;
      pname = repo;
      commit = rev;
      src = super.fetchFromGitHub { inherit repo owner rev sha256; };
      recipe = super.writeText "recipe" ''
        (${repo}
         :fetcher github
         :repo "${owner}/${repo}"
         ${extraRecipe})
      '';
    } // extraAttributes);

  in {

    browse-url-multi = githubPackageBuild {
      owner = "xFA25E";
      repo = "browse-url-multi";
      version = "0.1.1";
      rev = "897a5e42107cc708b74ac37a5eecce39339b6d32";
      sha256 = "0p11jlpdd29k5ljzd1j3829s3lzmw767xa3v0vn3dr1qmq8p9vj6";
    };

    cyrillic-dvorak-im = githubPackageBuild {
      owner = "xFA25E";
      repo = "cyrillic-dvorak-im";
      version = "0.1.0";
      rev = "e23aa4542c8830478af65670d7a96de1d19710a5";
      sha256 = "12adszd4p9i9glx2chasgq68i6cnxcrwbf5c268jjb5dw4q7ci0n";
    };

    pueue = githubPackageBuild {
      owner = "xFA25E";
      repo = "pueue";
      version = "1.0.4";
      rev = "3f18fc91f8c8843549a9e0bdb6320a751eb3a2e9";
      sha256 = "08dxvwrv04dvibnpvrk0lxvp2l9mwvvgxn3vldjm5fq66qylsvil";
      packageRequires = [ eself.transient ];
    };

    rx-widget = githubPackageBuild {
      owner = "xFA25E";
      repo = "rx-widget";
      version = "0.0.1";
      rev = "5fba827d3749d0dda9f951b804ac48fee9231100";
      sha256 = "08w9n0lmamq8px061w9b8jwym7ysg3azx4plv4a4wrj8lx28l6gl";
      packageRequires = [ eself.xr ];
    };

    sdcv = githubPackageBuild {
      owner = "xFA25E";
      repo = "sdcv";
      version = "3.4";
      rev = "932c089a67d354e6bdabdbfecd5710abfb78b332";
      sha256 = "0yp202g7h7cnai8w49918ry6qdx982gj85wpf9j8ylhpam6qvqzk";
    };

    shell-pwd = githubPackageBuild {
      owner = "xFA25E";
      repo = "shell-pwd";
      version = "0.2";
      rev = "21167976f009b57d3f8005eae9ada99043fabdea";
      sha256 = "185sxdjqapbd3m06gd1n8lqbh665dsr0chsd210v975y4fwx2xax";
    };

    skempo = githubPackageBuild {
      owner = "xFA25E";
      repo = "skempo";
      version = "0.2.1";
      rev = "992274f378d9876ed2a5bc812980be9df23349b1";
      sha256 = "1dbhkrixkwm39mxhkn7wv48aybzlzcgig8xabyiwp2fz9hhi7qh3";
      packageRequires = [ eself.parent-mode ];
    };

    xattr = githubPackageBuild {
      owner = "xFA25E";
      repo = "xattr";
      version = "0.0.3";
      rev = "815eef3fd89f5bcc816140466b9c03c706e72157";
      sha256 = "1hnwavh20qyph7i5mj34ryswp6021w1kpri18dkqhmx82id5k117";
      extraRecipe = ":files (\"xattr-core.so\" \"xattr.el\" \"xattr-map.el\")";
      extraAttributes = {
        EMACS_SRC = "${super.emacs}/share/emacs/${super.emacs.version}/src";
        buildInputs = [ super.gnulib ];
        preBuild = "make";
      };
    };

    dired-tags = githubPackageBuild {
      owner = "xFA25E";
      repo = "dired-tags";
      version = "0.0.2";
      rev = "8421bdb6f7e63f60c8c63a438cd26a2f29a7760f";
      sha256 = "1mid1gyfd2zhf3lkn3g2cpyyssqyl4z4qfxgxj1jzq9l65i2vsx8";
      packageRequires = [ eself.xattr ];
    };

  };
  emacsWithPackages = ((emacs-overlay.emacsPackagesFor super.emacs).overrideScope' overrides).emacsWithPackages;
in {
  myEmacs = emacsWithPackages (epkgs: with epkgs; [

    emacs-default

    async avy browse-url-multi cargo consult csv-mode cyrillic-dvorak-im
    dired-tags dumb-jump ebdb edit-indirect eglot emmet-mode envrc
    flymake-shellcheck format-all htmlize ipretty ledger-mode link-hint magit
    marginalia nix-mode notmuch nov ob-http org org-contrib org-mime pcmpl-args
    pdf-tools php-mode pueue rainbow-mode restclient reverse-im rg rust-mode
    rx-widget sdcv shell-pwd skempo sly sly-asdf sly-quicklisp sql-indent
    sqlup-mode transmission vlf web-mode wgrep

  ]);

}
