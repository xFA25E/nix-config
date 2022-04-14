self: super: let

  emacs-overlay = import (super.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "d570feea6d181b095b26a34acb2e4ef095ecf69b";
    sha256 = "0p6nb5jj6vlkgvgh7fkn7chk031n01adm2q8pxhxgadhmbirh7sf";
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
      rev = "fe6ca86be60e148e613ae23995fbc75d6838e687";
      sha256 = "1fw2vxjn0nj5kpbynznala8y49hvp3m53062d0r1s06nigj87lbk";
    };

    cyrillic-dvorak-im = githubPackageBuild {
      owner = "xFA25E";
      repo = "cyrillic-dvorak-im";
      version = "0.1.0";
      rev = "e23aa4542c8830478af65670d7a96de1d19710a5";
      sha256 = "12adszd4p9i9glx2chasgq68i6cnxcrwbf5c268jjb5dw4q7ci0n";
    };

    dired-tags = githubPackageBuild {
      owner = "xFA25E";
      repo = "dired-tags";
      version = "0.0.2";
      rev = "8421bdb6f7e63f60c8c63a438cd26a2f29a7760f";
      sha256 = "1mid1gyfd2zhf3lkn3g2cpyyssqyl4z4qfxgxj1jzq9l65i2vsx8";
      packageRequires = [ eself.xattr ];
    };

    pcmpl-args-parted = githubPackageBuild {
      owner = "xFA25E";
      repo = "pcmpl-args-parted";
      version = "0.0.1";
      rev = "27f4048d93c11954cbb4376ecd51132035b12f68";
      sha256 = "1vwvbap1ydfvk0s87rvv0bv0zpjc0isawhxhsd19hsdlafzdfwyb";
      packageRequires = [ eself.pcmpl-args ];
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

    sdcwoc = githubPackageBuild {
      owner = "xFA25E";
      repo = "sdcwoc";
      version = "0.0.1";
      rev = "e33e7082c207cce33fdb74fd541b9d2464cbfc7b";
      sha256 = "0v65hx0a97myc0mi7z9wjimjzlv4fjg4d8qgggk4j8w0bah3vjnf";
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
      rev = "2841bf6381f1fc5edcc4850c3707f1b194b9a477";
      sha256 = "0lm9lp6lmg84nvvpjkfra38h1pqqjkpcsyq4ggcr4yk2i0ahc4q2";
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

    yt-com = githubPackageBuild {
      owner = "xFA25E";
      repo = "yt-com";
      version = "1.0.0";
      rev = "be426f485c031664c2fa578b5c1a8dae954c4f8d";
      sha256 = "1jmd0h147q7mflna30i21hshfnc6pi3n9qhhsdz81xbxyyamrzhw";
    };

  };

  emacs28stage1 = super.emacs.override ({
    srcRepo = true;
    nativeComp = true;
    withSQLite3 = true;
  });

  emacs28stage2 = emacs28stage1.overrideAttrs (old: {
    name = "emacs-28.1";
    version = "28.1";
    src = super.fetchFromSavannah {
      repo = "emacs";
      rev = "emacs-28.1";
      sha256 = "01mfwl6lh79f9icrfw07dns3g0nqwc06k6fm3gr45iv1bjgg0z8g";
    };
    patches = [ ];
    postPatch = old.postPatch + ''
      substituteInPlace lisp/loadup.el \
        --replace '(emacs-repository-get-version)' '"emacs-28.1"' \
        --replace '(emacs-repository-get-branch)' '"emacs-28"'
    '';
  });

  emacs28stage3 = emacs28stage2.overrideAttrs (old: {
    passthru = old.passthru // {
      pkgs = emacs-overlay.emacsPackagesFor emacs28stage3;
    };
  });

  emacs28 = (emacs-overlay.emacsPackagesFor emacs28stage3).overrideScope' overrides;
in {
  myEmacs = emacs28.emacsWithPackages (epkgs: with epkgs; [

    emacs-default

    async avy browse-url-multi cargo consult csv-mode cyrillic-dvorak-im
    dired-tags dumb-jump ebdb edit-indirect eglot emmet-mode envrc
    flymake-shellcheck format-all htmlize ipretty ledger-mode link-hint magit
    marginalia nix-mode notmuch nov ob-http org org-contrib org-mime pcmpl-args
    pdf-tools php-mode pueue rainbow-mode restclient reverse-im rg rust-mode
    rx-widget sdcwoc shell-pwd skempo sly sly-asdf sly-quicklisp sql-indent
    sqlup-mode transmission vlf web-mode wgrep

    enwc pcmpl-args-parted yt-com telega

  ]);

}
