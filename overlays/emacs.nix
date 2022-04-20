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
      version = "0.0.2";
      rev = "88fe1d448506ee66d4c5c1a5e05023c02a6a1808";
      sha256 = "1nkivsfp2dd1c9alabwixdwibp2fjkk94vab6nva7kxz1mrwynns";
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
      version = "0.2.2";
      rev = "c5e229ba4f811f890b5cb56854205a5ac3302ce8";
      sha256 = "0xsr6819qaad0y8rhfrf555c9w7cmgbgkf1a2svgbp7rhhnywl0a";
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
      version = "1.0.1";
      rev = "7ad6b77ee76de667e1a0c7f7d383afd879ae6223";
      sha256 = "0yi6aqs1hrjkgnn0qlv6bvn6fcf4pcdw5lnhq4ww04q0hjxjm52d";
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

    async avy cargo consult csv-mode cyrillic-dvorak-im dired-tags dumb-jump
    ebdb edit-indirect eglot emmet-mode envrc flymake-shellcheck format-all
    htmlize ipretty ledger-mode link-hint magit marginalia nix-mode notmuch nov
    ob-http org org-contrib org-mime pcmpl-args pdf-tools php-mode pueue
    rainbow-mode restclient reverse-im rg rust-mode rx-widget sdcwoc shell-pwd
    skempo sly sly-asdf sly-quicklisp sql-indent sqlup-mode transmission vlf
    web-mode wgrep

    enwc pcmpl-args-parted yt-com

  ]);

}
