self: super: let

  emacs-overlay-src = super.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "1ad30847e88737fde2421da86ec48410e3d414a0";
    sha256 = "1zlfrf73gwbkq5r6j85f25dkvlypfc5ni38mxh01k2wq98lc92vf";
  };

  emacs-overlay = import emacs-overlay-src self super;

  emacs-source = let
    src = super.lib.trivial.importJSON "${emacs-overlay-src}/repos/emacs/emacs-master.json";
  in super.fetchgit {
    url = "https://git.savannah.gnu.org/git/emacs.git";
    rev = src.rev;
    sha256 = src.sha256;
  };

  hyperspec = super.fetchzip {
    name = "common-lisp-hyperspec";
    url = "http://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz";
    sha256 = "1zsi35245m5sfb862ibzy0pzlph48wvlggnqanymhgqkpa1v20ak";
    stripRoot = false;
  };

  emacs-default = super.writeTextDir "share/emacs/site-lisp/default.el" ''
    (setq common-lisp-hyperspec-root "file://${hyperspec}/HyperSpec/")
  '';

  # emacs-default = super.writeTextDir "share/emacs/site-lisp/default.el" ''
  #   (setq common-lisp-hyperspec-root "${self.hyperspec}/HyperSpec/")
  #   (setq find-function-C-source-directory "${emacs-source}/src")
  # '';

  overrides = eself: esuper: let

    githubPackageBuild = { owner, repo, version, rev, sha256, packageRequires ? [] }: esuper.melpaBuild {
      inherit version packageRequires;
      pname = repo;
      commit = rev;
      src = super.fetchFromGitHub { inherit repo owner rev sha256; };
      recipe = super.writeText "recipe" "(${repo} :repo \"${owner}/${repo}\" :fetcher github)";
    };

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
      rev = "e6f5119ed4f85c3f686c0b5af7ff7aca36c9da88";
      sha256 = "1ngdapdrajsby4v04av05lvh8lx7vl96mbcscx856q8gh7r647ck";
      packageRequires = [ eself.transient ];
    };

    sdcwoc = githubPackageBuild {
      owner = "xFA25E";
      repo = "sdcwoc";
      version = "0.0.1";
      rev = "ef9acd43e93edfc8ae5474dd02feee8331ec88a0";
      sha256 = "1mj2h8673vhi0pj3343wwbchdgv7fvz2gchd0cjaymi535156jgg";
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
      version = "0.1.2";
      rev = "74206140cc8e93d105c88be520db54c4ae87fc1a";
      sha256 = "0fzvaznnrcdnkl4jy1x04apksjrhr2fc6gak5xkbqnjib63rd3v9";
      packageRequires = [ eself.parent-mode ];
    };

  };
  emacsWithPackages = ((emacs-overlay.emacsPackagesFor super.emacs).overrideScope' overrides).emacsWithPackages;
  emacsWithPackagesGit = ((emacs-overlay.emacsPackagesFor emacs-overlay.emacsGit).overrideScope' overrides).emacsWithPackages;
  emacsWithPackagesGcc = ((emacs-overlay.emacsPackagesFor emacs-overlay.emacsGcc).overrideScope' overrides).emacsWithPackages;
in {
  myEmacs = emacsWithPackages (epkgs: with epkgs; [

    emacs-default

    async avy browse-url-multi cargo consult csv-mode cyrillic-dvorak-im dumb-jump ebdb
    edit-indirect eglot emmet-mode envrc flymake-shellcheck format-all htmlize ipretty ledger-mode
    link-hint magit marginalia nix-mode notmuch nov ob-http org-mime org-plus-contrib pcmpl-args
    pdf-tools php-mode pueue rainbow-mode restclient reverse-im rg rust-mode sdcwoc shell-pwd skempo
    sly sly-asdf sly-quicklisp smartparens sql-indent sqlup-mode transmission vlf web-mode wgrep

  ]);

}
