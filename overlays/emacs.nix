self: super: let

  emacs-overlay-src = super.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "0c6788be21379274f2952810b03c5b64d70044e7";
    sha256 = "1982wvkwbhxg51a9g5bchfzr8g1jlnh9n20c94mkwlpi7ca2m5gd";
  };

  emacs-overlay = import emacs-overlay-src self super;

  emacs-source = let
    src = super.lib.trivial.importJSON "${emacs-overlay-src}/repos/emacs/emacs-master.json";
  in super.fetchgit {
    url = "https://git.savannah.gnu.org/git/emacs.git";
    rev = src.rev;
    sha256 = src.sha256;
  };

  emacs-default = super.writeTextDir "share/emacs/site-lisp/default.el" ''
    (setq find-function-C-source-directory "${emacs-source}/src")
  '';

  overrides = eself: esuper: let
    make-melpa = { name, url, version, sha256, deps ? [], rev ? version }: esuper.melpaBuild {
      pname = name;
      ename = name;
      version = version;
      recipe = super.writeText "recipe" ''
        (${name}
          :fetcher git
          :url "${url}"
          :commit "${rev}")
      '';
      src = super.fetchgit {
        url = url;
        rev = rev;
        sha256 = sha256;
      };
      packageRequires = deps;
    };

  in {

    link-hint = make-melpa {
      name = "link-hint";
      url = "https://github.com/xFA25E/link-hint.el";
      version = "0.1";
      sha256 = "0iy50w7rwrppzhblv0fbfm7anzya776iiyylfmc09sm02j373274";
      rev = "5010bb10052395045a3afcb99177ce3b290d1b0d";
    };

    vcomplete = make-melpa {
      name = "vcomplete";
      url = "https://git.sr.ht/~dsemy/vcomplete";
      version = "0.1";
      sha256 = "0ymkyfm0cf7x46mghcg27yr42j23lrgkf12nw5vzsvq4qg38kbh9";
      rev = "d086a33a1ad88621c24ac081727c7d58df6271ea";
    };

    cyrillic-dvorak-im = make-melpa {
      name = "cyrillic-dvorak-im";
      url = "https://github.com/xFA25E/cyrillic-dvorak-im";
      version = "0.1.0";
      sha256 = "12adszd4p9i9glx2chasgq68i6cnxcrwbf5c268jjb5dw4q7ci0n";
    };

    shell-pwd = make-melpa {
      name = "shell-pwd";
      url = "https://github.com/xFA25E/shell-pwd";
      version = "0.1.1";
      sha256 = "1wapfjmdxvjk28dmxbqavhc4wgs2hfxxqp7040npjjk0wrz7i83f";
    };

    pueue = make-melpa {
      name = "pueue";
      url = "https://github.com/xFA25E/pueue";
      version = "0.1.0";
      sha256 = "0vxk0npry0wi1h7wpzq4bcpkzvv4px5k14rxkjbnznjbhy82kciz";
      deps = [ eself.bui ];
    };

    skempo = make-melpa {
      name = "skempo";
      url = "https://github.com/xFA25E/skempo";
      version = "0.1.0";
      sha256 = "0na465f27p6n64sf0pj0aqdi384m1wy3hxcc2d6a67hs39rkyvi9";
      deps = [ eself.parent-mode ];
    };

    browse-url-multi = make-melpa {
      name = "browse-url-multi";
      url = "https://github.com/xFA25E/browse-url-multi";
      version = "0.1.0";
      sha256 = "0n1g8511qsf5i5kadab1f3agwflsig8nlqdfymj1bvaqkgp0i7x5";
    };

  };
  emacsWithPackages = ((emacs-overlay.emacsPackagesFor super.emacs).overrideScope' overrides).emacsWithPackages;
  emacsWithPackagesGit = ((emacs-overlay.emacsPackagesFor emacs-overlay.emacsGit).overrideScope' overrides).emacsWithPackages;
  emacsWithPackagesGcc = ((emacs-overlay.emacsPackagesFor emacs-overlay.emacsGcc).overrideScope' overrides).emacsWithPackages;
in {
  myEmacs = emacsWithPackagesGit (epkgs: with epkgs; [

    async avy browse-url-multi cargo consult csv-mode cyrillic-dvorak-im
    dired-rsync direnv dumb-jump ebdb edit-indirect eglot emacs-default
    emmet-mode format-all htmlize ipretty ledger-mode link-hint magit marginalia
    nix-mode nov org-mime org-plus-contrib outline-minor-faces pcmpl-args
    pdf-tools php-mode pueue rainbow-mode restclient reverse-im rg rust-mode
    sdcv shell-pwd skempo sly sly-asdf sly-quicklisp smartparens sql-indent
    sqlup-mode transmission vcomplete vlf web-mode wgrep

  ]);

}
