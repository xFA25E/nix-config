self: super: let

  emacs-overlay-src = super.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "3378b6cd8b52f636e4d3cb1b4479f537c434e782";
    sha256 = "0llqicxac77dw2qzqip4gyf5gv4gvjz5fwr4zjzbx4hfbvdjv50w";
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

    sdcv = make-melpa {
      name = "sdcv";
      url = "https://github.com/xFA25E/sdcv";
      version = "3.4";
      sha256 = "160g6ia3z3h7y1rdmjpbf0jqkpw1qzby33hijws3qr9sx6nyj7qz";
      rev = "5225b57b09bdd78e8d074899ebf505e9122e8ee5";
      deps = [ eself.posframe ];
    };

    pcmpl-args = make-melpa {
      name = "pcmpl-args";
      url = "https://github.com/xFA25E/pcmpl-args.el";
      version = "0.1.2";
      sha256 = "1jzmqki0s87krbmw7jf9fmv3awbf2jw5bq099ha0m2harc2gald6";
      rev = "fb3c73ac7cea7be1836c47ae01335d810939e4ee";
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
      version = "0.2";
      sha256 = "185sxdjqapbd3m06gd1n8lqbh665dsr0chsd210v975y4fwx2xax";
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
      version = "0.1.1";
      sha256 = "0p11jlpdd29k5ljzd1j3829s3lzmw767xa3v0vn3dr1qmq8p9vj6";
    };

  };
  emacsWithPackages = ((emacs-overlay.emacsPackagesFor super.emacs).overrideScope' overrides).emacsWithPackages;
  emacsWithPackagesGit = ((emacs-overlay.emacsPackagesFor emacs-overlay.emacsGit).overrideScope' overrides).emacsWithPackages;
  emacsWithPackagesGcc = ((emacs-overlay.emacsPackagesFor emacs-overlay.emacsGcc).overrideScope' overrides).emacsWithPackages;
in {
  myEmacs = emacsWithPackages (epkgs: with epkgs; [

    async avy browse-url-multi cargo consult csv-mode cyrillic-dvorak-im
    dired-rsync direnv dumb-jump ebdb edit-indirect eglot emmet-mode
    flymake-shellcheck format-all htmlize ipretty ledger-mode link-hint magit
    marginalia nix-mode notmuch nov org-mime org-plus-contrib pcmpl-args
    pdf-tools php-mode pueue rainbow-mode restclient reverse-im rg rust-mode
    sdcv shell-pwd skempo sly sly-asdf sly-quicklisp smartparens sql-indent
    sqlup-mode transmission vlf web-mode wgrep

  ]);

}
