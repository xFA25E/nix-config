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

    pcmpl-args = make-melpa {
      name = "pcmpl-args";
      version = "0.1.1";

      url = "https://github.com/xFA25E/pcmpl-args.el";
      sha256 = "13vqrsp2m61id7k5phywyjpmis8zz6swlznhr9s4wgmzpc9kg90n";
      rev = "6b29a87d1243e5d3b83ce07ad40548333d00af21";

      # url = "https://github.com/sirikid/pcmpl-args.el";
      # sha256 = "1dj6k75fnkynssy8pdawwppv6s30f8g8d6xbky79i7l2wmjkcdid";
      # rev = "49fc9ef2e15b276bff799b6cc1fab96527a25403";

    };

    link-hint = make-melpa {
      name = "link-hint";
      url = "https://github.com/noctuid/link-hint.el";
      version = "0.1";
      sha256 = "0v2g9gzf2v88ag59q1pf5vhd4qjnz3g4i6gzl27k6fi7pvlxdn39";
      rev = "9fbf196d155016d9b8471a99318ed67a086cf257";
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
      sha256 = "0lp4443nrmxr3ass33lha5nlk03yb8lqb6gf88n343gdwdhgzaxv";
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
  myEmacs = emacsWithPackagesGit (epkgs: with epkgs; [

    async avy browse-url-multi cargo consult csv-mode cyrillic-dvorak-im
    dired-rsync direnv dumb-jump ebdb edit-indirect eglot emacs-default
    emmet-mode format-all htmlize ipretty ledger-mode link-hint magit marginalia
    nix-mode notmuch nov org-mime org-plus-contrib outline-minor-faces
    pcmpl-args pdf-tools php-mode pueue rainbow-mode restclient reverse-im rg
    rust-mode sdcv shell-pwd skempo sly sly-asdf sly-quicklisp smartparens
    sql-indent sqlup-mode transmission vcomplete vlf web-mode wgrep

    flymake-shellcheck

  ]);

}
