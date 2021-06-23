self: super: let

  emacs-overlay-src = super.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "6b6945c6066590764f09a5429af92c2ab1b35f67";
    sha256 = "1sqavxvqp0fyriyv6s3d2j3wa7zg34ipvqpcrcz7l3xya08r7b8p";
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
    make-melpa = { name, version, owner, checksum, deps ? [] }: esuper.melpaBuild {
      pname = name;
      ename = name;
      version = version;
      recipe = super.writeText "recipe" ''
        (${name}
          :fetcher github
          :repo "${owner}/${name}"
          :commit "${version}")
      '';
      src = super.fetchFromGitHub {
        owner = owner;
        repo = name;
        rev = version;
        sha256 = checksum;
      };
      packageRequires = deps;
    };
  in {

    vcomplete = esuper.melpaBuild {
      pname = "vcomplete";
      ename = "vcomplete";
      version = "0.1";
      recipe = super.writeText "recipe" ''
        (vcomplete
          :fetcher git
          :url "https://git.sr.ht/~dsemy/vcomplete"
          :commit "d086a33a1ad88621c24ac081727c7d58df6271ea")
      '';
      src = super.fetchgit {
        url = "https://git.sr.ht/~dsemy/vcomplete";
        rev = "d086a33a1ad88621c24ac081727c7d58df6271ea";
        sha256 = "0ymkyfm0cf7x46mghcg27yr42j23lrgkf12nw5vzsvq4qg38kbh9";
      };
    };

    cyrillic-dvorak-im = make-melpa {
      name = "cyrillic-dvorak-im";
      version = "0.1.0";
      owner = "xFA25E";
      checksum = "12adszd4p9i9glx2chasgq68i6cnxcrwbf5c268jjb5dw4q7ci0n";
    };

    shell-pwd = make-melpa {
      owner = "xFA25E";
      name = "shell-pwd";
      version = "0.1.1";
      checksum = "1wapfjmdxvjk28dmxbqavhc4wgs2hfxxqp7040npjjk0wrz7i83f";
    };

    pueue = make-melpa {
      owner = "xFA25E";
      name = "pueue";
      version = "0.1.0";
      checksum = "0vxk0npry0wi1h7wpzq4bcpkzvv4px5k14rxkjbnznjbhy82kciz";
      deps = [ eself.bui ];
    };

    skempo = make-melpa {
      owner = "xFA25E";
      name = "skempo";
      version = "0.1.0";
      checksum = "0na465f27p6n64sf0pj0aqdi384m1wy3hxcc2d6a67hs39rkyvi9";
      deps = [ eself.parent-mode ];
    };
  };
  emacsWithPackages = ((emacs-overlay.emacsPackagesFor super.emacs).overrideScope' overrides).emacsWithPackages;
  emacsWithPackagesGit = ((emacs-overlay.emacsPackagesFor emacs-overlay.emacsGit).overrideScope' overrides).emacsWithPackages;
  emacsWithPackagesGcc = ((emacs-overlay.emacsPackagesFor emacs-overlay.emacsGcc).overrideScope' overrides).emacsWithPackages;
in {
  myEmacs = emacsWithPackagesGit (epkgs: with epkgs; [

    ace-link async avy bash-completion cargo consult csv-mode cyrillic-dvorak-im
    dired-rsync direnv dumb-jump edit-indirect eglot emacs-default emmet-mode
    format-all htmlize ipretty ledger-mode magit marginalia native-complete
    nix-mode nov org-mime org-plus-contrib outline-minor-faces pcmpl-args
    pdf-tools php-mode pueue rainbow-mode restclient reverse-im rg rust-mode
    sdcv shell-pwd skempo sly sly-asdf sly-quicklisp smartparens sql-indent
    sqlup-mode transmission vcomplete vlf web-mode wgrep

  ]);
}
