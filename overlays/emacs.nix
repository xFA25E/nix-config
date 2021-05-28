self: super: let

  emacs-overlay-src = super.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "cbc63fe7656be94bf0ebd366740dbc0112cf7069";
    sha256 = "1gvflm1l5cfj2a2k25f0wgfbymwwvki48nw97g05jd9i9l891yh2";
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
          :commit "5e055dc55665565e29f13a8e45986f194a41b2f2")
      '';
      src = super.fetchgit {
        url = "https://git.sr.ht/~dsemy/vcomplete";
        rev = "5e055dc55665565e29f13a8e45986f194a41b2f2";
        sha256 = "0vrvvwfvjaayajd6l48ygsyk1w59y5vs69k4gby2sjbpii0l0a3x";
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
      checksum = "0a8ga9svfx06z8d8pd1wkz2vvglfckfv4aqbviy809a9x38mw2ng";
      deps = [ eself.parent-mode ];
    };
  };
  # emacsWithPackages = ((emacs-overlay.emacsPackagesFor super.emacs).overrideScope' overrides).emacsWithPackages;
  emacsWithPackages = ((emacs-overlay.emacsPackagesFor emacs-overlay.emacsGit).overrideScope' overrides).emacsWithPackages;
in {
  myEmacs = emacsWithPackages (epkgs: with epkgs; [

    ace-link async avy bash-completion cargo consult csv-mode cyrillic-dvorak-im
    dired-rsync direnv dumb-jump edit-indirect eglot emacs-default emmet-mode
    fd-dired format-all htmlize ipretty ledger-mode magit marginalia
    native-complete nix-mode nov orderless org-mime org-plus-contrib
    outline-minor-faces pdf-tools php-mode pueue rainbow-mode restclient
    reverse-im rg rust-mode sdcv shell-pwd skempo sly sly-asdf sly-quicklisp
    smartparens sql-indent sqlup-mode transmission vcomplete vlf web-mode wgrep

  ]);
}
