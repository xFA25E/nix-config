self: super: let
  emacs-source = super.fetchFromGitHub {
    owner = "emacs-mirror";
    repo = "emacs";
    rev = "emacs-27.1";
    sha256 = "1i50ksf96fxa3ymdb1irpc82vi67861sr4xlcmh9f64qw9imm3ks";
  };

  emacs-default = super.writeTextDir "share/emacs/site-lisp/default.el" ''
    (setq find-function-C-source-directory "${emacs-source}/src")
  '';

  emacs-overlay = import (builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz) self super;

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

    # my

    cyrillic-dvorak-im = make-melpa {
      name = "cyrillic-dvorak-im";
      version = "0.1.0";
      owner = "xFA25E";
      checksum = "12adszd4p9i9glx2chasgq68i6cnxcrwbf5c268jjb5dw4q7ci0n";
    };

    shell-pwd = make-melpa {
      owner = "xFA25E";
      name = "shell-pwd";
      version = "0.1.0";
      checksum = "1463a8x9xg02szwrn8iq2v6bg7k7dm4fhg34k59li6kfl9n9fbiw";
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
      checksum = "1j0l12pqvjikry3hj5yz01x8l5qisl10fyzsj3n0f7299p49ck01";
      deps = [ eself.parent-mode ];
    };
  };
  emacsWithPackages = ((emacs-overlay.emacsPackagesFor super.emacs).overrideScope' overrides).emacsWithPackages;
in {
  myEmacs = emacsWithPackages (epkgs: with epkgs; [
    emacs-default

    # my
    cyrillic-dvorak-im pueue shell-pwd skempo

    # melpa

    ace-link async avy bash-completion bicycle cargo consult dired-rsync direnv
    dumb-jump edit-indirect eglot emmet-mode fd-dired form-feed format-all
    htmlize insert-char-preview ipretty ledger-mode magit marginalia
    native-complete nix-mode nov orderless org-mime outline-minor-faces
    pdf-tools php-mode restclient reverse-im rg rust-mode sdcv sly sly-asdf
    sly-quicklisp smartparens sqlup-mode transmission vlf web-mode wgrep

    # elpa
    csv-mode rainbow-mode sql-indent

    # org
    org-plus-contrib
  ]);
}
