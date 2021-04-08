self: super: let
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
      checksum = "08f9spkv54lmkg06p2mhhk164rcwvp6fvqh275iwsclwjaylbljh";
    };

    pueue = esuper.melpaBuild {
      pname = "pueue";
      ename = "pueue";
      version = "20210311";
      recipe = super.writeText "recipe" ''
        (pueue
          :fetcher github
          :repo "xFA25E/pueue"
          :commit "a4467da565833e83c650740719d1c51fba6658eb")
      '';
      src = super.fetchFromGitHub {
        owner = "xFA25E";
        repo = "pueue";
        rev = "a4467da565833e83c650740719d1c51fba6658eb";
        sha256 = "1k68mnxbsc8k450y0yfnan2vqa86lrb176mxv4pvqfkz4icpxyqf";
      };
      packageRequires = [ eself.bui ];
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
    # my

    cyrillic-dvorak-im pueue shell-pwd skempo

    # melpa

    ace-link apache-mode async avy bash-completion bicycle cargo cider
    clojure-mode consult diff-hl dired-rsync direnv dumb-jump edit-indirect
    eglot emmet-mode fd-dired flycheck flycheck-checkbashisms form-feed
    format-all # geiser
    gitconfig-mode gitignore-mode htmlize insert-char-preview
    ipretty json-mode ledger-mode magit marginalia nix-mode nov orderless
    org-mime outline-minor-faces pdf-tools php-mode restclient reverse-im rg
    robots-txt-mode rust-mode sdcv sly sly-asdf sly-quicklisp smartparens
    sqlup-mode sudo-edit transmission vlf web-mode wgrep

    # elpa
    csv-mode # modus-themes
    rainbow-mode sql-indent

    # org
    org-plus-contrib
  ]);

  emacsEditor = super.writeShellScriptBin "emacseditor" ''
    "${self.myEmacs}/bin/emacsclient" --create-frame --alternate-editor=${self.myEmacs}/bin/emacs "$@"
  '';
}
