{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    extraPackages = epkgs:
      with epkgs; [
        amded
        apheleia
        async
        avy
        cargo
        cider
        clojure-mode
        consult
        csv-mode
        cyrillic-dvorak-im
        dired-tags
        djvu
        dumb-jump
        ebdb
        edit-indirect
        eglot
        emmet-mode
        envrc
        enwc
        flymake-collection
        htmlize
        ipretty
        json-navigator
        ledger-mode
        link-hint
        magit
        marginalia
        nix-mode
        nixos-options
        notmuch
        nov
        ob-http
        org
        org-contrib
        org-mime
        org-roam
        pcmpl-args
        pdf-tools
        php-mode
        pueue
        rainbow-mode
        restclient
        reverse-im
        rg
        rust-mode
        rx-widget
        sdcwoc
        sly
        sly-asdf
        sly-quicklisp
        sql-indent
        sqlup-mode
        transmission
        tree-sitter
        tree-sitter-langs
        vlf
        web-mode
        wgrep
      ];
    package = pkgs.emacsNativeComp;
  };
}
