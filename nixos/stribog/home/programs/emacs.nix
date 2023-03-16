{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    extraPackages = epkgs:
      with epkgs; [
        affe
        amded
        apheleia
        async
        avy
        avy-embark-collect
        cargo
        cider
        clojure-mode
        consult
        csharp-mode
        csproj-mode
        csv-mode
        cyrillic-dvorak-im
        dired-atool-transient
        dired-tags
        djvu
        dotnet
        dumb-jump
        ebdb
        edit-indirect
        eglot
        embark
        embark-consult
        emmet-mode
        envrc
        flymake-collection
        htmlize
        ipretty
        json-navigator
        ledger-mode
        link-hint
        magit
        marginalia
        nix-mode
        notmuch
        nov
        orderless
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
        tempo-extra
        transmission
        tree-sitter
        tree-sitter-langs
        vertico
        vlf
        web-mode
        wgrep
      ];
  };
}
