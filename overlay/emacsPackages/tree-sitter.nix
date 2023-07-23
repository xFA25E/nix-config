final: prev: efinal: eprev:
eprev.tree-sitter.overrideAttrs ({patches ? [], ...}: {
  patches =
    patches
    ++ map final.fetchpatch [
      {
        url = "https://github.com/emacs-tree-sitter/elisp-tree-sitter/pull/231.diff";
        hash = "sha256-nJghRLLEsUvXx8kz6ZjX/NGs92JcrGMJBUlW+7YtVMg=";
      }
    ];
})
