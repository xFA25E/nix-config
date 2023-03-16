final: prev: efinal: eprev:
eprev.emacsql.overrideAttrs ({
  buildInputs ? [],
  postBuild ? "",
  postInstall ? "",
  version,
  ...
}: {
  buildInputs = buildInputs ++ [final.sqlite];

  postBuild =
    postBuild
    + ''
      cd source/sqlite
      make
      cd -
    '';

  postInstall =
    postInstall
    + "\n"
    + ''
      install -m=755 -D source/sqlite/emacsql-sqlite \
        $out/share/emacs/site-lisp/elpa/emacsql-sqlite-${version}/sqlite/emacsql-sqlite
    '';

  stripDebugList = ["share"];
})
