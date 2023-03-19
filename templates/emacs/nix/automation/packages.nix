{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  l = inputs.nixpkgs.lib // builtins;
in {
  eldev = nixpkgs.stdenv.mkDerivation {
    name = "eldev";
    src = inputs.eldev;
    dontUnpack = true;
    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;
    nativeBuildInputs = [nixpkgs.emacs];
    installPhase = ''
      mkdir -p $out/bin
      cp $src/bin/eldev $out/bin/
    '';
  };

  emacsPackageTests = nixpkgs.runCommand "emacs-package-tests" {} ''
    cp ${l.sources.sourceFilesBySuffices inputs.self [".el"]}/*.el .
    loadfiles=""
    for file in *.el ; do
      loadfiles="$loadfiles -l $file"
    done
    ${nixpkgs.emacs}/bin/emacs -Q -module-assertions -batch \
      -L . $loadfiles -f ert-run-tests-batch-and-exit \
      && touch $out
  '';
}
