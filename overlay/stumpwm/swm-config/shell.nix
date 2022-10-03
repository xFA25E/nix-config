{ pkgs ? import <nixpkgs> {} }:
let

  PROJECT_ROOT = builtins.toString ./.;
  QUICKLISP_DIR = "${PROJECT_ROOT}/.quicklisp";

  quicklisp-lisp = builtins.fetchurl https://beta.quicklisp.org/quicklisp.lisp;
  quickstart = pkgs.writeShellScriptBin "quickstart" ''
    ${pkgs.sbcl}/bin/sbcl \
        --non-interactive \
        --no-userinit \
        --load "${quicklisp-lisp}" \
        --eval "(quicklisp-quickstart:install :path #P\"${QUICKLISP_DIR}/\")"
  '';

  init-lisp = pkgs.writeText "init.lisp" ''
    #-quicklisp
    (let ((quicklisp-init #P"${QUICKLISP_DIR}/setup.lisp"))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))
  '';

  sbcl = pkgs.symlinkJoin {
    name = "sbcl";
    paths = [ pkgs.sbcl ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/sbcl --add-flags '--userinit ${init-lisp}'
    '';
  };

in pkgs.mkShell {
  CL_SOURCE_REGISTRY="${PROJECT_ROOT}:";
  ASDF_OUTPUT_TRANSLATIONS = ''
    (:output-translations
     :ignore-inherited-configuration
     (t ("${PROJECT_ROOT}" ".common-lisp" :implementation)))
  '';
  buildInputs =  [ quickstart sbcl ];
  shellHook = ''
    [ -d "${QUICKLISP_DIR}" ] || quickstart
  '';
}
