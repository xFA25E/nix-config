let
  pkgs = import <nixpkgs> {};

  PROJECT_ROOT = builtins.toString ./.;
  QUICKLISP_DIR = "${PROJECT_ROOT}/.quicklisp";
  ASDF_OUTPUT_TRANSLATIONS = ''
    (:output-translations
     :ignore-inherited-configuration
     (t ("${PROJECT_ROOT}" ".common-lisp" :implementation)))
  '';

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

    #+quicklisp
    (pushnew #P"${builtins.toString ./../.}/" ql:*local-project-directories*)
  '';

  make-implementation = name: pkg: flags: pkgs.symlinkJoin {
    name = name;
    paths = [ pkg ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/${name} --add-flags '${flags}'
    '';
  };

  sbcl = make-implementation "sbcl" pkgs.sbcl "--userinit ${init-lisp}";
  ecl = make-implementation "ecl" pkgs.ecl "--norc --load ${init-lisp}";
  ccl = make-implementation "ccl" pkgs.ccl "--no-init --load ${init-lisp}";
  clisp = make-implementation "clisp" pkgs.clisp "-norc -i ${init-lisp}";
  abcl = make-implementation "abcl" pkgs.abcl "--noinit --load ${init-lisp}";

in pkgs.mkShell {
  inherit ASDF_OUTPUT_TRANSLATIONS;
  buildInputs =  [ quickstart sbcl ecl ccl clisp abcl ];
  shellHook = ''
    [ -d "${QUICKLISP_DIR}" ] || quickstart
  '';
}
