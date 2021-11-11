let
  pkgs = import <nixpkgs> {};
  quicklisp-lisp = builtins.fetchurl https://beta.quicklisp.org/quicklisp.lisp;
  quickstart = pkgs.writeShellScriptBin "quickstart" ''
    ${pkgs.sbcl}/bin/sbcl \
        --non-interactive \
        --no-userinit \
        --load "${quicklisp-lisp}" \
        --eval "(quicklisp-quickstart:install :path #p\".quicklisp/\")"
  '';
  init-lisp = pkgs.writeText "init.lisp" ''
    #-quicklisp
    (let ((quicklisp-init ".quicklisp/setup.lisp"))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))

    #+quicklisp
    (let ((docs-dir (uiop:run-program '("xdg-user-dir" "DOCUMENTS") :output '(:string :stripped t))))
      (pushnew (uiop:subpathname* docs-dir "projects/common-lisp/")
               ql:*local-project-directories*))
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
  name = "-dev-env";
  buildInputs =  [ quickstart sbcl ecl ccl clisp abcl ];
}
