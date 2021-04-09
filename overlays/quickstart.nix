self: super: let
  quickstart-lisp = builtins.fetchurl https://beta.quicklisp.org/quicklisp.lisp;
  quicklisp-dir = "\${QUICKLISP:-\${XDG_CACHE_HOME:-$HOME/.cache}/quicklisp}";
in {
  quickstart = super.writeShellScriptBin "quickstart" ''
    ${self.sbcl}/bin/sbcl \
        --non-interactive \
        --no-userinit \
        --load "${quickstart-lisp}" \
        --eval "(quicklisp-quickstart:install :path #p\"${quicklisp-dir}/\")"
  '';
}
