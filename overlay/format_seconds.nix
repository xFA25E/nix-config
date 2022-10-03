{
  emacs,
  lib,
  writeShellScriptBin,
}: let
  script = lib.strings.escapeShellArg ''
    (let ((format (pop command-line-args-left))
        (seconds (string-to-number (pop command-line-args-left))))
    (princ (format-seconds format seconds)))
  '';
in
  writeShellScriptBin "format_seconds" ''
    set -u
    exec ${emacs}/bin/emacs -Q --batch --eval ${script} "$1" "$2"
  ''
