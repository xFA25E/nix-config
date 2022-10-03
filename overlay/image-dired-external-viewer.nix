{
  mpv,
  sxiv,
  writeShellScriptBin,
}:
writeShellScriptBin "image-dired-external-viewer" ''
  set -u
  f=$1
  mime=$(file --brief --mime-type "$f")
  case "$mime" in
    image/*)         exec ${sxiv}/bin/sxiv "$f" ;;
    video/*|audio/*) exec ${mpv}/bin/mpv --profile=gui "$f" ;;
    *)               exit 1 ;;
  esac
''
