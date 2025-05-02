{
  writeShellScriptBin,
  xclip,
}:
writeShellScriptBin "image_clipboard" ''
  set -u
  f=$1
  type="$(file --brief --mime-type "$f")"
  exec ${xclip}/bin/xclip -t "$type" "$f"
''
