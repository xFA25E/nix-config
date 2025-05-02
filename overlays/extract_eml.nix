{
  mu,
  writeShellScriptBin,
}:
writeShellScriptBin "extract_eml" ''
  set -eu
  test -e "$1"
  basename=$(basename "$1" .eml)
  mkdir -p "$basename"
  ${mu}/bin/mu extract --save-all --target-dir "$basename" "$1"
  ${mu}/bin/mu view "$1" >"$basename/mail.txt"
''
