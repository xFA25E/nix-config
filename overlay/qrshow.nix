{
  qrencode,
  sxiv,
  writeShellScriptBin,
}:
writeShellScriptBin "qrshow" ''
  set -u
  text=$1
  out=$(mktemp /tmp/qrXXXXX.png)
  ${qrencode}/bin/qrencode -o "$out" -s 15 "$text"
  ${sxiv}/bin/sxiv "$out"
  rm "$out"
''
