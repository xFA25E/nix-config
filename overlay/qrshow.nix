{
  qrencode,
  nsxiv,
  writeShellScriptBin,
}:
writeShellScriptBin "qrshow" ''
  set -u
  text=$1
  out=$(mktemp /tmp/qrXXXXX.png)
  ${qrencode}/bin/qrencode -o "$out" -s 15 "$text"
  ${nsxiv}/bin/nsxiv "$out"
  rm "$out"
''
