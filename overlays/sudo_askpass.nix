{
  pass,
  writeShellScriptBin,
}:
writeShellScriptBin "sudo_askpass" "exec ${pass}/bin/pass sudo"
