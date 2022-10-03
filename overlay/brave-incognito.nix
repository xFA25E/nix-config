{
  brave,
  writeShellScriptBin,
}:
writeShellScriptBin "brave-incognito" ''
  exec ${brave}/bin/brave -incognito "$@"
''
