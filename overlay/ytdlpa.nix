{
  writeShellScriptBin,
  ytdlp,
}:
writeShellScriptBin "ytdlpa" ''
  exec ${ytdlp}/bin/ytdlp --format bestaudio/best --extract-audio "$@"
''
