{
  writeShellScriptBin,
  ytdlp,
}:
writeShellScriptBin "ytdlap" ''
  exec ${ytdlp}/bin/ytdlp --format bestaudio/best --extract-audio "$@"
''
