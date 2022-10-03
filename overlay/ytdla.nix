{
  writeShellScriptBin,
  ytdl,
}:
writeShellScriptBin "ytdla" ''
  exec ${ytdl}/bin/ytdl --format bestaudio/best --extract-audio "$@"
''
