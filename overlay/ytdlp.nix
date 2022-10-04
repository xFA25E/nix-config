{
  writeShellScriptBin,
  ytdl,
}:
writeShellScriptBin "ytdlp" ''
  FMT='%(playlist_uploader)s - %(playlist)s - %(playlist_index)s - %(channel)s - %(upload_date)s - %(title)s.%(ext)s'
  exec ${ytdl}/bin/ytdl --output "$FMT" "$@"
''
