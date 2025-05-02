{
  select_music_dir,
  writeShellScriptBin,
  yt-dlp,
}:
writeShellScriptBin "ytdlm" ''
  set -eu
  DIR=$(${select_music_dir}/bin/select_music_dir)
  test -n "$DIR"

  exec ${yt-dlp}/bin/yt-dlp --format bestaudio/best --extract-audio \
                            --embed-thumbnail --embed-metadata \
                            --paths "$DIR" --output '%(title)s.%(ext)s' "$@"
''
