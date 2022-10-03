{
  select_music_dir,
  writeShellScriptBin,
  yt-dlp,
}:
writeShellScriptBin "ytdlpam" ''
  set -eu
  DIR=$(${select_music_dir}/bin/select_music_dir)
  test -n "$DIR"

  FMT='%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s'
  exec ${yt-dlp}/bin/yt-dlp --format bestaudio/best --extract-audio \
                            --yes-playlist --paths "$DIR" --output "$FMT" "$@"
''
