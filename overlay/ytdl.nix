{
  filename_put_duration,
  writeShellScriptBin,
  yt-dlp,
}: let
  fpd = "${filename_put_duration}/bin/filename_put_duration";
in
  writeShellScriptBin "ytdl" ''
    DIR=''${YTDL_DIR:-''${XDG_VIDEOS_DIR:-''${HOME}/Videos}/youtube}
    exec ${yt-dlp}/bin/yt-dlp --paths "$DIR" --exec '${fpd} {}' "$@"
  ''
