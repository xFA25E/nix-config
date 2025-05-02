{
  ffmpeg,
  writeShellScriptBin,
}:
writeShellScriptBin "strip_video" ''
  set -eu

  case "$1" in
      mp3)
          shift
          for video in "$@"; do
              ${ffmpeg}/bin/ffmpeg -y -loglevel repeat+info -i file:"$video" \
                       -vn -acodec libmp3lame -q:a 5 file:"''${video%.*}.mp3"
          done
          ;;
      opus)
          shift
          for video in "$@"; do
              ${ffmpeg}/bin/ffmpeg -n -i "$video" -vn -acodec copy \
                       "''${video%.*}.opus"
          done
          ;;
      m4a)
          shift
          for video in "$@"; do
              ${ffmpeg}/bin/ffmpeg -n -i "$video" -vn -acodec copy \
                       "''${video%.*}.m4a"
          done
          ;;
      *)
          echo "Select output format: mp3, m4a, opus" >&2
          exit 1
  esac
''
