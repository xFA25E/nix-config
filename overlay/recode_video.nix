{
  ffmpeg,
  writeShellScriptBin,
}:
writeShellScriptBin "recode_video" ''
  set -eu

  case "$1" in
      avc) vcodec=libx264 ;;
      hevc) vcodec=libx265 ;;
      *) echo "Select vcodec: avc, hevc" >&2 ; exit 1
  esac

  shift

  for video in "$@"; do
      ${ffmpeg}/bin/ffmpeg -n -i "$video" -vcodec "$vcodec" -crf 23 \
               "''${video%.*}.$1.''${video##*.}"
  done
''
