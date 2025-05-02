{
  ffmpeg,
  writeShellScriptBin,
}:
writeShellScriptBin "resize_video" ''
  set -eu

  width=$1
  case "$width" in
      144) height=256 ;;
      240) height=426 ;;
      360) height=640 ;;
      480) height=854 ;;
      720) height=1280 ;;
      *) echo "Select height: 144, 240, 360, 480, 720" >&2 ; exit 1
  esac

  shift

  for video in "$@"; do
      ${ffmpeg}/bin/ffmpeg -i "$video" -vf scale="$height:$width" -acodec copy \
             "''${video%.*}.h$width.''${video##*.}"
  done
''
