{
  ffmpeg,
  jq,
  writeShellScriptBin,
}:
writeShellScriptBin "video_seconds" ''
  set -eu

  ${ffmpeg}/bin/ffprobe -v quiet -of json -show_entries format=duration "$1" \
      | ${jq}/bin/jq '.format.duration | tonumber | round'
''
