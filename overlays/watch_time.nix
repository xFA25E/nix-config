{
  format_seconds,
  gawk,
  video_seconds,
  writeShellScriptBin,
}:
writeShellScriptBin "watch_time" ''
  set -eu

  speed=''${1:?provide speed}

  shift

  sum=0

  for f in "$@"; do
      sum=$((sum + $(${video_seconds}/bin/video_seconds "$f")))
  done

  adjusted=$(${gawk}/bin/awk -v s="$sum" -v v="$speed" 'BEGIN { print s / v }')

  ${format_seconds}/bin/format_seconds "%02h:%02m:%02s" "$adjusted"
''
