{
  video_seconds,
  writeShellScriptBin,
}:
writeShellScriptBin "sort_videos_by_duration" ''
  set -eu

  if [ "$1" = "-0" ]; then
      shift
      zout=1
  fi

  for video in "$@"; do
      duration=$(${video_seconds}/bin/video_seconds "$video")
      printf '%d %s\0' "$duration" "$video"
  done \
      | sort -z -n \
      | cut -z -d " " -f 2- \
      | if [ -z "$zout" ]; then tr -t '\0' '\n'; else tee; fi
''
