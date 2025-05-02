{
  format_seconds,
  video_seconds,
  writeShellScriptBin,
}:
writeShellScriptBin "filename_put_duration" ''
  set -eu

  for file in "$@"; do
      dur=$(${video_seconds}/bin/video_seconds "$file" | xargs ${format_seconds}/bin/format_seconds '%hh%mm%z%ss')
      ext=''${file##*.}
      len=$((''${#ext} + 1))
      printf -v newfile "%s - %s.%s" "''${file:0:-''${len}}" "$dur" "$ext"
      mv -v "$file" "$newfile"
  done
''
