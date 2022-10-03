{
  dmenu,
  findutils,
  writeShellScriptBin,
}:
writeShellScriptBin "select_music_dir" ''
  set -eu
  MUSIC_DIRECTORY="''${XDG_MUSIC_DIR:-''${HOME}/Music}"
  test -d "$MUSIC_DIRECTORY"

  ${findutils}/bin/find "$MUSIC_DIRECTORY" -type d ! -wholename "$MUSIC_DIRECTORY" \
      | cut -c "$((''${#MUSIC_DIRECTORY} + 2))-" \
      | sort \
      | ${dmenu}/bin/dmenu -p "Select directory" -l 19 \
      | xargs -I'{}' printf '%s/%s' "$MUSIC_DIRECTORY" '{}'
''
