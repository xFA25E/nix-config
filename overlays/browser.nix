{
  brave-incognito,
  dmenu,
  firefox,
  mpvi,
  writeShellScriptBin,
  ytdli,
}:
writeShellScriptBin "browser" ''
  declare -A browsers=(
      ["firefox"]="${firefox}/bin/firefox"
      ["brave"]="${brave-incognito}/bin/brave-incognito"
      ["ytdl"]="${ytdli}/bin/ytdli"
      ["mpv"]="${mpvi}/bin/mpvi"
  )
  choice=$(printf '%s\n' "''${!browsers[@]}" | "${dmenu}/bin/dmenu" || printf firefox)
  exec "''${browsers[$choice]}" "$@"
''
