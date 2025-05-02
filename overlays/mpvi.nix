{
  dmenu,
  mpv,
  select_ytdl_fmt,
  writeShellScriptBin,
}:
writeShellScriptBin "mpvi" ''
  set -eu

  case "$(printf 'select\naudio' | ${dmenu}/bin/dmenu -p "mpvi")" in
    select)
      fmt=$(${select_ytdl_fmt}/bin/select_ytdl_fmt "$1")
      fmt="$fmt+bestaudio/$fmt/best"
      ;;
    audio) fmt='bestaudio/best' ;;
    *) exit 1
  esac

  exec ${mpv}/bin/mpv "$1" \
       --pause \
       --profile=gui \
       --load-unsafe-playlists \
       --ytdl-raw-options=yes-playlist= \
       --ytdl-format="$fmt" \
       --loop-playlist=inf
''
