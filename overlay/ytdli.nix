{
  dmenu,
  jq,
  lib,
  pueue,
  select_ytdl_fmt,
  writeShellScriptBin,
  ytdl,
  ytdla,
  ytdlam,
  ytdlpam,
}: let
  inherit (lib.strings) makeBinPath;
in
  writeShellScriptBin "ytdli" ''
    set -eu
    export PATH="${makeBinPath [dmenu jq pueue ytdl ytdla ytdlam ytdlpam]}:$PATH"

    if test "false" = "$(pueue status --json | jq '.groups | has("ytdl")')"; then
        pueue group add --parallel 3 ytdl
    fi

    case "$(printf 'video\naudio\nmusic\nmusicp\nselectv' | dmenu -p "Download")" in
      video) pueue add -eg ytdl -- ytdl "$1" ;;
      audio) pueue add -eg ytdl -- ytdla "$1" ;;
      music) pueue add -eig ytdl -- ytdlam "$1" ;;
      musicp) pueue add -eig ytdl -- ytdlpam "$1" ;;
      selectv)
        fmt=$(${select_ytdl_fmt}/bin/select_ytdl_fmt "$1")
        fmt="$fmt+bestaudio/$fmt/best"
        pueue add -eg ytdl -- ytdl -f "$fmt" "$1"
        ;;
      *) exit 1
    esac
  ''
