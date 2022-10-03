{
  dmenu,
  gawk,
  jq,
  util-linux,
  writeShellScriptBin,
  yt-dlp,
}:
writeShellScriptBin "select_ytdl_fmt" ''
  set -eu
  columns='format_id,width,height,ext,filesize,vcodec,acodec'
  jq_format=".formats[] | (\"\(.$(echo "$columns" | sed 's/,/)|\\(./g'))\")"
  ${yt-dlp}/bin/yt-dlp --no-color --dump-json "$1" \
      | ${jq}/bin/jq --raw-output "$jq_format" \
      | ${util-linux}/bin/column --table --separator '|' \
      | ${dmenu}/bin/dmenu -p "Select format" -l 19 \
      | ${gawk}/bin/awk '{print $1}'
''
