{
  runCommand,
  src,
}:
runCommand "mpv-youtube-quality" {inherit src;} ''
  mkdir -p $out
  cp $src/youtube-quality.{conf,lua} $out
''
