{
  notifiers,
  writeShellScriptBin,
}:
writeShellScriptBin "interval_timer" ''
  ${notifiers}/bin/notify_bubble
  while test $# -ne 0; do
      dur=$1
      rep=$2
      shift
      shift
      for rep in $(seq $rep); do
          sleep $dur
          ${notifiers}/bin/notify_ding
      done
      ${notifiers}/bin/notify_bubble
  done
''
