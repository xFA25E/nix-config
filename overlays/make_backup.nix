{
  rsync,
  writeShellScriptBin,
}:
writeShellScriptBin "make_backup" ''
  set -eu

  test -d "$1"

  ${rsync}/bin/rsync --delete --delete-excluded --archive --verbose --partial \
          --progress --relative --xattrs --hard-links --backup \
          --backup-dir="''${1%/}/backup" --suffix="$(date '+.~%Y%m%d~')" \
          --exclude='lost+found' --exclude="$1" --filter=':- .gitignore' \
          ~/Documents ~/Downloads ~/Music ~/Pictures ~/Videos ~/archives ~/org \
          ~/roam ~/phone \
          ~/.authinfo.gpg ~/.bash_history ~/.gnupg ~/.ssh \
          ~/.local/share/emacs ~/.local/share/mail ~/.local/share/password-store \
          ~/.config/transmission-daemon \
          "''${1%/}/current"
''
