{
  git,
  gnupg,
  writeShellScriptBin,
}:
writeShellScriptBin "install_keys" ''
  set -eu

  usb=$1
  tempdir=$(mktemp --directory /tmp/secrets.XXXX)
  passdir=''${PASSWORD_STORE_DIR:-$HOME/.password-store}

  echo "Mounting.."
  sudo mount -t vfat "$usb" "$tempdir" -o rw,umask=0000

  echo "Installing authinfo.."
  cp "$tempdir"/keys/authinfo.gpg "$HOME"/.authinfo.gpg
  chmod 600 "$HOME"/.authinfo.gpg

  echo "Installing ssh.."
  mkdir -p "$HOME"/.ssh
  chmod 700 "$HOME"/.ssh
  cp "$tempdir"/keys/ssh/* "$HOME"/.ssh

  echo "Installing gpg.."
  if [ -z "''${DISPLAY:-}" ]; then
      ${gnupg}/bin/gpg --pinentry-mode=loopback --import "$tempdir"/keys/gpg/secret.key
      ${gnupg}/bin/gpg --pinentry-mode=loopback --import "$tempdir"/keys/gpg/public.key
  else
      ${gnupg}/bin/gpg --import "$tempdir"/keys/gpg/secret.key
      ${gnupg}/bin/gpg --import "$tempdir"/keys/gpg/public.key
  fi
  chmod 700 "$HOME"/.gnupg

  echo "Cloning pass.."
  ${git}/bin/git clone "$tempdir"/keys/password-store "$passdir"
  chmod 700 "$passdir"

  echo "Unmounting.."
  sudo umount "$tempdir"
  rmdir "$tempdir"

  echo ">>> You should trust your key"
  echo "$ gpg --edit-key 'your@mail'"
  echo "$ trust"
''
