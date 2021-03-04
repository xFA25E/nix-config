#!/bin/sh
export PATH="${HOME}/.local/bin:/usr/bin:/bin:/usr/sbin:/sbin:${PATH}"
export EDITOR="emacseditor"
export VISUAL="${EDITOR}"
export MANPATH="/usr/local/man:${MANPATH}"
export BROWSER="qutebrowser"
export TERMINAL="uxterm"

# xdg dirs
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_BIN_HOME="${HOME}/.local/bin"
# export XDG_DESKTOP_DIR="${HOME}/Desktop"
export XDG_DOWNLOAD_DIR="${HOME}/Downloads"
export XDG_DOCUMENTS_DIR="${HOME}/Documents"
export XDG_MUSIC_DIR="${HOME}/Music"
export XDG_PICTURES_DIR="${HOME}/Pictures"
export XDG_VIDEOS_DIR="${HOME}/Videos"
export XDG_CONFIG_DIRS="/usr/etc/xdg:/etc/xdg:${XDG_CONFIG_DIRS}"
XDG_RUNTIME_DIR="/run/user/$(id -u)"
if [ ! -d "${XDG_RUNTIME_DIR}" ]; then
    XDG_RUNTIME_DIR="/tmp/run$(id -u)"
    mkdir --mode=700 "${XDG_RUNTIME_DIR}"
fi
export XDG_RUNTIME_DIR

# bspwm
export BSPWM_SOCKET="${XDG_RUNTIME_DIR}/bspwm.socket"

# Some other configuration
export GTK_IM_MODULE=ibus
export XMODIFIERS=ibus
export QT_IM_MODULE=ibus
## Ensure that GTK themes are applied uniformly in the Desktop Environment
export GTK2_RC_FILES="${XDG_CONFIG_HOME}/gtk-2.0/gtkrc"

# Rust
export CARGO_HOME="${XDG_CACHE_HOME}/cargo"
export RUSTUP_HOME="${XDG_CACHE_HOME}/rustup"
# RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src" && export RUST_SRC_PATH
export PATH="${CARGO_HOME}/bin:${PATH}"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# less
export LESSHISFILE="/dev/null"

# mu/mu4e mail
export MU_HOME="${XDG_CACHE_HOME}/mu"
export MAILDIR="${XDG_DATA_HOME}/mail"

# abduco
export ABDUCO_CMD="terminal_wm"
export ABDUCO_SOCKET_DIR="${XDG_RUNTIME_DIR}/abduco"

# rimer
export RIMER_CALLBACK='rimer_callback'

# mpd
export MPD_HOST="localhost"
export MPD_PORT="6600"

# sudo
export SUDO_ASKPASS="${XDG_BIN_HOME}/sudo_askpass"

# ssh
export SSH_ASKPASS="ssh-askpass"

# env for posix shell initialization
export ENV="${HOME}/.shinit"

# java
export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_HOME=/usr/lib/jvm/default

# composer
export PATH="${HOME}/.config/composer/vendor/bin:${PATH}"

# cuda
export CUDA_CACHE_PATH="${XDG_CACHE_HOME}/nv"

# npm
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/npm/npmrc"

# gnupg
export GNUPGHOME="${XDG_DATA_HOME}/gnupg"

# pass
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"

# xauthority
export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"

# stardict
export SDCV_HISTSIZE=0

# history
export HISTFILESIZE=0
export HISTSIZE=0
export HISTFILE=

# zoom
export SSB_HOME="${XDG_CACHE_HOME}/zoom"

# sqlite
export SQLITE_HISTORY=/dev/null

# MySQL
export MYSQL_HISTFILE=/dev/null

# vagrant
export VAGRANT_HOME="${XDG_DATA_HOME}/vagrant"
export VAGRANT_ALIAS_FILE="${XDG_DATA_HOME}/vagrant/aliases"

# gem
export GEM_HOME="${XDG_DATA_HOME}/gem"
export GEM_SPEC_CACHE="${XDG_CACHE_HOME}/gem"

# adb
export ANDROID_SDK_HOME="${XDG_CONFIG_HOME}/android"
export ADB_VENDOR_KEY="${XDG_CONFIG_HOME}/android"

# boot
export BOOT_HOME="${XDG_CACHE_HOME}/boot"

# pager
export PAGER=cat

# nix
for sh in nix.sh my-profile.sh hm-session-vars.sh; do
    if [ -e "${HOME}/.nix-profile/etc/profile.d/${sh}" ]; then
        . "${HOME}/.nix-profile/etc/profile.d/${sh}"
    fi
done

# youtube-dl
export YTDL_DIR="${XDG_VIDEOS_DIR:-${HOME}/Videos}/youtube"

# Following automatically calls "startx" when you login:
if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
    exec startx "${HOME}/.xinitrc" -- -keeptty -nolisten tcp >"${XDG_RUNTIME_DIR:?}/xorg.log" 2>&1
fi
