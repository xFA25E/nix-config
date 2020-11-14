export HOME="${HOME:-/home/$(whoami)}"

# some env vars
export PATH="${HOME}/.local/bin:/usr/bin:/bin:/usr/sbin:/sbin:${PATH}"
export EDITOR="emw"
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
if [ ! -d "${XDG_RUNTIME_DIR}" ]
then
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
export RUSTUP_HOME="${XDG_CACHE_HOME}/rustup"
export CARGO_HOME="${XDG_CACHE_HOME}/cargo"
export PATH="${CARGO_HOME}/bin:${PATH}"
RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src" && export RUST_SRC_PATH
# export CARGO_TARGET_DIR="${CARGO_HOME}/target"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# runel
export RUNEL_WM_NAME="runel_panel"
export LEMONBAR_ARGS="-n,${RUNEL_WM_NAME},-f,Iosevka-12"

# less
export LESSHISFILE="/dev/null"

# mu/mu4e mail
export MU_HOME="${XDG_CACHE_HOME}/mu"
export MAILDIR="${XDG_DATA_HOME}/mail"

# abduco
export ABDUCO_CMD="terminal_wm"
export ABDUCO_SOCKET_DIR="${XDG_RUNTIME_DIR}/abduco"

# rimer
export TIMER_UPDATER='panel_timer'
export TIMER_PROFILES='countdown study 00:25
countdown pause 00:05
countdown other 00:25
countdown work 00:25
countdown eyes 00:30
stopwatch study
stopwatch work'

# mpd
export MPD_HOST="localhost"
export MPD_PORT="6600"

# sudo
export SUDO_ASKPASS="${XDG_BIN_HOME}/sudo_askpass"

# ssh
export SSH_ASKPASS="ssh-askpass"

# env for posix shell initialization
export ENV="${XDG_CONFIG_HOME}/sh/init"

# c compiler
# export CFLAGS='-Wall -Wextra -Werror -pedantic'

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

# readline
export INPUTRC="${XDG_CONFIG_HOME}/readline/inputrc"

# xinit
export XINITRC="${XDG_CONFIG_HOME}/X11/xinitrc"

# xauthority
export XAUTHORITY="${XDG_RUNTIME_DIR}/Xauthority"

# stardict
export STARDICT_DATA_DIR="${XDG_DATA_HOME}"
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

# lein
export LEIN_HOME="${XDG_CACHE_HOME}/lein"

# gem
export GEM_HOME="${XDG_DATA_HOME}/gem"
export GEM_SPEC_CACHE="${XDG_CACHE_HOME}/gem"

# adb
export ANDROID_SDK_HOME="${XDG_CONFIG_HOME}/android"
export ADB_VENDOR_KEY="${XDG_CONFIG_HOME}/android"

# boot
export BOOT_HOME="${XDG_CACHE_HOME}/boot"

# Following automatically calls "startx" when you login:
[ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ] && \
    exec startx "${XINITRC}" -- -keeptty -nolisten tcp >"${XDG_RUNTIME_DIR:?}/xorg.log" 2>&1
