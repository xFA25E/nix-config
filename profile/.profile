export HOME="${HOME:-/home/$(whoami)}"

# some env vars
export PATH="${HOME}/.local/bin:/usr/bin:/bin:/usr/sbin:/sbin:${PATH}"
export EDITOR="emw"
export VISUAL="${EDITOR}"
export MANPATH="/usr/local/man:${MANPATH}"
export BROWSER="qutebrowser"
export TERMINAL="urxvt"

# xdg dirs
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_BIN_HOME="${HOME}/.local/bin"
export XDG_DESKTOP_DIR="${HOME}/Desktop"
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
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# Rust vars
export PATH="${HOME}/.cargo/bin:${PATH}"
RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src" && export RUST_SRC_PATH
export CARGO_TARGET_DIR="${HOME}/.cache/cargo/target"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# runel conf
export RUNEL_WM_NAME="runel_panel"
export LEMONBAR_ARGS="-n,${RUNEL_WM_NAME},-f,Source Code Pro-8"

export LESSHISFILE="/dev/null"

# mu/mu4e mail
export MAILDIR="${HOME}/.mail"

# abduco
export ABDUCO_CMD="terminal_wm"

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
export ENV="${HOME}/.shinit"

# c compiler
export CFLAGS='-Wall -Wextra -pedantic'

# java
export _JAVA_AWT_WM_NONREPARENTING=1

# composer
export PATH="${HOME}/.config/composer/vendor/bin:${PATH}"

if [ -e /home/val/.nix-profile/etc/profile.d/nix.sh ]; then
    . /home/val/.nix-profile/etc/profile.d/nix.sh
fi

# urxvt
export RXVT_SOCKET="${XDG_RUNTIME_DIR}/urxvt"

# Following automatically calls "startx" when you login:
[ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ] && exec startx -- -keeptty -nolisten tcp >"${HOME:?}/.xorg.log" 2>&1
