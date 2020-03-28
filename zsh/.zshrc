[ -n "${ENV}" ] && source "${ENV}"

set -o dvorak
fpath+=~/.zfunc

autoload -Uz compinit
compinit

PROMPT='%F{red}%?%f %F{blue}%~%f %F{white}%(!.#.$)%f '

HISTFILE="$HOME/.zsh_history"
HISTSIZE=99999
SAVEHIST=$HISTSIZE

# Load plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
