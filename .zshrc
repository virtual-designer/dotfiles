# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

zstyle :compinstall filename '/home/rakinar2/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

red=$'%F{red}'
blue=$'%F{blue}'
dimed=$'%{\e[2m%}'
bold=$'%{\e[1m%}'
cyan=$'%F{cyan}'
gray=$'%F{7}'
reset=$'%{\e[0m%}%f'

if [[ ! -t 1 ]]; then
    red=''
    blue=''
    dimed=''
    bold=''
    cyan=''
    gray=''
    reset=''
fi

autoload -Uz vcs_info
zstyle ':vcs_info:git:*' formats "${bold}${blue}git:(${red}%b${blue})${reset}${reset} "

function precmd {
    vcs_info
}

function arrow_color {    
    if [[ $? -eq 0 ]]; then
	printf "%s" "${cyan}"
    else
	printf "%s" "${red}"
    fi
}

setopt prompt_subst
PROMPT='${bold}$(arrow_color)➜${reset}${reset} ${bold}${blue}%n@%m${reset}${reset} ${dimed}${gray}%~${reset}${reset} ${vcs_info_msg_0_}${reset}${reset}${bold}%#${reset} '

# go
export PATH="$PATH:/usr/local/go/bin"

# fnm
FNM_PATH="/home/rakinar2/.local/share/fnm"
if [ -d "$FNM_PATH" ]; then
  export PATH="$FNM_PATH:$PATH"
  eval "`fnm env`"
fi

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# code
if [ -z "$(command -v code)" ]; then
  alias code="code-insiders"
fi

# pnpm
export PNPM_HOME="/home/rakinar2/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

alias et="emacsclient -t"
alias ec="emacsclient -c"

