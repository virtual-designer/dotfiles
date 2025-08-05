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
dim=$'%{\e[2m%}'
reset=$'%{\e[0m%}'
bold=$'%{\e[1m%}'
cyan=$'%F{cyan}'
gray=$'%F{7}'

if [[ ! -t 1 ]]; then
    red=''
    blue=''
    dim=''
    reset=''
    bold=''
    cyan=''
    gray=''
fi

autoload -Uz vcs_info

function precmd {
    vcs_info
}

function arrow_color {    
    if [[ $? -eq 0 ]]; then
	echo "${cyan}"
    else
	echo "${red}"
    fi
}

zstyle ':vcs_info:git:*' formats "${bold}${blue}git:(${red}%b${blue})${reset}${reset} "
setopt prompt_subst

PROMPT='${bold}$(arrow_color)➜${reset}${reset} ${bold}${blue}%n@%m${reset}${reset} ${dim}${gray}%~${reset}${reset} ${vcs_info_msg_0_}${reset}${bold}%#${reset} '

