# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
    for rc in ~/.bashrc.d/*; do
        if [ -f "$rc" ]; then
            . "$rc"
        fi
    done
fi
unset rc

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

branch=""

git_branch_parse() {
  branch="$(git symbolic-ref --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)"

  if [ $? -ne 0 ]; then
    branch=""
  fi
}

git_branch() {
  echo -n "$branch"
}

git_status_start() {
  if [ -n "$branch" ]; then
    echo " git:("
  fi
}

git_status_end() {
  if [ -n "$branch" ]; then
    echo ")"
  fi
}

arrow_color() {
  if [ $? -eq 0 ]; then
    echo -e "\e[1;36m"
  else
    echo -e "\e[1;31m"
  fi
}

if [ -n "$PROMPT_COMMAND" ]; then
  PROMPT_COMMAND="$PROMPT_COMMAND; git_branch_parse"
else
  PROMPT_COMMAND="git_branch_parse;"
fi

PS1='\[$(arrow_color)\]➜\[\e[0m\] \[\e[1;34m\]\u@\H\[\e[0m\e[2;37m\] \[\e[0m\e[2;37m\]\w\[\e[0m\e[1m\e[34m\]$(git_status_start)\[\e[1;31m\]$(git_branch)\[\e[1;34m\]$(git_status_end) \[\e[0m\e[1m\]$\[\e[0m\] '


# pnpm
export PNPM_HOME="/home/rakinar2/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

