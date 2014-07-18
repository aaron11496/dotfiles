#!/bin/zsh

bindkey -e

autoload -U select-word-style
select-word-style bash
export EDITOR=vim

# History settings
setopt hist_ignore_space hist_ignore_all_dups hist_expire_dups_first share_history extendedglob
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

setopt autopushd autocd pushd_ignore_dups pushd_silent interactivecomments
setopt rcquotes

# Use modern completion system
autoload -Uz compinit
compinit
# zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
# zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

[[ -x /usr/bin/lesspipe ]] && eval $(SHELL=/bin/sh lesspipe)
[[ -e ~/.zsh_aliases ]] && source ~/.zsh_aliases

[[ -e /usr/local/bin/virtualenvwrapper_lazy.sh ]] && source /usr/local/bin/virtualenvwrapper_lazy.sh

export VIRTUAL_ENV_DISABLE_PROMPT=1

export WORKON_HOME="$HOME/.virtualenvs"

source ~/config/autoworkon.sh

chpwd() { auto_workon }

autoload -U colors && colors
autoload -Uz vcs_info
setopt prompt_subst

zstyle ':vcs_info:*' stagedstr '%F{green}●'  # %c
zstyle ':vcs_info:*' unstagedstr '%F{red}●'  # %u
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' formats " %F{yellow}%b"
zstyle ':vcs_info:git*' actionformats " %F{red}%b|%a"

precmd() { vcs_info }


if [ $SSH_CLIENT ]; then
    DOMAIN='%F{red}%m'
else
    DOMAIN='%m'
fi

PROMPT='%B%(!.%F{red}.%F{green})%n@$DOMAIN%F{white}%b:%B%F{blue}%~%f${vcs_info_msg_0_}%f %F{cyan}${VIRTUAL_ENV:t}%b%f
%# '
RPROMPT=''
