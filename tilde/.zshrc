#!/bin/zsh

bindkey -e  # emacs-style keybindings

export EDITOR='vim'
export BROWSER='google-chrome'

setopt auto_cd auto_pushd cdable_vars pushd_ignore_dups pushd_silent  # cd
setopt always_to_end list_types  # completion

setopt extendedglob hist_expire_dups_first hist_ignore_all_dups hist_ignore_space hist_verify share_history  # history
HISTSIZE=2000
HISTFILE=~/.zsh_history

setopt check_jobs interactive_comments rcquotes transient_rprompt


# autoload -U select-word-style
# select-word-style bash

# Use modern completion system
autoload -Uz compinit
compinit
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
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

if [[ -e ~/config/autoworkon.sh ]]; then
    source ~/config/autoworkon.sh
    chpwd() { auto_workon }
fi

autoload -U colors && colors
autoload -Uz vcs_info
setopt prompt_subst

function +vi-git-stash() {
    local -a stashes

    if [[ -s ${hook_com[base]}/.git/refs/stash ]] ; then
        stashes=$(git stash list 2>/dev/null | wc -l)
        hook_com[misc]+=" (${stashes} stashed)"
    fi
}

zstyle ':vcs_info:git*+set-message:*' hooks git-stash
zstyle ':vcs_info:*' stagedstr '%F{green}●'  # %c
zstyle ':vcs_info:*' unstagedstr '%F{red}●'  # %u
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:git*' formats " %F{yellow}%b%F{magenta}%m"
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
###-begin-npm-completion-###
#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm
#

COMP_WORDBREAKS=${COMP_WORDBREAKS/=/}
COMP_WORDBREAKS=${COMP_WORDBREAKS/@/}
export COMP_WORDBREAKS

if type complete &>/dev/null; then
  _npm_completion () {
    local si="$IFS"
    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$COMP_CWORD" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           npm completion -- "${COMP_WORDS[@]}" \
                           2>/dev/null)) || return $?
    IFS="$si"
  }
  complete -F _npm_completion npm
elif type compdef &>/dev/null; then
  _npm_completion() {
    si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 npm completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef _npm_completion npm
elif type compctl &>/dev/null; then
  _npm_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       npm completion -- "${words[@]}" \
                       2>/dev/null)) || return $?
    IFS="$si"
  }
  compctl -K _npm_completion npm
fi
###-end-npm-completion-###


autoload bashcompinit
bashcompinit
