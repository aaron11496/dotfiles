#!/bin/zsh

bindkey -e  # emacs-style keybindings

export EDITOR='vim'
export BROWSER='google-chrome'
export MINICOM='-c on'

setopt auto_cd auto_pushd cdable_vars pushd_ignore_dups pushd_silent auto_param_slash  # cd
setopt always_to_end list_types  # completion
setopt hist_expire_dups_first hist_ignore_all_dups hist_ignore_space hist_verify share_history  # history
HISTSIZE=2000
SAVEHIST=10000
HISTFILE=~/.zsh_history

setopt extendedglob check_jobs interactive_comments rcquotes transient_rprompt  # etc

# Use modern completion system
fpath+=~/.zfunc
autoload bashcompinit
bashcompinit
autoload -Uz compinit
eval "$(dircolors -b)"
compinit
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' menu select=2
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
# zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

[[ -x /usr/bin/lesspipe ]] && eval $(SHELL=/bin/sh lesspipe)
[[ -e ~/.aliases ]] && source ~/.aliases

[[ -e /usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh ]] && source /usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh
export VIRTUAL_ENV_DISABLE_PROMPT=1
export WORKON_HOME="$HOME/.virtualenvs"
complete -F 'lsvirtualenv -b' workon

autoload -U colors && colors
setopt prompt_subst

# source ~/zsh-git-prompt/zshrc.sh
# GIT_PROMPT_EXECUTABLE="haskell"

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' formats " %B%F{magenta}%b%%b%f%m%u%c"
zstyle ':vcs_info:git:*' actionformats " %F{red}%b|%a%%b%f%m%u%c"
zstyle ':vcs_info:git:*' stagedstr "%B%F{green}+%f%b"
zstyle ':vcs_info:git:*' unstagedstr "%B%F{red}?%f%b"

precmd() { vcs_info }

DOMAIN=
[ $SSH_CLIENT ] && DOMAIN='%B%(!.%F{red}.%F{green})%n@%F{red}%m%F{white}%b:'

PROMPT='$DOMAIN%B%F{blue}%~%b%f${vcs_info_msg_0_}%B%F{cyan}${VIRTUAL_ENV+ ${VIRTUAL_ENV:t}}%b%f
%# '
RPROMPT=

man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

# The next line updates PATH for the Google Cloud SDK.
if [ -f ~/google-cloud-sdk/path.zsh.inc ]; then . ~/google-cloud-sdk/path.zsh.inc; fi

# The next line enables shell command completion for gcloud.
if [ -f ~/google-cloud-sdk/completion.zsh.inc ]; then . ~/google-cloud-sdk/completion.zsh.inc; fi

PATH="$PATH:$HOME/.local/bin"
