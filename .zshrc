[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"
[[ -e ~/.bash_aliases ]] && source ~/.bash_aliases

[[ -e /usr/local/bin/virtualenvwrapper.sh ]] && source /usr/local/bin/virtualenvwrapper.sh
[[ -d "/var/lib/gems/1.8/bin" ]] && export PATH="$PATH:/var/lib/gems/1.8/bin"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

export PYTHONPATH=.

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# set editor
export EDITOR="vim"

# History settings
setopt hist_ignore_all_dups hist_expire_dups_first share_history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

setopt autopushd autocd pushd_ignore_dups pushd_silent

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


# Automatically activate Git projects' virtual environments based on the
# directory name of the project. Virtual environment name can be overridden
# by placing a .venv file in the project root with a virtualenv name in it
function workon_cwd {
    # Check that this is a Git repo
    GIT_DIR=`git rev-parse --git-dir 2> /dev/null`
    if [[ $? == 0 ]]; then
        # Find the repo root and check for virtualenv name override
        GIT_DIR=`\cd $GIT_DIR; pwd`
        PROJECT_ROOT=`dirname "$GIT_DIR"`
        ENV_NAME=`basename "$PROJECT_ROOT"`
        if [ -f "$PROJECT_ROOT/.venv" ]; then
            ENV_NAME=`cat "$PROJECT_ROOT/.venv"`
        fi
        # Activate the environment only if it is not already active
        if [ "$VIRTUAL_ENV" != "$WORKON_HOME/$ENV_NAME" ]; then
            if [ -e "$WORKON_HOME/$ENV_NAME/bin/activate" ]; then
                workon "$ENV_NAME" && export CD_VIRTUAL_ENV="$ENV_NAME"
            fi
        fi
    elif [ $CD_VIRTUAL_ENV ]; then
        # We've just left the repo, deactivate the environment
        # Note: this only happens if the virtualenv was activated automatically
        deactivate && unset CD_VIRTUAL_ENV
    fi
}
# New cd function that does the virtualenv magic
function venv_cd { cd "$@" && workon_cwd }
function pr_cd { cd "$PROJECT_ROOT" }
alias cd="venv_cd"
alias cdpr=pr_cd

autoload -U colors && colors
autoload -Uz vcs_info
setopt prompt_subst

zstyle ':vcs_info:*' stagedstr '%F{green}●'  # %c
zstyle ':vcs_info:*' unstagedstr '%F{red}●'  # %u
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:git*' formats " %F{yellow}%b"
zstyle ':vcs_info:git*' actionformats " %F{red}%b|%a"
precmd () { vcs_info }

if [ $SSH_CLIENT ]; then
    DOMAIN='%F{red}%m'
else
    DOMAIN='%m'
fi

PROMPT='%B%(!.%F{red}.%F{green})%n@$DOMAIN%F{white}%b:%B%F{blue}%~%f${vcs_info_msg_0_}%f %F{cyan}${VIRTUAL_ENV:t}%b%f
%# '
RPROMPT=''
