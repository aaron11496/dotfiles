# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything

[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
#HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

txtblk="\[\033[0;30m\]" # Black - Regular
txtred="\[\033[0;31m\]" # Red
txtgrn="\[\033[0;32m\]" # Green
txtylw="\[\033[0;33m\]" # Yellow
txtblu="\[\033[0;34m\]" # Blue
txtpur="\[\033[0;35m\]" # Purple
txtcyn="\[\033[0;36m\]" # Cyan
txtwht="\[\033[0;37m\]" # White
bldblk="\[\033[1;30m\]" # Black - Bold
bldred="\[\033[1;31m\]" # Red
bldgrn="\[\033[1;32m\]" # Green
bldylw="\[\033[1;33m\]" # Yellow
bldblu="\[\033[1;34m\]" # Blue
bldpur="\[\033[1;35m\]" # Purple
bldcyn="\[\033[1;36m\]" # Cyan
bldwht="\[\033[1;37m\]" # White
undblk="\[\033[4;30m\]" # Black - Underline
undred="\[\033[4;31m\]" # Red
undgrn="\[\033[4;32m\]" # Green
undylw="\[\033[4;33m\]" # Yellow
undblu="\[\033[4;34m\]" # Blue
undpur="\[\033[4;35m\]" # Purple
undcyn="\[\033[4;36m\]" # Cyan
undwht="\[\033[4;37m\]" # White
bakblk="\[\033[40m\]"   # Black - Background
bakred="\[\033[41m\]"   # Red
bakgrn="\[\033[42m\]"   # Green
bakylw="\[\033[43m\]"   # Yellow
bakblu="\[\033[44m\]"   # Blue
bakpur="\[\033[45m\]"   # Purple
bakcyn="\[\033[46m\]"   # Cyan
bakwht="\[\033[47m\]"   # White
txtrst="\[\033[0m\]"    # Text Reset

case "$TERM" in
    xterm*|rxvt*)
        TITLEBAR="\[\033]0;\u @ \h : \w\007\]";;
    *)
        TITLEBAR="";;
esac

PS1_SHORT="$TITLEBAR\\$ "
PS1_FULL="$TITLEBAR\
$(if [ "`id -u`" = 0 ]; then echo ${bldred}; else echo ${bldgrn}; fi)\u@\
$(if [ -n "$SSH_CLIENT" ]; then echo ${bldred}; fi)\h\
${txtrst}:\
${bldblu}\w\
${bldylw}\$(__git_ps1)\
${txtrst}\n\\$ "

function prepare_prompt()
{
    PS1_NEXT="$USERNAME$SSH_CLIENT$PWD$(__git_ps1)";
    if [ "$PS1_LAST" = "$PS1_NEXT" ]; then
        PS1=$PS1_SHORT;
    elif [ "$PS1_LAST" = "?" ]; then
        PS1="\[u\033[1A\]\[u\033[2D\]$PS1_FULL"
    else
        PS1=$PS1_FULL;
    fi
    PS1_LAST=$PS1_NEXT;
}

alias ?='PS1_LAST=?'

PROMPT_COMMAND=prepare_prompt


# If this is an xterm set the title to user@host:dir
#case "$TERM" in
#    xterm*|rxvt*)
#        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
#        ;;
#    *)
#        ;;
#esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
    alias zgrep='zgrep --color=auto'
fi

alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'
alias lla='ls -lA'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

export EDITOR='vim'

export PATH=${HOME}/bin:${PATH}
export PATH=${PATH}:${HOME}/.gem/ruby/1.8/bin

if [ -x /pluto/local/activate-environment ]; then
    source /pluto/local/activate-environment
fi
