alias p='popd'

alias ll='ls -l --group-directories-first'
alias la='ls -A --group-directories-first'
alias l='ls -CF --group-directories-first'
alias lla='ls -lA --group-directories-first'

# colorful ls/dir/grep
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

alias ack='ack-grep'
alias df='df -T'
alias go='gnome-open'
alias t='py.test --tb short'
alias ymd='date +%Y%m%d'

alias k9='sudo killall -9'
alias k15='sudo killall -15'

alias wi='whois -H'

alias ga='git add'
alias gb='git branch'
alias gc='git checkout'
alias gcl='git clone'
alias gd='git diff'
alias gdc='git diff --cached'
alias gds='git diff --stat'
alias gf='git fetch'
alias gg='git grep'
alias ggi='git grep -i'
alias gl='git log'
alias gls='git log --stat'
alias glp='git log -p --stat'
alias gm='git commit -m'
alias gma='git commit -am'
alias gp='git push'
alias gpu='git pull'
alias gra='git remote add'
alias grr='git remote rm'
alias gs='git status -s'
alias gt='git tag'
