#!/bin/zsh
# 'auto_workon' is meant to be used as a zsh 'chpwd' command.
# It automatically activate Git projects' virtual environments based on the
# directory name. Virtual environment name can be overridden by placing a
# filed called .venv containing the desired virtualenv name.

# If you are using virtualenvwrapper_lazy then you must set WORKON_HOME
# in your .zshrc for this to work.

# With the current version of virtualenvwrapper, you must manually edit
# 'virtualenvwrapper_cd' in virtualenvwrapper.sh to use 'cd -q' when in zsh.

# To use, put something like the following in your .zshrc:
# export WORKON_HOME="$HOME/.virtualenvs"
# source ~/config/autoworkon.sh
# chpwd() { auto_workon }


git_venv_name () {
    GIT_DIR=`git rev-parse --git-dir 2> /dev/null`
    [[ $? != 0 ]] && return

    GIT_DIR=`readlink -e $GIT_DIR`
    PROJECT_ROOT=`dirname "$GIT_DIR"`

    if [ -f "$PROJECT_ROOT/.venv" ]; then
        ENV_NAME=`cat "$PROJECT_ROOT/.venv"`
    else
        ENV_NAME=`basename "$PROJECT_ROOT"`
    fi

    [ -e "$WORKON_HOME/$ENV_NAME/bin/activate" ] && echo $ENV_NAME
}

auto_workon () {
    ENV_NAME=`git_venv_name`
    if [ -z "$ENV_NAME" ]; then
        if [ "$CD_VIRTUAL_ENV" -a "$VIRTUAL_ENV" ]; then
            deactivate
            unset CD_VIRTUAL_ENV
        fi
    elif [ "$VIRTUAL_ENV" != "$WORKON_HOME/$ENV_NAME" ]; then
        workon "$ENV_NAME"
        export CD_VIRTUAL_ENV="$ENV_NAME"
    fi
}
