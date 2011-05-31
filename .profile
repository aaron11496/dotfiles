# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# personal bin
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# for xmonad
if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$PATH:$HOME/.cabal/bin"
fi

# for emacs
if [ -d "$HOME/.emacs.d/bin" ]; then
    PATH="$PATH:$HOME/.emacs.d/bin"
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi
