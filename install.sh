#!/bin/bash

FILENAMES=(
    '.bash_aliases'
    '.bashrc'
    '.csshrc'
    '.emacs'
    '.emacs.d'
    '.gitconfig'
    '.gitignore'
    '.profile'
    '.vimrc'
    '.Xdefaults'
    '.xinitrc'
    '.Xmodmap'
    '.xmonad'
);

for filename in ${FILENAMES[@]};
do
    ln -s -v -b --suffix=".bak" config/$filename ~/;
done;

ln -s -v -b --suffix=".bak" config/ipython_config.py ~/.ipython/profile_default
