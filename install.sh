#!/bin/bash

FILENAMES=(
    '.Xdefaults'
    '.Xmodmap'
    '.bash_aliases'
    '.bashrc'
    '.csshrc'
    '.emacs'
    '.emacs.d'
    '.gitconfig'
    '.gitignore'
    '.profile'
    '.vimrc'
    '.xinitrc'
    '.xmonad'
    'ipy_user_conf.py'
);

for filename in ${FILENAMES[@]};
do
    ln -s -v -b --suffix=".bak" config/$filename ~/;
done;
