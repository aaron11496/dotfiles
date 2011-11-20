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
    'ipy_user_conf.py'
);

for filename in ${FILENAMES[@]};
do
    ln -s -v -b --suffix=".bak" config/$filename ~/;
done;
