#!/bin/bash

pushd ~

FILENAMES=(
    '.bash_aliases'
    '.bashrc'
    '.csshrc'
    '.emacs'
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
    ln -s -v -b --suffix=".bak" ~/config/$filename ~/;
done;

ln -s -v -b --suffix=".bak" ~/config/ipython_config.py ~/.ipython/profile_default

for fn in config/venvwrapperhooks/*; do
    ln -s -v -b --suffix=".bak" ~/$fn ~/.virtualenvs/ ;
done;

if [[ ! -d ~/.emacs.d ]]; then
    mkdirs ~/.emacs.d
    for fn in .emacs.d/*; do
        ln -s -v -b --suffix=".bak" ~/.emacs.d/ ;
    done
fi
if [[ ! -d ~/.virtualenvs ]];
    mkdir ~/.virtualenvs ;
    for fn in config/venvwrapperhooks/*; do
        ln -s -v -b --suffix=".bak" ~/$fn ~/.virtualenvs/ ;
    done;

if [[ ! -d ~/.emacs.d ]]; then
    mkdir ~/.emacs.d ;
    for fn in ~/config/.emacs.d/*; do
        ln -s -v -b --suffix=".bak" $fn ~/.emacs.d/  ;
    done
fi

popd
