#!/bin/bash

# TODO: this sucks, let's try something else

pushd ~

FILES=(
    '.aliases'
    '.bash_aliases'
    '.bashrc'
    '.csshrc'
    '.emacs'
    '.gitconfig'
    '.profile'
    '.vimrc'
    '.xbindkeysrc'
    '.Xresources'
    '.xinitrc'
    '.Xmodmap'
    '.zsh_aliases'
    '.zshrc'
);

DIRECTORIES=(
    '.xmonad'
    '.emacs.d'
    '.virtualenvs'
    '.ipython/profile_default'
);

for fn in ${FILES[@]}; do
    ln -s -v ~/config/$fn ~/;
done

for dir in ${DIRECTORIES[@]}; do
    [[ ! -d ~/$dir ]] && mkdir -p ~/$dir;
    for fn in ~/config/$dir/*; do
        ln -s -v $fn ~/$dir/;
    done
done

ln -s -v ~/config/gitignore ~/.gitignore;

popd
