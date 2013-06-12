#!/bin/bash

pushd ~

FILES=(
    '.aliases'
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

popd
