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
);

DIRS=(
    '.xmonad'
    '.emacs.d'
    '.virtaulenvs'
    '.ipython/profile_default'
);

for filename in ${FILENAMES[@]};
do
    ln -s -v -b --suffix=".bak" ~/config/$filename ~/;
done

for dir in ${DIRS[@]}; do
    if [[ ! -d ~/$dir ]]; then
        mkdir -p ~/$dir;
    fi
    for fn in ~/config/$dir/*; do
        ln -s -v -b --suffix=".bak" $fn ~/$dir/ ;
    done
done

popd
