#!/bin/bash

NEWTILDE="$(dirname $0)/tilde"
TARGET_FILES=$(find $NEWTILDE -type f)

pushd ~/

for file in $TARGET_FILES; do
    newfile=${file:$((${#NEWTILDE}+1))}
    newdir=$(dirname $newfile)
    if [ ! -d $newdir ]; then
        mkdir -p -v $(dirname $newfile)
    fi
    ln -s -v $file $newfile
done

popd
