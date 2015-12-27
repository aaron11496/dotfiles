#!/bin/bash

TILDE="$(readlink -f $(dirname $0)/tilde)"
TARGET_FILES=$(find $TILDE -type f -exec readlink -f '{}' \;)

pushd ~ >/dev/null

for file in $TARGET_FILES
do
    newlink=${file:${#TILDE}+1}
    newlinkdir=$(dirname $newlink)

    [ ! -d "$newlinkdir" ] && mkdir -p -v "$newlinkdir"

    ln -s -v "$file" "$newlink"

done

popd >/dev/null

echo "All done."
