#!/bin/sh

if [ ! -d qcsrc ]; then
    echo "failed to find qcsrc directory in $(pwd), please run this script"
    echo "from nexuiz data directory"
    exit 1
else
    # ensure this is actually a xonotic repo
    pushd qcsrc > /dev/null
    if [ ! -d client -o ! -d common -o ! -d menu -o ! -d server -o ! -d warpzonelib ]; then
        echo "this doesnt look like a nexuiz source tree, aborting"
        popd > /dev/null
        exit 1
    fi
fi

echo -n "removing redundant files ..."
rm -f nexuiz.ncb
rm -f nexuiz.sln
rm -f nexuiz.suo
rm -f nexuiz.vcproj
rm -f nexuiz.vcproj.user
echo "complete"

echo -n "creating projects ..."
echo "client" >  dirs
echo "server" >> dirs
echo "menu"   >> dirs

echo "complete"

echo -n "creating zip archive ..."
zip -r -9 ../nexuiz.zip * > /dev/null
echo "complete"

popd > /dev/null
echo "finished!"
