#!/bin/sh

if [ ! -d qcsrc ]; then
    echo "failed to find qcsrc directory in $(pwd), please run this script"
    echo "from xonotic-data.pk3dir"
    exit 1
else
    # ensure this is actually a xonotic repo
    pushd qcsrc > /dev/null
    if [ ! -d client -o ! -d common -o ! -d dpdefs -o ! -d menu -o ! -d server -o ! -d warpzonelib ]; then
        echo "this doesnt look like a xonotic source tree, aborting"
        popd > /dev/null
        exit 1
    fi
fi

# force reset and update
git rev-parse
if [ $? -ne 0 ]; then
    echo "not a git directory, continuing without rebase"
else
    echo -n "resetting git state and updating ... "
    git reset --hard HEAD > /dev/null 2>&1
    git pull > /dev/null 2>&1
    echo "complete"
fi

echo -n "generate precache for csqc ..."
./collect-precache.sh > /dev/null 2>&1
echo "complete"

echo -n "removing redundant files ..."
rm -f Makefile
rm -f autocvarize-update.sh
rm -f autocvarize.pl
rm -f collect-precache.sh
rm -f fteqcc-bugs.qc
rm -f i18n-badwords.txt
rm -f i18n-guide.txt
echo "complete"

echo -n "creating projects ..."
echo "client" >  dirs
echo "server" >> dirs
echo "menu"   >> dirs

echo "complete"

echo -n "creating zip archive ..."
zip -r -9 ../xonotic.zip * > /dev/null
echo "complete"

popd > /dev/null
echo "finished!"
