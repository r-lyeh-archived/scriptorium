#!/bin/bash
# By Ingwie Phoenix
#
# Use this on OS X if you have issues with libmysqlclient.18.dylib
# Run this inside your binary tree!

change_name() {
    install_name_tool -change \
        libmysqlclient.18.dylib \
        /usr/local/mysql/lib/libmysqlclient.18.dylib \
        $1
}

find . -name "*.dylib" -o -name "os" -o -name "os-fcgi" | while read fh; do
    echo "-- Updating: $fh"
    change_name $fh
done
