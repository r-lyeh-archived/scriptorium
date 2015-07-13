#!/bin/sh
cd ./objectscript
git reset --hard HEAD
git pull
mkdir build
cd build
cmake -DBUILD_SOCI=ON -DCMAKE_INSTALL_PREFIX=/ ..
make
make install
service os-fcgi restart
cd ../..
sudo -u os -i "/home/os/upgrade-os.org.sh"
