#!/bin/sh
rm -rf archive
mkdir -p archive
cd archive
cp ../../dist/build/homepage/homepage .
ldd-copy homepage lib/
patchelf --set-rpath lib/ homepage
patchelf --set-interpreter lib/ld* homepage
cd ..
apack -f homepage.tar.bz2 archive
rm -rf archive
