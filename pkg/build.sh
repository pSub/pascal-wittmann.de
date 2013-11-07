#!/bin/sh
cp ../dist/build/homepage/homepage . 
rm -rf lib/
ldd-copy homepage lib/
patchelf --set-rpath lib/ homepage
patchelf --set-interpreter lib/ld* homepage
apack homepage.tar.bz2 homepage lib/
