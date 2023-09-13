#!/bin/sh

git submodule update --init --recursive

chmod +x "External/Packman/packman.sh"
chmod +x "2-Build.sh"
chmod +x "3-Prepare NRI SDK.sh"
chmod +x "4-Clean.sh"

mkdir -p "_Compiler"

cd "_Compiler"
cmake ..
cd ..
