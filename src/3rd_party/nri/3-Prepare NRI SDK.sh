#!/bin/bash

NRI_DIR=.

rm -rf "_NRI_SDK"

mkdir -p "_NRI_SDK/Include"
mkdir -p "_NRI_SDK/Lib/Debug"
mkdir -p "_NRI_SDK/Lib/Release"

cd "_NRI_SDK"

cp -r ../$NRI_DIR/Include/ "Include"
cp -H ../_Build/Debug/libNRI.so "Lib/Debug"
cp -H ../_Build/Release/libNRI.so "Lib/Release"
cp ../$NRI_DIR/LICENSE.txt "."

cd ..
