@echo off

:: Set compressor
if "%~1"=="" (
    echo Usage: %~0 compressor
    exit /b 1
)
set compressor=%~1

:: Get assets
if not exist gamedata_origin (
    git clone ./.git ./temp
    cd temp
    git checkout 2045fd565ee903b1ea1a386afd8da73466e65d1d
    cd ..
    move temp/gamedata gamedata_origin
)

:: Generate patch
%compressor% -diff gamedata gamedata_origin -out patch
