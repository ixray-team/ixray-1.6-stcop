@echo off

:: Set compressor
if "%~1"=="" (
    echo Usage: %~0 compressor
    exit /b 1
)
set compressor=%~1

git clone https://github.com/ixray-team/ixray-multiplatform-assets ./temp
move temp/gamedata_cop gamedata_origin
move temp/gamedata_cs gamedata_cs_origin
move temp/gamedata_soc gamedata_soc_origin
::move temp/gamedata_coc gamedata_coc_origin

:: Generate patch
%compressor% -diff gamedata gamedata_origin -out patch
%compressor% -diff gamedata_cs gamedata_cs_origin -out patch_cs
%compressor% -diff gamedata_soc gamedata_soc_origin -out patch_soc
::%compressor% -diff gamedata_coc gamedata_coc_origin -out patch_coc

:: Cleanup
echo Cleaning up...
rd /s /q temp 2>nul
rd /s /q gamedata_origin 2>nul
rd /s /q gamedata_cs_origin 2>nul
rd /s /q gamedata_soc_origin 2>nul
::rd /s /q gamedata_coc_origin 2>nul

echo Cache cleared successfully!
