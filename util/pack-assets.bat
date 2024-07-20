@echo off

:: Set release
if "%~1"=="" (
    echo Usage: %~0 release
    exit /b 1
)
set release=%~1

:: Pack assets
move gamedata gamedata_main
move patch gamedata
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "publish\ixray-1.6-r%release%-assets.zip" ^
    "gamedata" ^
    -ir!"fsgame.ltx" ^
    -ir!".xrignore" ^
    -ir!"patches\xpatch_03.db" ^
    -xr!"temp\fsgame.ltx"
move gamedata patch
move gamedata_main gamedata
