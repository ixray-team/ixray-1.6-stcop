@echo off

:: Set release
if "%~1"=="" (
    echo Usage: %~0 release
    exit /b 1
)
set release=%~1

:: Pack developer build
mkdir build\x64\Engine-Windows\temp-developer
copy fsgame.ltx build\x64\Engine-Windows\temp-developer
copy .xrignore build\x64\Engine-Windows\temp-developer
xcopy patches\xpatch_03.db build\x64\Engine-Windows\temp-developer\patches\ /e
xcopy patch build\x64\Engine-Windows\temp-developer\gamedata /e /i
xcopy build\x64\Engine-Windows\bin\RelWithDebInfo build\x64\Engine-Windows\temp-developer\bin /e /i
pushd build\x64\Engine-Windows\temp-developer
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-develop.zip" ^
    "bin" ^
    -ir!"gamedata" ^
    -ir!"fsgame.ltx" ^
    -ir!".xrignore" ^
    -ir!"patches\xpatch_03.db" ^
    -xr!"*.pdb"
popd
move build\x64\Engine-Windows\temp-developer\ixray-1.6-r%release%-engine-x64-develop.zip ^
    publish\ixray-1.6-r%release%-engine-x64-develop.zip

:: Pack game build
mkdir build\x64\Engine-Windows\temp-game
copy fsgame.ltx build\x64\Engine-Windows\temp-game
copy .xrignore build\x64\Engine-Windows\temp-game
xcopy patches\xpatch_03.db build\x64\Engine-Windows\temp-game\patches\ /e
xcopy patch build\x64\Engine-Windows\temp-game\gamedata /e /i
xcopy build\x64\Engine-Windows\bin\Release build\x64\Engine-Windows\temp-game\bin /e /i
pushd build\x64\Engine-Windows\temp-game
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-game.zip" ^
    "bin" ^
    -ir!"gamedata" ^
    -ir!"fsgame.ltx" ^
    -ir!".xrignore" ^
    -ir!"patches\xpatch_03.db" ^
    -xr!"*.pdb"
popd
move build\x64\Engine-Windows\temp-game\ixray-1.6-r%release%-engine-x64-game.zip ^
    publish\ixray-1.6-r%release%-engine-x64-game.zip
