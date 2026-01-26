@echo off

:: Set release
if "%~1"=="" (
    echo Usage: %~0 release
    exit /b 1
)
set release=%~1

:: Pack developer build
mkdir build\x64\Server-Windows\temp-developer
copy fsgame.ltx build\x64\Server-Windows\temp-developer
copy .xrignore build\x64\Server-Windows\temp-developer
xcopy patch build\x64\Server-Windows\temp-developer\gamedata /e /i
xcopy build\x64\Server-Windows\bin\RelWithDebInfo build\x64\Server-Windows\temp-developer\bin /e /i
pushd build\x64\Server-Windows\temp-developer
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-develop.zip" ^
    "bin" ^
    -ir!"gamedata" ^
    -ir!"fsgame.ltx" ^
    -ir!".xrignore" ^
    -xr!"*.pdb"
popd
move build\x64\Server-Windows\temp-developer\ixray-1.6-r%release%-engine-x64-develop.zip ^
    publish\ixray-1.6-r%release%-engine-x64-develop.zip

:: Pack game build
mkdir build\x64\Server-Windows\temp-game
copy fsgame.ltx build\x64\Server-Windows\temp-game
copy .xrignore build\x64\Server-Windows\temp-game
xcopy patch build\x64\Server-Windows\temp-game\gamedata /e /i
xcopy build\x64\Server-Windows\bin\Release build\x64\Server-Windows\temp-game\bin /e /i
pushd build\x64\Server-Windows\temp-game
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-game.zip" ^
    "bin" ^
    -ir!"gamedata" ^
    -ir!"fsgame.ltx" ^
    -ir!".xrignore" ^
    -xr!"*.pdb"
popd
move build\x64\Server-Windows\temp-game\ixray-1.6-r%release%-engine-x64-game.zip ^
    publish\ixray-1.6-r%release%-engine-x64-game.zip

:: Pack editors build
mkdir build\x64\Editors-Windows\temp-editor
copy fs*.ltx build\x64\Editors-Windows\temp-editor
copy .xrignore build\x64\Editors-Windows\temp-editor
copy ActorEditorLevel.cform build\x64\Editors-Windows\temp-editor
xcopy patch build\x64\Editors-Windows\temp-editor\gamedata /e /i
xcopy rawdata build\x64\Editors-Windows\temp-editor\rawdata /e /i
xcopy build\x64\Editors-Windows\bin\RelWithDebInfo build\x64\Editors-Windows\temp-editor\bin /e /i
pushd build\x64\Editors-Windows\temp-editor
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-editors-x64.zip" ^
    "bin" ^
    -ir!"gamedata" ^
    -ir!"rawdata" ^
    -ir!"fs*.ltx" ^
    -ir!".xrignore" ^
    -ir!"ActorEditorLevel.cform" ^
    -xr!"*.pdb"
popd
move build\x64\Editors-Windows\temp-editor\ixray-1.6-r%release%-editors-x64.zip ^
    publish\ixray-1.6-r%release%-editors-x64.zip
