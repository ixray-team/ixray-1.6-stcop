@echo off

:: Set release
if "%~1"=="" (
    echo Usage: %~0 release
    exit /b 1
)
set release=%~1

:: Pack dependencies
pushd build\x64\Engine-Windows
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-dependencies.zip" ^
    "dep"
popd
move build\x64\Engine-Windows\ixray-1.6-r%release%-dependencies.zip ^
    publish\ixray-1.6-r%release%-dependencies.zip
