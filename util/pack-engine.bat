@echo off

:: Set release
if "%~1"=="" (
    echo Usage: %~0 release
    exit /b 1
)
set release=%~1

:: Pack binaries
pushd build\x64\Engine-Windows
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-debug-bin.zip" ^
    "bin\Debug" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-release-bin.zip" ^
    "bin\Release" ^
    -xr!"*.pdb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-mixed-bin.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.pdb"
popd
move build\x64\Engine-Windows\ixray-1.6-r%release%-engine-x64-debug-bin.zip ^
    publish\ixray-1.6-r%release%-engine-x64-debug-bin.zip
move build\x64\Engine-Windows\ixray-1.6-r%release%-engine-x64-mixed-bin.zip ^
    publish\ixray-1.6-r%release%-engine-x64-mixed-bin.zip
move build\x64\Engine-Windows\ixray-1.6-r%release%-engine-x64-release-bin.zip ^
    publish\ixray-1.6-r%release%-engine-x64-release-bin.zip

:: Pack libraries
pushd build\x64\Engine-Windows
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-debug-lib.zip" ^
    "lib\Debug" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-release-lib.zip" ^
    "lib\Release" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-mixed-lib.zip" ^
    "lib\RelWithDebInfo" ^
    -xr!"*.pdb" ^
    -xr!"*.idb"
popd
move build\x64\Engine-Windows\ixray-1.6-r%release%-engine-x64-debug-lib.zip ^
    publish\ixray-1.6-r%release%-engine-x64-debug-lib.zip
move build\x64\Engine-Windows\ixray-1.6-r%release%-engine-x64-release-lib.zip ^
    publish\ixray-1.6-r%release%-engine-x64-release-lib.zip
move build\x64\Engine-Windows\ixray-1.6-r%release%-engine-x64-mixed-lib.zip ^
    publish\ixray-1.6-r%release%-engine-x64-mixed-lib.zip

:: Pack engine symbols
pushd build\x64\Engine-Windows
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-debug-pdb.zip" ^
    "bin\Debug" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\Debug" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
"%ProgramFiles%"\7-Zip\7z.exe a -tzip "ixray-1.6-r%release%-engine-x64-mixed-pdb.zip" ^
    "bin\RelWithDebInfo" ^
    -xr!"*.exe" ^
    -xr!"*.dll" ^
    -ir!"lib\RelWithDebInfo" ^
    -xr!"*.lib" ^
    -xr!"*.exp"
popd
move build\x64\Engine-Windows\ixray-1.6-r%release%-engine-x64-debug-pdb.zip ^
    publish\ixray-1.6-r%release%-engine-x64-debug-pdb.zip
move build\x64\Engine-Windows\ixray-1.6-r%release%-engine-x64-mixed-pdb.zip ^
    publish\ixray-1.6-r%release%-engine-x64-mixed-pdb.zip
