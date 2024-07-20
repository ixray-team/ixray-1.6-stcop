@echo off

:: Set release
if "%~1"=="" (
    echo Usage: %~0 release
    exit /b 1
)
set release=%~1

:: Build all
call util\build-compressor.bat
call util\build-engine.bat
call util\build-server.bat
call util\build-utilities.bat

:: Prepare packing
call util\generate-patch.bat
call util\clear-utilities.bat

:: Pack all
call util\pack-assets.bat %release%
call util\pack-dependencies.bat %release%
call util\pack-packages.bat %release%
call util\pack-engine.bat %release%
call util\pack-server.bat %release%
call util\pack-utilities.bat %release%
call util\pack-builds.bat
