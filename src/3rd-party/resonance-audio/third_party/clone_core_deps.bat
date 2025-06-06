@echo off
setlocal enabledelayedexpansion

rem Change working directory to script folder
cd /d "%~dp0"

echo === Cloning Resonance Audio core dependencies ===

call :GitCloneIfNotExist eigen https://gitlab.com/libeigen/eigen.git master
call :GitCloneIfNotExist pffft https://bitbucket.org/jpommier/pffft.git master

echo === Done cloning dependencies ===
exit /b 0

:GitCloneIfNotExist
set FOLDER=%~1
set URL=%~2
set BRANCH=%~3
if not exist "%FOLDER%" (
    echo Cloning %FOLDER% from %URL% branch %BRANCH%...
    git clone -b "%BRANCH%" "%URL%" "%FOLDER%"
    if errorlevel 1 (
        echo [ERROR] Failed to clone %FOLDER% from %URL%
        exit /b 1
    )
) else (
    echo %FOLDER% already exists, skipping...
)
exit /b 0
