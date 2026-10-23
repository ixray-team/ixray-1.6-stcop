@echo off
setlocal DisableDelayedExpansion
pushd "%~dp0" || exit /b 1
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0Prepare_GameFolder.ps1" -GameRoot "%CD%"
if errorlevel 1 (
    echo Startup configuration could not be prepared. See the error above.
    pause
    popd
    exit /b 1
)
set "GRASS_TINT_ENGINE=bin\xrEngine.exe"
if not exist "%GRASS_TINT_ENGINE%" set "GRASS_TINT_ENGINE=bin\Release\xrEngine.exe"
"%GRASS_TINT_ENGINE%" -r4 -fsltx fsgame_grass_tint.ltx %*
set "GAME_RESULT=%ERRORLEVEL%"
popd
exit /b %GAME_RESULT%
